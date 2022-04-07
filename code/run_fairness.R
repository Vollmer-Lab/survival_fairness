library(mlr3proba)
library(dplyr)
set.seed(24)

lgr::get_logger("mlr3")$set_threshold("warn")
lgr::get_logger("bbotk")$set_threshold("warn")

run_all = function(task, N_rep = 2, lrn = "surv.coxph", resamp = rsmp("holdout")) {
  run_one = function(task, p_disadv, lrn, resamp) {
    lrn = lrn(lrn)
    d = as.data.frame(task$data())

    split = partition(task, ratio = 0.5)
    adv = d[split$train, ]
    disadv = d[split$test, ]

    dadv = rbinom(nrow(disadv), 1, p_disadv) == 1
    # permute
    for (which in colnames(disadv)) {
      disadv[dadv, which] = sample(disadv[dadv, which])
    }

    score_adv = resample(
      as_task_surv(adv, event = "status"),
      lrn,
      resamp
    )$aggregate(measures)
    score_disadv = resample(
      as_task_surv(disadv, event = "status"),
      lrn,
      resamp
    )$aggregate(measures)

    round(
      apply(rbind(score_adv, score_disadv), 2, function(x) abs(x[1] - x[2])), 3
    )
  }

  measures = c(
    msr("surv.graf", id = "IGS"),
    msr("surv.graf", proper = TRUE, id = "IGS_proper"),
    msr("surv.intlogloss", id = "ILL"),
    msr("surv.intlogloss", proper = TRUE, id = "ILL_proper"),
    msr("surv.logloss", IPCW = FALSE, id = "NLL"),
    msr("surv.logloss", IPCW = TRUE, id = "SNL"),
    msr("surv.rcll", id = "RCLL"),
    msr("surv.cindex", id = "C_H"),
    msr("surv.cindex", id = "C_U", weight_meth = "G2"),
    msr("surv.calib_alpha", id = "calib_A"),
    msr("surv.dcalib", id = "calib_D")
  )

  props = seq.int(0, 1, 0.1)
  ret = matrix(NA, length(measures), length(props))

  for (i in seq_along(props)) {
    x = replicate(N_rep, run_one(
      task = task,
      p_disadv = props[[i]],
      resamp = resamp,
      lrn = lrn
    ))
    x[x == Inf] = NA
    ret[, i] = rowMeans(x, na.rm = TRUE)
  }

  dimnames(ret) <- c(dimnames(x)[1], list(props))
  reshape2::melt(ret) %>%
    dplyr::mutate(Measure = Var1, Prop = Var2, Score = value,
                  Task = task$id) %>%
    select(Measure, Prop, Score, Task)
}


files <- dir(here::here("code/data"), pattern = "\\.rds$", full.names = TRUE)
names <- fs::path_ext_remove(fs::path_file(files))

file_stats <- purrr::map2_dfr(files, names, ~{
  data = readRDS(.x)
  data.frame(file = .x, name = .y, nrow = nrow(data), ncol = ncol(data))
})

file_stats <- file_stats[which(!(file_stats$name %in% c("child", "hdfail"))), ]
file_stats <- file_stats[order(file_stats$nrow), ]

tasks <- mlr3misc::named_list(file_stats$name)

for (i in seq_along(file_stats$file)) {
  data = readRDS(file_stats$file[i])
  task = as_task_surv(data, target = "time", event = "status", id = file_stats$name[i])
  tasks[[i]] = task
  rm(data, task)
}

message("Running on ", length(tasks), " tasks")

# Single run for debugging
if (FALSE) {
  tictoc::tic()
  res <- run_all(tasks$flchain, N_rep = 1)
  tictoc::toc()
}

# Do it slowly and step by step with try() for debugging and at least some results
fs::dir_create(here::here("code/results"))
for (task in tasks) {
  res_path <- fs::path(here::here("code/results"), task$id, ext = "csv")
  if (fs::file_exists(res_path)) next

  message("Running on ", task$id)

  res <- try(run_all(task, N_rep = 10, resamp = rsmp("cv", folds = 3)))

  if (inherits(res, "try-error")) {
    message("Failed at ", task$id)
    next
  }
  # Writing results of each task separately for safety
  write.csv(res, file = res_path)
  message("Finished on ", task$id)
}

# Parallelization attempt failed, might be possible with more debugging
# future::plan("multisession")
# furrr::future_walk(tasks, ~{
#   res_path <- fs::path(here::here("code/results"), .x$id, ext = "csv")
#   if (fs::file_exists(res_path)) return(NULL)
# 
#   res <- run_all(.x, N_rep = 10, resamp = rsmp("cv", folds = 3))
# 
#   write.csv(res, file = res_path)
# }, .options = furrr::furrr_options(seed = TRUE))
# 
# # reassemble results to previously intended format
# res_full <- purrr::map_df(
#   fs::dir_ls(here::here("code/results"), glob = "*.csv"),
#   read.csv
# )

# reassemble results to previously intended format
res_full <- purrr::map_df(
  fs::dir_ls(here::here("code/results"), glob = "*.csv"),
  read.csv
)

setdiff(names(tasks), unique(res_full$Task))

write.csv(res_full, fs::path(here::here("code"), "survival_fairness.csv"))
