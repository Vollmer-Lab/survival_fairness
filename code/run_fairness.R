library(mlr3proba)
library(dplyr)

run_all = function(task, N_rep = 2, lrn = "surv.coxph", resamp = rsmp("holdout")) {
  run_one = function(task = tsk("whas"), p_disadv = 0.5, lrn = "surv.coxph",
                     resamp = rsmp("holdout")) {
    lrn = lrn(lrn)
    d = as.data.frame(task$data())

    split = partition(task, ratio = 0.5)
    adv = d[split$train, ]
    disadv = d[split$test, ]

    dadv = rbinom(nrow(disadv), 1, p_disadv) == 1
    sumdadv = sum(dadv)
    for (which in colnames(disadv)) {
      col = disadv[[which]]
      if (is.factor(col)) {
        disadv[dadv, which] <- sample(levels(col), sumdadv, TRUE)
      } else {
        disadv[dadv, which] <- round(runif(sumdadv, min(col), max(col)))
      }
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

    round(apply(
      rbind(score_adv, score_disadv), 2,
      function(x) abs(x[1] - x[2])
    ), 3)
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
    msr("surv.cindex", id = "C_U", weight_meth = "G2")
  )

  props = seq.int(0, 1, 0.1)
  ret = matrix(NA, length(measures), length(props))

  for (i in seq_along(props)) {
    x = replicate(N_rep, run_one(
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

# tasks <- list(tsk("rats"), tsk("whas"))

files <- dir(here::here("code/data"), pattern = "\\.rds$", full.names = TRUE)
names <- fs::path_ext_remove(fs::path_file(files))
tasks <- mlr3misc::named_list(names)

for (i in seq_along(files)) {
  data = readRDS(files[i])
  
  task = as_task_surv(data, target = "time", event = "status", id = names[i])
  #task$set_col_roles("status", add_to = "stratum")
  
  tasks[[i]] = task
  rm(data, task)
}

res <- lapply(tasks, run_all, N_rep = 2)
write.csv(do.call(rbind, res), "survival_fairness.csv")
