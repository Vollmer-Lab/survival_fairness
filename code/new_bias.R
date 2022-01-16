library(survival)
library(mlr3proba)
library(mlr3fairness)
library(dplyr)
library(ggplot2)

add_bias <- function(df, T, method, adv_col, bias_col, status_col) {
  df %>%
    dplyr::mutate(
      biased = case_when(
        time >= T & get(adv_col) == 0 & method == "group_permute" ~
          sample(get(bias_col)[df[adv_col] == 0], nrow(.), TRUE),
        time >= T & get(adv_col) == 0 & method == "full_permute" ~
          sample(get(bias_col)),
        TRUE ~ get(bias_col)
      ),
      adv = get(adv_col),
      event = get(status_col)
    ) %>%
    dplyr::select(-any_of(c(bias_col, adv_col, status_col)))
}

group_eval <- function(task) {
  m = lapply(msrs(c("surv.graf", "surv.cindex")),
              groupwise_metrics, task = task)
  res = resample(task, lrn("surv.coxph"), rsmp("cv", folds = 3))
  agg = res$aggregate(unlist(m))
  c("graf" = abs(agg[1] - agg[2]), "cindex" = (agg[3] - agg[4]))
}

set_pta <- function(task) {
  task$col_roles$pta <- "adv"
  task
}

exp_one <- function(data, T, method, adv_col, bias_col, status_col) {
  task <- data %>%
    add_bias(T, method, adv_col, bias_col, status_col) %>%
    as_task_surv() %>%
    set_pta()

  group_eval(task)
}

exp_full <- function(data, adv_col, bias_col, status_col) {
  method <- c("none", "group_permute", "full_permute")
  cutoff <- seq.int(min(data$time), max(data$time), length.out = 4)
  config <- expand.grid(method = method, T = cutoff)
  config$seed <- seq(nrow(config)) * 1e4 * pi
  mat <- data.frame(matrix(
    NA, nrow(config), 4, FALSE,
    list(NULL, c("method", "T", "IBS", "C"))
  ))
  for (i in seq(nrow(config))) {
    set.seed(config[i, "seed"])
    res <- exp_one(
      data, config[i, "T"], config[i, "method"], adv_col,
      bias_col, status_col
    )
    mat[i, 1:2] <- as.matrix(config[i, 1:2])
    mat[i, 3:4] <- as.matrix(res)
  }

  mat
}

res_vet <- survival::veteran %>%
  mutate(prior = as.integer(prior == 10)) %>%
  exp_full("prior", "karno", "status")

ggplot(
  data.frame(res_vet),
  aes(x = method, y = T, size = IBS, color = IBS, fill = IBS)) +
  geom_point()
