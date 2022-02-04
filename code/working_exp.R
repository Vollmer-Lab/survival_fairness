library(survival)
library(mlr3proba)
library(mlr3fairness)
library(mlr3learners)

set.seed(20220208)

## list of measures
grep("surv", mlr3::mlr_measures$keys(), value = TRUE)


x = replicate(10, {
  measures = c(
    msrs(c("surv.cindex", "surv.graf", "surv.calib_alpha",
    "surv.calib_beta", "surv.dcalib")),
    msr("surv.graf", proper = TRUE, id = "graf_proper")
  )
  task = tsk("whas")
  task$col_roles$pta <- "sexF"
  # task before biasing
  m = lapply(measures,
    groupwise_metrics,
    task = task
  )
  score_before = resample(
    task,
    lrn("surv.coxph"),
    rsmp("cv", folds = 3)
  )$aggregate(unlist(m))
  score_before = setNames(c(
    abs(score_before[1] - score_before[2]),
    abs(score_before[3] - score_before[4]),
    abs(score_before[5] - score_before[6])
  ), c("cindex", "graf", "graf_proper"))

  # task after biasing
  d = task$data()
  dadv = d$sexF == 0
  sumdadv = sum(dadv)
  for (which in setdiff(colnames(d), "sexF")) {
    if (is.factor(d[[which]])) {
      d[dadv, which] <- sample(levels(d[[which]]), sumdadv, TRUE)
    } else {
      d[dadv, which] <- round(runif(sumdadv, min(d[[which]]), max(d[[which]])))
    }
  }

  task = as_task_surv(d, event = "status")
  task$col_roles$pta <- "sexF"
  # task before biasing
  m = lapply(measures,
    groupwise_metrics,
    task = task
  )
  score_after = resample(
    task,
    lrn("surv.coxph"),
    rsmp("cv", folds = 3)
  )$aggregate(unlist(m))
  score_after = setNames(c(
    abs(score_after[1] - score_after[2]),
    abs(score_after[3] - score_after[4]),
    abs(score_after[5] - score_after[6]),
    abs(score_after[7] - score_after[8]),
    abs(score_after[9] - score_after[10]),
    abs(score_after[11] - score_after[12])
  ), c("cindex", "graf", "calib_alpha", "calib_beta",
      "dcalib", "graf_proper"))

  rbind(before = score_before, after = score_after)
})

x = t(apply(round(x, 3), c(1, 2), function(.x) {
  mean(.x[.x != Inf])
}))

round(cbind(x, difference = abs(x[, 1] - x[, 2])), 3)
