library(survival)
library(mlr3proba)
library(mlr3fairness)
library(mlr3learners)
library(ggplot2)
library(dplyr)


## list of measures
grep("surv", mlr3::mlr_measures$keys(), value = TRUE)

measures = c(
  msrs(c(
    "surv.cindex", "surv.graf", "surv.calib_alpha",
    "surv.calib_beta", "surv.dcalib"
  )),
  msr("surv.graf", proper = TRUE, id = "graf_proper")
)
setScores = function(scores) {
  setNames(c(
    abs(scores[1] - scores[2]),
    abs(scores[3] - scores[4]),
    abs(scores[5] - scores[6]),
    abs(scores[7] - scores[8]),
    abs(scores[9] - scores[10]),
    abs(scores[11] - scores[12])
  ), c(
    "cindex", "graf", "calib_alpha", "calib_beta",
    "dcalib", "graf_proper"
  ))
}

runExp = function(task = "whas", p_disadv = 0.5, lrn = "surv.coxph",
                  resamp = rsmp("holdout"), event_col = "status") {

  task = tsk(task)
  d = task$data()
  d$pta = rbinom(task$nrow, 1, p_disadv)

  cutoffs = unique(round(quantile(task$times())))
  # cutoffs = 0 ## bias everyone
  out = matrix(0, length(cutoffs), length(measures))
  for (i in seq_along(cutoffs)) {
    t = cutoffs[[i]]
    dadv = d$pta == 1 & d$t >= t
    sumdadv = sum(dadv)
    for (which in setdiff(colnames(d), "pta")) {
      col = d[[which]]
      if (is.factor(col)) {
        d[dadv, which] <- sample(levels(col), sumdadv, TRUE)
      } else {
        d[dadv, which] <- round(runif(sumdadv, min(col), max(col)))
      }
    }

    task = as_task_surv(d, event = event_col)
    task$col_roles$pta <- "pta"
    m = lapply(measures, groupwise_metrics, task = task)

    score = setScores(resample(task, lrn(lrn), resamp)$aggregate(unlist(m)))

    out[i, ] = round(score, 3)
  }
  dimnames(out) <- list(cutoffs, c(
    "cindex", "graf", "calib_alpha", "calib_beta",
    "dcalib", "graf_proper"
  ))
  out
}

ret = array(NA, c(5, 6, 3))
props = c(0.1, 0.5, 0.9)
set.seed(340233490)
for (i in seq_along(props)) {
  x = replicate(5, runExp(p_disadv = props[[i]], resamp = rsmp("cv", folds = 3)))
  x[x == Inf] = NA
  ret[, , i] = round(apply(x, c(1, 2), mean, na.rm = TRUE), 3)
}

dimnames(ret) <- c(dimnames(x)[1:2], list(props))


reshape2::melt(ret) %>%
  dplyr::group_by(Var2) %>%
  dplyr::mutate(
    Measure = Var2,
    Time = Var1,
    Prop = Var3,
    value = value / sum(value)
  ) %>%
  dplyr::ungroup() %>%
    ggplot(aes(x = Time, y = Measure, size = value, color = value)) +
    facet_grid(cols = vars(Prop)) +
    geom_point() +
    theme_bw()
