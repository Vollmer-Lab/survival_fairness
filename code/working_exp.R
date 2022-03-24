library(mlr3)
library(mlr3extralearners)
library(mlr3proba)
library(dplyr)
library(ggplot2)

measures = c(
  msr("surv.graf", id = "IGS"),
  msr("surv.graf", proper = TRUE, id = "IGS_proper"),
  msr("surv.intlogloss", id = "ILL"),
  msr("surv.intlogloss", proper = TRUE, id = "ILL_proper"),
  msr("surv.logloss", id = "ILL"),
  msr("surv.cindex", id = "cindex"),
  msr("surv.dcalib", id = "dcalib"),
  msr("surv.calib_alpha", id = "Hcalib")
)

runExp = function(task = "whas", p_disadv = 0.5, lrn = "surv.coxph",
                  resamp = rsmp("holdout")) {

  lrn = lrn(lrn)
  task = tsk(task)
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
    as_task_surv(adv, event = 'status'),
    lrn,
    resamp
  )$aggregate(measures)
  score_disadv = resample(
    as_task_surv(disadv, event = 'status'),
    lrn,
    resamp
  )$aggregate(measures)

    round(apply(
      rbind(score_adv, score_disadv), 2,
      function(x) abs(x[1] - x[2])
    ), 3)
}

props = seq.int(0, 1, 0.2)
ret = matrix(NA, length(measures), length(props))

for (i in seq_along(props)) {
  x = replicate(10, runExp(
    p_disadv = props[[i]],
    resamp = rsmp("holdout")
  ))
  x[x == Inf] = NA
  ret[, i] = rowMeans(x, na.rm = TRUE)
}

dimnames(ret) <- c(dimnames(x)[1], list(props))

library(patchwork)
reshape2::melt(t(ret)) %>%
  dplyr::filter(grepl("^I", Var2)) %>%
  ggplot(aes(x = Var1, y = value, group = Var2, color = Var2, fill = Var2)) +
  geom_line() +
  facet_wrap(vars(Var2)) +
  labs(x = "P(Censoring)", y = "Bias") +
  theme_bw() +
  theme(legend.position = "n") +

reshape2::melt(t(ret)) %>%
  dplyr::filter(!grepl("^I", Var2)) %>%
  ggplot(aes(x = Var1, y = value, group = Var2, color = Var2, fill = Var2)) +
  geom_line() +
  facet_wrap(vars(Var2), scales = "free") +
  labs(x = "P(Censoring)", y = "Bias") +
  theme_bw() +
  theme(legend.position = "n")
