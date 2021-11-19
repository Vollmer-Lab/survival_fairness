## 1. Lognormal data generating process for one protected attribute white/BAME
##    and one treatment variable
##  a) NoTrt mean = 10, Trt mean = 3. White sd = 3, BAME sd = 5
##  b) WhiteNoTrt (10, 3); WhiteTrt (9, 3); BAMENoTrt (10, 3); BAMETrt (7, 3)
##  c) Consider how the DGP changes over time, i.e. which group fails early and which fails late
library(dplyr)
library(ggplot2)
library(distr6)
library(patchwork)
library(mlr3proba)
library(mlr3extralearners)

DGP <- \ (WNT_mean, WT_mean, MNT_mean, MT_mean, WNT_sd, WT_sd, MNT_sd, MT_sd) {
  dstrs("Lognormal",
    pars = data.frame(mean = c(WNT_mean, WT_mean, MNT_mean, MT_mean),
                      sd = c(WNT_sd, WT_sd, MNT_sd, MT_sd))
  )
}

generator <- \ (DGP, p_white, p_trt) {

  function(n, seed) {
    set.seed(seed)

    white <- rbinom(n, 1, p_white)
    trt <- rbinom(n, 1, p_trt)

    data.frame(white, trt, event = 1) %>%
      mutate(
        time = case_when(
          white & !trt ~ DGP$rand(1)[[1]],
          white & trt ~ DGP$rand(1)[[2]],
          !white & !trt ~ DGP$rand(1)[[3]],
          !white & trt ~ DGP$rand(1)[[4]]
        ),
        white = factor(white),
        trt = factor(trt)
      ) %>%
      as_task_surv()
  }
}

LearnerDGP <- R6::R6Class(
  "LearnerDGP",
  inherit = mlr3proba::LearnerSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.cheat",
        predict_types = c("distr", "crank"),
        feature_types = mlr_reflections$task_feature_types,
        param_set = paradox::ps(DGP = paradox::p_uty())
      )
    }
  ),
  private = list(
    .train = function(task) {
      list()
    },
    .predict = function(task) {
      DGP <- self$param_set$values$DGP
      distr <- task$data() %>%
        mutate(distr = case_when(
        trt == 0 & white == 1 ~ list(DGP[1]),
        trt == 1 & white == 1 ~ list(DGP[2]),
        trt == 0 & white == 0 ~ list(DGP[3]),
        trt == 1 & white == 0 ~ list(DGP[4])
      )) %>% select(distr)
      distr <- VectorDistribution$new(distr$distr)
      list(distr = distr, crank = -distr$mean())
    }
  )
)
utils::getFromNamespace("mlr_learners", ns = "mlr3")$add("surv.dgp", LearnerDGP)

plot_task <- \ (task) {
  dat <- task$data() %>%
    mutate(white = if_else(white == 0, "min", "whi"),
          trt = if_else(trt == 1, "trt", "no_trt"),
          int = interaction(white, trt))
  ggplot(dat, aes(x = time)) +
    geom_density() +
  ggplot(dat, aes(x = time, group = white, fill = white)) +
    geom_density(alpha = 0.4) +
  ggplot(dat, aes(x = time, group = trt, fill = trt)) +
    geom_density(alpha = 0.4) +
  ggplot(dat, aes(x = time, group = int, fill = int)) +
    geom_density(alpha = 0.4) &
  theme_classic() &
  theme(legend.title = element_blank())
}

predict_eval <- \ (learner, test, meas = msr("surv.graf")) {
  which_whi <- which(test$data()$white == 1)
  which_min <- which(test$data()$white == 0)
  white <- learner$predict(test, which_whi)$score(meas)
  minority <- learner$predict(test, which_min)$score(meas)
  overall <- learner$predict(test)$score(meas)
  data.frame(white = white, minority = minority, overall = overall)
}