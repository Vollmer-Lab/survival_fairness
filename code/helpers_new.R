library(data.table)
library(dplyr)
library(ggplot2)
library(distr6)
library(patchwork)
library(mlr3misc)
library(mlr3proba)
library(mlr3extralearners)
library(mlr3fairness)

stopifnot(packageVersion("mlr3fairness") >= "0.2.0")
stopifnot(packageVersion("mlr3proba") >= "0.4.2.9000")


DGP_adv <- function(n, trt) {
  if (trt) rexp(n, 1 / 40) else rexp(n, 1 / 20)
}

DGP_disadv <- function(n, trt, T, sd = 1) {
  t <- DGP_adv(n, trt)
  which <- t >= T
  t[which] <- t[which] + abs(rnorm(sum(which), 0, sd))
  t
}

set_pta <- function(task) {
  task$col_roles$pta <- "adv"
  task
}

generator <- function (p_adv, p_trt, T, sd) {
  function(n, seed, id) {
    set.seed(seed)

    adv <- rbinom(n, 1, p_adv)
    trt <- rbinom(n, 1, p_trt)

    data.frame(adv, trt, event = 1) %>%
      mutate(
        time = case_when(
          adv & !trt ~ DGP_adv(n, FALSE),
          adv & trt ~ DGP_adv(n, TRUE),
          !adv & !trt ~ DGP_disadv(n, FALSE, T, sd),
          !adv & trt ~ DGP_disadv(n, TRUE, T, sd)
        ),
        adv = factor(adv),
        trt = factor(trt)
      ) %>%
      as_task_surv(id = id) %>%
      set_pta()
  }
}

generate <- function(p_adv, sd, seed, T = Inf, p_trt = 0.5, n_train = 200,
                      n_test = 100) {
  dgp <- generator(p_adv, p_trt, T, sd)
  list(
    train = dgp(n_train, seed, id = "train"),
    test = dgp(n_test, seed + n_train + n_test + 1, id = "test")
  )
}


eval_groupwise <- function(tasks, learner = lrn("surv.coxph")) {
  learner$train(tasks$train)
  test = tasks$test
  prds = learner$predict(test)
  map_dtr(msrs(c("surv.cindex", "surv.graf")), function(m) {
    ms = c(groupwise_metrics(m, test), m)
    scores = prds$score(ms, task = test, train_set = seq_len(test$nrow))
    data.table("diff" = abs(scores[1] - scores[2]), "metric" = m$id)
  })
}

p_adv = seq.int(0.5, 0.9, length.out = 5)
sd = seq.int(0, 10, length.out = 5)
T = c(0, 10, 20)
config <- expand.grid(p_adv = p_adv, sd = sd, T = T)
config$seed <- seq(nrow(config)) * 1e4 * pi

run_exp <- function(config) {
  mat <- matrix(NA, nrow(config), 5, FALSE,
                list(NULL, c("p_adv", "sd", "T", "C", "IBS")))
  for (i in seq(nrow(config))) {
    res <- eval_groupwise(generate(config[i, 1], config[i, 2], config[i, 3],
                                    config[i, 4]))
    mat[i, 1:3] <- as.matrix(config[i, 1:3])
    mat[i, 4:5] <- as.matrix(res[, 1])
  }

  data.frame(mat)
}

res_exp <- run_exp(config)
head(res_exp)
aes <- aes(x = p_adv, y = sd, size = IBS, color = IBS, fill = IBS)
p0 <- ggplot(filter(res_exp, T == 0), aes) +
  geom_point() + labs(title = "T = 0")
p10 <- ggplot(filter(res_exp, T == 10), aes) +
  geom_point() + labs(title = "T = 10")
p20 <- ggplot(filter(res_exp, T == 20), aes) +
  geom_point() + labs(title = "T = 20")

p0 + p10 + p20 + plot_layout(2, 2) &
  theme_bw() & scale_color_gradient() & scale_fill_gradient() & guides(size = "none")
