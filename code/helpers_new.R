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

## Hazard:
##  h(t) = if (TRT) 20X^2 else 10X^2 --> 10X^2(TRT + 1)

gen_unbiased <- function(n, p_trt, p_adv) {
  X <- runif(n, 1, 3)
  trt <- rbinom(n, 1, p_trt)
  adv <- factor(rbinom(n, 1, p_adv))

  time <- rexp(n, 1 / 10) * X^2 * (trt + 1)
  data.frame(X = X, trt = factor(trt), time = time, adv = adv, event = 1)
}

add_bias <- function(DGP, T, sd) {
  DGP %>%
    dplyr::mutate(
      X = case_when(
        time >= T & adv == 0 ~ X + rnorm(nrow(.), 0, sd),
        TRUE ~ X
      )
    )
}

set_pta <- function(task) {
  task$col_roles$pta <- "adv"
  task
}

generator <- function (p_adv, p_trt, T, sd) {
  function(n, seed, id) {
    set.seed(seed)

    gen_unbiased(n = n, p_trt = p_trt, p_adv = p_adv) %>%
      add_bias(T, sd) %>%
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

p_adv = c(0.5, 0.7, 0.9)
sd = 0:3
T = c(0, 50, 100, 300)
config <- expand.grid(p_adv = p_adv, sd = sd, T = T)
config$seed <- seq(nrow(config)) * 1e4 * pi

run_exp <- function(config) {
  mat <- matrix(NA, nrow(config), 5, FALSE,
                list(NULL, c("p_adv", "sd", "T", "C", "IBS")))
  for (i in seq(nrow(config))) {
    res <- eval_groupwise(generate(p_adv = config[i, "p_adv"],
        sd = config[i, "sd"], T = config[i, "T"],
                                    seed = config[i, "seed"]))
    mat[i, 1:3] <- as.matrix(config[i, 1:3])
    mat[i, 4:5] <- as.matrix(res[, 1])
  }

  data.frame(mat)
}

res_exp <- run_exp(config)
aes <- aes(x = p_adv, y = sd, size = IBS, color = IBS, fill = IBS)
p0 <- ggplot(filter(res_exp, T == 0), aes) +
  geom_point() + labs(title = "T = 0")
p50 <- ggplot(filter(res_exp, T == 50), aes) +
  geom_point() + labs(title = "T = 50")
p100 <- ggplot(filter(res_exp, T == 100), aes) +
  geom_point() + labs(title = "T = 100")
p300 <- ggplot(filter(res_exp, T == 300), aes) +
  geom_point() + labs(title = "T = 300")

p0 + p50 + p100 + p300 + plot_layout(2, 2) &
  theme_bw() & scale_color_gradient() & scale_fill_gradient() & guides(size = "none")
