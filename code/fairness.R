source("code/helpers.R")

d        <- DGP(WNT_mean = 3, WT_mean = 3, MNT_mean = 3, MT_mean = 3,
                WNT_sd   = 3, WT_sd   = 3, MNT_sd   = 3, MT_sd   = 3)
generate <- generator(DGP = d, p_white = 0.5, p_trt = 0.5)
train    <- generate(n = 200, seed = 20211108)
test     <- generate(n = 100, seed = 20211110)
evals    <- rbind(
  CPH = predict_eval_2(lrn("surv.coxph")$train(train), test),
  RSF = predict_eval_2(lrn("surv.rfsrc")$train(train), test),
  DGP = predict_eval_2(lrn("surv.dgp", DGP = d)$train(train), test)
)

plot_task(train)
plot_task(test)
evals

source("code/helpers.R")
devtools::load_all("~/Documents/mlr-repos/mlr3fairness")

d <- DGP(WNT_mean = 3, WT_mean = 3.5, MNT_mean = 2, MT_mean = 2.5,
                WNT_sd   = 1, WT_sd   = 1, MNT_sd   = 1, MT_sd   = 1)
generate <- generator(DGP = d, p_white = 0.5, p_trt = 0.5)
train    <- generate(n = 200, seed = 20211108)
test     <- generate(n = 300, seed = 20211110)
predict_eval_2(lrn("surv.dgp", DGP = d)$train(train), test, meas = msr("surv.cindex"))
plot_task(train)
