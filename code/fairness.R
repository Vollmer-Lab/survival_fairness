source("code/helpers.R")

d        <- DGP(WNT_mean = 3, WT_mean = 3, MNT_mean = 2, MT_mean = 1,
                WNT_sd   = 3, WT_sd   = 5, MNT_sd   = 3, MT_sd   = 5)
generate <- generator(DGP = d, p_white = 0.5, p_trt = 0.5)
train    <- generate(n = 200, seed = 20211108)
test     <- generate(n = 100, seed = 20211110)
evals    <- rbind(
  CPH = predict_eval(lrn("surv.coxph")$train(train), test),
  RSF = predict_eval(lrn("surv.rfsrc")$train(train), test),
  DGP = predict_eval(lrn("surv.dgp", DGP = d)$train(train), test)
)

plot_task(train)
plot_task(test)
evals
