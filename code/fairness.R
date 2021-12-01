library("mlr3misc")
source("code/helpers.R")

d <- DGP(WNT_mean = 3, WT_mean = 3, MNT_mean = 3, MT_mean = 3,
  WNT_sd   = 3, WT_sd   = 3, MNT_sd   = 3, MT_sd   = 3)

generate <- generator(DGP = d, p_white = 0.5, p_trt = 0.5)
train    <- generate(n = 200, seed = 20211108)
test     <- generate(n = 100, seed = 20211110)
evals    <- rbind(
  CPH = predict_eval_2(lrn("surv.coxph")$train(train), test),
  RSF = predict_eval_2(lrn("surv.rfsrc")$train(train), test),
  DGP = predict_eval_2(lrn("surv.dgp", DGP = d)$train(train), test)
)


# Wasserstein Distance for d = 1
# We approximate the integral via sums 
# integrate DGP
wassersteind1 = function(lrn1, lrn2, p = 1) {
  # quantile(z=1) is NaN. So we use .99999 instead.
  xx = map(seq(from = 0, to = .99999, length.out = 20), function(x, lrn1, lrn2, p) {
    q1 = lrn1$predict(test)$distr$quantile(x)
    q2 = lrn2$predict(test)$distr$quantile(x)
    abs(q1 - q2)^p
  }, lrn1 = lrn1, lrn2 = lrn2, p = p)
  apply(do.call("rbind", xx), 2, mean)^(1/p)
}
lrn1 = lrn("surv.dgp", DGP = d)$train(train)
lrn2 = lrn("surv.coxph")$train(train)

wassersteind1(lrn1,lrn2)



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





