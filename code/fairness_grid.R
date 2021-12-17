source("code/helpers.R")

# Short eval:
d <- DGP(WNT_mean = 3, WT_mean = 3, MNT_mean = 3, MT_mean = 3, WNT_sd = 3, WT_sd   = 3, MNT_sd = 3, MT_sd   = 3)
generate <- generator(DGP = d, p_white = 0.5, p_trt = 0.5)
train    <- generate(n = 200, seed = 20211108)
test     <- generate(n = 100, seed = 20211110)
eval_groupwise(lrn("surv.coxph")$train(train), test)


# Define grid to evaluate
grd = expand.grid(
    trt_effect = seq(0, 2, length.out = 3),
    maj_mean = seq(0.05, 10, length.out = 3),
    mnt_mean = seq(0.05, 10, length.out = 3),
    maj_sd = seq(0.01, 10, length.out = 3),
    mnt_sd = seq(0.01, 10, length.out = 3),
    p_white = c(0.5, 0.9),
    p_trt = 0.5,
    n_train = 200,
    n_test = 100
)
grd$seed = seq(nrow(grd)) * pi

#res = vapply(seq(nrow(grd)), function(i) as.matrix(eval_dgpw(grd[i, ])), matrix(NA_character_, 2, 4))
res = lapply(
    seq(5),
    function(i) {
      out = reshape2::melt(eval_dgpw(grd[i, ]))
      ch = subset(out, metric == "surv.cindex")
      ibs = subset(out, metric == "surv.graf")
      ch_diff = as.numeric(abs(subset(ch, variable == "group_1", select = "value") -
        subset(ch, variable == "group_0", select = "value")))
      ibs_diff = as.numeric(abs(subset(ibs, variable == "group_1", select = "value") -
        subset(ibs, variable == "group_0", select = "value")))
      data.frame(comb = i, Ch = ch_diff, IBS = ibs_diff)
    }
)
res = do.call(rbind, res)
res
