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
    maj_mean = seq(0.05, 5, length.out = 5),
    mnt_mean = seq(0.05, 5, length.out = 5),
    maj_sd = seq(0, 5, length.out = 5),
    mnt_sd = seq(0, 5, length.out = 5),
    p_white = c(0.1, 0.3, 0.5),
    p_trt = c(0.1, 0.3, 0.5),
    n_train = 200,
    n_test = 100,
    seed = 20211201
)

eval_dgpw(grd[1,])
eval_dgpw(grd[2,])
eval_dgpw(grd[3,])
eval_dgpw(grd[4,])

