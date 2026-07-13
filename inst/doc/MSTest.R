## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)
library(MSTest)
seed <- 1234

## ----simulate-----------------------------------------------------------------
set.seed(seed)
mdl <- list(n     = 200,
            mu    = c(0, 8),
            sigma = c(1, 1),
            phi   = c(0.5),
            k     = 2,
            P     = rbind(c(0.90, 0.10),
                          c(0.10, 0.90)))

sim <- simuMSAR(mdl)
plot(sim)

## ----estimate-----------------------------------------------------------------
set.seed(seed)
mdl_est <- MSARmdl(sim$y, p = 1, k = 2,
                   control = list(msmu = TRUE, msvar = TRUE, use_diff_init = 5))
summary(mdl_est)

## ----lmclrt-------------------------------------------------------------------
set.seed(seed)
lmc <- LMCLRTest(sim$y, p = 1, k0 = 1, k1 = 2,
                 control = list(N = 19,
                                mdl_h0_control = list(const = TRUE, getSE = FALSE),
                                mdl_h1_control = list(msmu = TRUE, msvar = TRUE,
                                                      getSE = FALSE, use_diff_init = 3)))
summary(lmc)

## ----mmclrt, eval = FALSE-----------------------------------------------------
# set.seed(seed)
# y0 <- simuNorm(list(n = 150, q = 1, mu = 0, sigma = as.matrix(1)))$y
# mmc <- MMCLRTest(y0, p = 0, k0 = 1, k1 = 2,
#                  control = list(N = 19, eps = 0.1, CI_union = FALSE,
#                                 type = "GenSA", threshold_stop = 0.05,
#                                 maxit = 10, silence = TRUE,
#                                 mdl_h0_control = list(getSE = FALSE),
#                                 mdl_h1_control = list(msmu = TRUE, msvar = TRUE,
#                                                       getSE = FALSE, use_diff_init = 1)))
# summary(mmc)

## ----dlmc---------------------------------------------------------------------
set.seed(seed)
mom <- DLMCTest(sim$y, p = 1, control = list(N = 99, simdist_N = 10000))
summary(mom)

## ----chp----------------------------------------------------------------------
set.seed(seed)
chp <- CHPTest(sim$y, p = 1, control = list(N = 99, rho_b = 0.7))
summary(chp)

