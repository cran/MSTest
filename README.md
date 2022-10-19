# MSTest

This package implements hypothesis testing procedures that can be used to identify the number of regimes in a Markov-Switching model. It includes the Monte Carlo moment-based test of Dufour & Luger (2017), the parametric bootstrap test described in Qu & Zhuo (2021) and Kasahara & Shimotsu (2018), the Monte Carlo Likelihood ratio tests of Rodriguez Rondon & Dufour (2022), the optimal test for regime switching of Carrasco, Hu, & Ploberger (2014), and the likelihood ratio test described in Hansen (1992).

In addition to testing procedures, the package also includes functions that can be used to simulate: autoregressive, vector autoregressive, Markov switching autoregressive, and Markov switching vector autoregressive processes among others. Model estimation procedures are also available.

For a more detailed description of this package see Rodriguez Rondon & Dufour (2022).

## Installation

Currently, this package is only available through github. To install it you can use the following (requires 'devtools' package): 

```r
devtools::install_github("roga11/MSTest")
```

## Load Package

Once package has been installed it can be loaded. 
```{r}
library(MSTest)
```
## Data

The MSTest package includes 3 datasets that can be used as examples. The three datasets are:  
  
* hamilton84GNP: US GNP data from 1951Q2 - 1984Q4 used in Hamilton (1989) and Hansen (1992; 1996)
* chp10GNP: US GNP data from 1951Q2 - 2010Q4 used by Carrasco, Hu and Ploberger (2014)
* USGNP: US GNP data from 1947Q2 - 2022Q2 from FRED 
  
They can be loaded using the following code. 
```{r}
GNPdata <- USGNP # this can be hamilton82GNP, chp10GNP or USGNP
Y       <- as.matrix(GNPdata$GNP_logdiff)
date    <- as.Date(GNPdata$DATE)
plot(date,Y,xlab='Time',ylab='GNP - log difference',type = 'l')
```

You an also learn more about these datasets and their sources from their description in the help tab.
```{r}
?hamilton84GNP
```


## Model Estimation & Process simulation

This first example uses the US GNP growth data from 1951Q2-1984Q4 considered in Hamilton (1989). The data is made available as 'hamilton84GNP' through this package. In Hamilton (1989), the model is estimated with four autoregressive lags and only the mean is allowed to change between two (i.e., expansionary and recessionary) regimes and it is estimated by MLE and so we begin by estimating that model.  Estimation results can be compared with those found in Hamilton (1994) p. 698. Note however, that standard errors here were obtained using a different approximation method and hence these may differ slightly. 

```r
set.seed(123) # for initial values
y_gnp_gw_84 <- as.matrix(hamilton84GNP$GNP_logdiff)


# Set options for model estimation
control <- list(msmu   = TRUE, 
                msvar  = FALSE, 
                method = "MLE",
                use_diff_init = 5)

# Estimate model with p=4 and switch in mean only as in Hamilton (1989)
hamilton89_mdl <- MSARmdl(y_gnp_gw_84, p = 4, k = 2, control)
hamilton89_mdl

# plot smoothed probability of recessionary state
plot(hamilton89_mdl$St[,1], type = 'l')
```
 
This package also provides functions to simulate Markov switching processes among others. To do this, we use the 'simuMSAR' function to simulate a Markov switching process and then uses 'MSARmdl' to estimate the model. Estimated coefficients may be compared with the true parameters used to generate the data. A plot also shows the fit of the smoothed probabilities. 

```r
set.seed(123)
# Define DGP of MS AR process
mdl_ms2 <- list(n     = 500, 
                mu    = c(5,10),
                sigma = c(1,2),
                phi   = c(0.5),
                k     = 2,
                P     = rbind(c(0.90, 0.10),
                              c(0.10, 0.90)))

# Simulate process using simuMSAR() function
y_ms_simu <- simuMSAR(mdl_ms2)

# Set options for model estimation
control <- list(msmu   = TRUE, 
                msvar  = TRUE, 
                method = "EM",
                use_diff_init = 10)

# Estimate model
y_ms_mdl <- MSARmdl(y_ms_simu$y, p = 1, k = 2, control)
y_ms_mdl

plot(y_ms_mdl$St[,1], type = 'l')
lines(y_ms_simu$St, col = 'red', lty = 2)
```

This third example, the 'simuMSVAR' function to simulate a bivariate Markov switching vector autoregressive process and then uses 'MSVARmdl' to estimate the model. Estimated coefficients may be compared with the true parameters used to generate the data. A plot also shows the fit of the smoothed probabilities. 

```r
set.seed(1234)
# Define DGP of MS VAR process
mdl_msvar2 <- list(n     = 1000, 
                   p     = 1,
                   mu    = rbind(c(5,-2),
                                 c(10,2)),
                   sigma = list(rbind(c(5.0, 1.5),
                                      c(1.5, 1.0)),
                                rbind(c(7.0, 3.0),
                                      c(3.0, 2.0))),
                   phi   = rbind(c(0.50, 0.30),
                                 c(0.20, 0.70)),
                   k     = 2,
                   P     = rbind(c(0.90, 0.10),
                                 c(0.10, 0.90)))

# Simulate process using simuMSVAR() function
y_msvar_simu <- simuMSVAR(mdl_msvar2)

# Set options for model estimation
control <- list(msmu   = TRUE, 
                msvar  = TRUE,
                method = "EM",
                use_diff_init = 10)
                
# Estimate model
y_msvar_mdl <- MSVARmdl(y_msvar_simu$y, p = 1, k = 2, control)

y_msvar_mdl

plot(y_msvar_mdl$St[,2], type = 'l')
lines(y_msvar_simu$St, col = 'red', lty = 2)


```

## Hypothesis Testing

The main contribution of this r package is the hypothesis testing procedures it makes available. 

Here we use The LMC-LRT procedure proposed in Rodriguez Rondon & Dufour (2022a; 2022b).
```r
set.seed(123)

# Define DGP of MS AR process
mdl_ms2 <- list(n     = 500, 
                mu    = c(5,10),
                sigma = c(1,2),
                phi   = c(0.5),
                k     = 2,
                P     = rbind(c(0.90, 0.10),
                              c(0.10, 0.90)))

# Simulate process using simuMSAR() function
y_ms_simu <- simuMSAR(mdl_ms2)

# Set test procedure options
lmc_control = list(N = 99,
                  converge_check = NULL,
                   mdl_h0_control = list(const  = TRUE, 
                                         getSE  = TRUE),
                   mdl_h1_control = list(msmu   = TRUE, 
                                         msvar  = TRUE,
                                         getSE  = TRUE,
                                         method = "EM",
                                         maxit  = 500,
                                         use_diff_init = 1))


lmc_lrt <- LMCLRTest(y_ms_simu$y, p = 1 , k0 = 1 , k1 = 2, lmc_control)
lmc_lrt
```

We can also use the moment-based test procedure proposed by Dufour & Luger (2017)

```r
set.seed(123)

# Set test procedure options
lmc_control = list(N = 99,
                   simdist_N = 10000,
                   getSE = TRUE)

# perform test on Hamilton 1989 data
lmc_mb <- DLMCTest(y_ms_simu$y, p = 1, control = lmc_control)
lmc_mb
```

The package also makes available the Maximized Monte Carlo versions of both these tests and the standardized likelihood ratio test proposed by Hansen (1992) (see HLRTest()) and the parameter stability test of Carrasco, Hu, & Ploberger (2014) (see CHPTest()).


## References

Carrasco, Marine, Liang Hu, and Werner Ploberger. 2014. “Optimal test for Markov switching parameters.” *Econometrica* 82 (2): 765–784.

Dempster, A. P., N. M. Laird, and D. B. Rubin. 1977. “Maximum Likelihood from Incomplete Data via the EM Algorithm.” *Journal of the Royal Statistical Society*. Series B 39 (1): 1–38..

Dufour, Jean-Marie, and Richard Luger. 2017. “Identification-robust moment-based tests for Markov switching in autoregressive models.” *Econometric Reviews* 36 (6-9): 713–727.

Kasahara, Hiroyuk, and Katsum Shimotsu. 2018. “Testing the number of regimes in Markov regime switching models.” arXiv preprint arXiv:1801.06862.

Krolzig, Hans-Martin. 1997. “The markov-switching vector autoregressive model.”. *Springer*.

Hamilton, James D. 1989. “A new approach to the economic analysis of nonstationary time series and the business cycle.” *Econometrica* 57 (2): 357–384.

Hamilton, James D. 1994. "Time series analysis". Princeton university press.

Hansen, Bruce E. 1992. “The likelihood ratio test under nonstandard conditions: testing the Markov switching model of GNP.” *Journal of applied Econometrics* 7 (S1): S61–S82.

Rodriguez Rondon, Gabriel and Jean-Marie Dufour. 2022. "Simulation-Based Inference for Markov Switching Models” *JSM Proceedings, Business and Economic Statistics Section: American Statistical Association*.

Rodriguez Rondon, Gabriel and Jean-Marie Dufour. 2022. “Monte Carlo Likelihood Ratio Tests for Markov Switching Models.” Unpublished manuscript.

Rodriguez Rondon, Gabriel and Jean-Marie Dufour. 2022. “MSTest: An R-package for Testing Markov-Switching Models.” Unpublished manuscript.

Qu, Zhongjun, and Fan Zhuo. 2021. “Likelihood Ratio-Based Tests for Markov Regime Switching.” *The Review of Economic Studies* 88 (2): 937–968.
