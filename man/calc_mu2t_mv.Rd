% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{calc_mu2t_mv}
\alias{calc_mu2t_mv}
\title{Test statistic for switch in mean and variance}
\usage{
calc_mu2t_mv(mdl, rho, ltmt, hv)
}
\arguments{
\item{mdl}{List containing model attributes (see \code{\link{ARmdl}}).}

\item{rho}{Number determining value of \code{rho}.}

\item{ltmt}{List containing derivatives output from \code{\link{chpDmat}}.}

\item{hv}{Number determining value of \code{h}.}
}
\value{
Part of test statistic given \code{rho} and \code{hv} value.
}
\description{
This function computes part of the test statistic given by 
eq. 2.5 of CHP 2014 when the alternative has switching mean and variance. 
The output is used in \code{\link{chpStat}} which computes the full test
statistics.
}
\references{
Carrasco, Marine, Liang Hu, and Werner Ploberger. 2014. “Optimal 
test for Markov switching parameters.” \emph{Econometrica} 82 (2): 765–784.
}
\keyword{internal}
