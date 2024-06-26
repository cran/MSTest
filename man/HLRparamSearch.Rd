% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/htest_HLRTest.R
\name{HLRparamSearch}
\alias{HLRparamSearch}
\title{HLR param search}
\usage{
HLRparamSearch(gx, gp, gq, b, null, HLR_opt_ls)
}
\arguments{
\item{gx}{matrix/grid containing values for switching parameters in second regime.}

\item{gp}{matrix/grid containing values for probability of going from regime 1 at (\code{t}) to regime 1 at (\code{t+1}).}

\item{gq}{matrix/grid containing values for probability of going from regime 2 at (\code{t}) to regime 2 at (\code{t+1}) (if not bounded to be \code{1-p} i.e., \code{qbound=FALSE}).}

\item{b}{vector of initial parameter values of restricted model.}

\item{null}{vector with likelihood under the null hypothesis.}

\item{HLR_opt_ls}{List with model properties and test controls defined in \code{HLRTest()}.}
}
\value{
List which contains:
\itemize{
  \item cs: Vector with standardized LRT statistic for each grid point.
  \item draws: List with a (\code{nwband+1 x N} matrix for each grid point. Each row of these matrices is a vector of simulated test statistics under the null hypothesis for a value of bandwidth .
  \item coefficients: A  matrix with coefficients for each grid point.
}
}
\description{
This function performs the parameter grid search needed for 
the likelihood ratio test described in Hansen (1992).
}
\keyword{internal}
