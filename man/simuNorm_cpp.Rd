% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{simuNorm_cpp}
\alias{simuNorm_cpp}
\title{Simulate normally distributed process}
\usage{
simuNorm_cpp(mdl_h0, burnin = 0L)
}
\arguments{
\item{mdl_h0}{List containing the following DGP parameters
\itemize{
  \item n: Length of series.
  \item mu: A (\code{q x 1}) vector of means.
  \item sigma: A (\code{q x q}) covariance matrix.
  \item q: Number of series.
  \item eps: An optional (\code{T+burnin x q}) matrix with standard normal errors to be used. Errors will be generated if not provided.
}}

\item{burnin}{Number of simulated observations to remove from beginning. Default is \code{100}.}
}
\value{
List with simulated series and its DGP parameters.
}
\description{
This function simulates a normally distributed process.
}
\keyword{internal}
