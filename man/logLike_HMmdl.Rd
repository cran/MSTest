% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{logLike_HMmdl}
\alias{logLike_HMmdl}
\title{Hidden Markov model log-likelihood function}
\usage{
logLike_HMmdl(theta, mdl, k)
}
\arguments{
\item{\code{theta}}{Vector of model parameters.}

\item{\code{mdl}}{List with model attributes.}

\item{\code{k}}{Integer determining the number of regimes.}
}
\value{
Log-likelihood value.
}
\description{
This function computes the log-likelihood for a markov-switching autoregressive model.
}
\keyword{internal}
