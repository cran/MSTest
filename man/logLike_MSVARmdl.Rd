% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{logLike_MSVARmdl}
\alias{logLike_MSVARmdl}
\title{Markov-switching vector autoregressive log-likelihood objective function}
\usage{
logLike_MSVARmdl(theta, mdl, k)
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
This function computes the log-likelihood for a markov-switching vector autoregressive model.
}
\keyword{internal}
