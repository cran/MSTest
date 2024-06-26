% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{EMiter_HMmdl}
\alias{EMiter_HMmdl}
\title{EM algorithm iteration for Hidden Markov model}
\usage{
EMiter_HMmdl(mdl, EMest_output, k)
}
\arguments{
\item{mdl}{List with model attributes.}

\item{EMest_output}{List with attributes from previous iteration.}

\item{k}{Integer determining the number of regimes.}
}
\value{
List with attributes from new iteration.
}
\description{
This function performs the one iteration (E-step and M-step) of the Expectation Maximization algorithm for a Hidden Markov model.
}
\keyword{internal}
