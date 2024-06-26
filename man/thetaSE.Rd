% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helperfuncs.R
\name{thetaSE}
\alias{thetaSE}
\title{Theta standard errors}
\usage{
thetaSE(mdl)
}
\arguments{
\item{mdl}{List with model properties}
}
\value{
List provided as input with additional attributes \code{HESS},\code{theta_se}, \code{info_mat}, and \code{nearPD_used}.
}
\description{
This function computes the standard errors of the parameters in vector theta. This is done using an approximation of the Hessian matrix (using \code{\link[numDeriv]{hessian}} and \code{nearPD} if \code{info_mat} is not PD).
}
\keyword{internal}
