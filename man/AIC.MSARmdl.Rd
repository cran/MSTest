% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{AIC.MSARmdl}
\alias{AIC.MSARmdl}
\title{AIC of a \code{MSARmdl} object}
\usage{
\method{AIC}{MSARmdl}(object, ..., k = 2)
}
\arguments{
\item{object}{a fitted model object for which there exists a
    \code{logLik} method to extract the corresponding log-likelihood, or
    an object inheriting from class \code{logLik}.}

\item{...}{optionally more fitted model objects.}

\item{k}{numeric, the \emph{penalty} per parameter to be used; the
    default \code{k = 2} is the classical AIC.}
}
\value{
AIC value.
}
\description{
This is a method for the function \code{AIC()} for objects of the class \code{MSARmdl}.
}
\keyword{internal}
