% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models.R
\name{simuAR}
\alias{simuAR}
\title{Simulate autoregressive process}
\usage{
simuAR(mdl_h0, burnin = 100)
}
\arguments{
\item{mdl_h0}{List containing the following DGP parameters
\itemize{
  \item n: Length of series.
  \item mu: Mean of process.
  \item sigma: Standard deviation of process.
  \item phi: Vector of autoregressive coefficients.
  \item eps: An optional (\code{T+burnin x q}) matrix with standard normal errors to be used. Errors will be generated if not provided.
}}

\item{burnin}{Number of simulated observations to remove from beginning. Default is \code{100}.}
}
\value{
List with simulated autoregressive series and its DGP parameters.
}
\description{
This function simulates an autoregresive process.
}
\examples{
set.seed(1234)
# Define DGP of AR process
mdl_ar <- list(n     = 500, 
               mu    = 5,
               sigma = 2,
               phi   = c(0.5,0.2))

# Simulate process using simuAR() function
y_simu <- simuAR(mdl_ar)

plot(y_simu)
}
