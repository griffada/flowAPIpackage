% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shinyfunctions.R
\name{dglo}
\alias{dglo}
\alias{qglo}
\title{FEH Generalised Logistic Distribution}
\usage{
dglo(x, loc, scale, sh)

qglo(p, loc, scale, sh, lower.tail = TRUE)
}
\arguments{
\item{x, p}{a vector of values or probabilities of exceedence}

\item{loc}{location parameter}

\item{scale}{scale parameter > 0}

\item{sh}{shape parameter \eqn{ \xi \in [-1,1]}}

\item{lower.tail}{if true, computes the quantile for \eqn{P[X < x] = p}}
}
\value{
evaluation of density and quantile function
}
\description{
Density and Quantile functions for the FEH 3-parameter generalised Logistic
distribution, as appears in \code{ilaprosUtils}.
}
