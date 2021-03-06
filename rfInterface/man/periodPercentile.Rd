% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shinyfunctions.R
\name{periodPercentile}
\alias{periodPercentile}
\title{Percentiles of daily flow over specified periods}
\usage{
periodPercentile(daily, pc = c(0.5, 0.95), period = "year")
}
\arguments{
\item{daily}{two-column dataframe: date, flow}

\item{pc}{vector of percentiles of exceedence}

\item{period}{string (\code{month}, \code{year}, \code{decade}) indicating period}
}
\value{
data.frame with period, percentiles and period minimum and maximum.
}
\description{
This computes percentiles of flow given daily flow records,
values computed over each period as selected. Returned as time-series.
}
