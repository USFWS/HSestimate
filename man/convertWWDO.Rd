% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert.R
\name{convertWWDO}
\alias{convertWWDO}
\title{Find season or daily WWDO harvest effort in non-WWDO states and convert to zero}
\usage{
convertWWDO(data_df, type, summary = F)
}
\arguments{
\item{data_df}{Season or daily data tibble}

\item{type}{"season" or "daily"}

\item{summary}{Whether a summary of WWDO errors should be returned; TRUE or FALSE}
}
\description{
Internal function used in \code{\link{checkSeason}} and \code{\link{checkDaily}}. Find any harvest of WWDO in non-wwdo states and WWDO in edge states that exceed the designated limit; for these cases, change WWDO days_hunted, retrieved, and unretrieved values to 0.
}
\references{
\url{https://github.com/USFWS/HSestimate}
}
\author{
Abby Walter, \email{abby_walter@fws.gov}
}
