% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/audit.R
\name{seasonAudit}
\alias{seasonAudit}
\title{Audit season data}
\usage{
seasonAudit(
  season_checked,
  season_corrected,
  season_error_ids,
  daily_error_ids
)
}
\arguments{
\item{season_checked}{Season data tibble checked by \code{\link{checkSeason}}}

\item{season_corrected}{Season data tibble corrected by \code{\link{seasonCorrect}} (and additionally \code{\link{convertSDBR}} for WF)}

\item{season_error_ids}{Survey IDs of records with errors in the season data}

\item{daily_error_ids}{Survey IDs of records with errors in the daily data}
}
\description{
Internal function used in \code{\link{editCheck}}.
}
\references{
\url{https://github.com/USFWS/HSestimate}
}
\author{
Abby Walter, \email{abby_walter@fws.gov}
}
