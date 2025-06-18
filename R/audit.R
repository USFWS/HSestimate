#' Audit season data
#'
#' Internal function used in \code{\link{editCheck}}. 
#' 
#' @importFrom dplyr filter
#' @importFrom dplyr bind_rows
#' @importFrom rlang .data
#'
#' @param season_checked Season data tibble checked by \code{\link{checkSeason}}
#' @param season_corrected Season data tibble corrected by \code{\link{seasonCorrect}} (and additionally \code{\link{convertSDBR}} for WF)
#' @param season_error_ids Survey IDs of records with errors in the season data
#' @param daily_error_ids Survey IDs of records with errors in the daily data
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/HSestimate}

seasonAudit <-
  function(season_checked, season_corrected, season_error_ids, daily_error_ids) {
    season_checked |> 
      filter(.data$surveyID %in% season_error_ids) |> 
      # Add in survey IDs with an error in the daily audit file too
      bind_rows(
        season_corrected |> 
          filter(.data$surveyID %in% daily_error_ids))
  }

#' Audit daily data
#'
#' Internal function used in \code{\link{editCheck}}. 
#' 
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @param daily_checked Daily data tibble checked by \code{\link{checkDaily}}
#' @param daily_error_ids Survey IDs of records with errors in the daily data
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/HSestimate}

dailyAudit <-
  function(daily_checked, daily_error_ids) {
    filter(daily_checked, .data$surveyID %in% daily_error_ids)
  }