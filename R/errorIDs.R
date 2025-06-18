#' Pull daily data surveyIDs for records that contain errors
#'
#' Internal function used in \code{\link{editCheck}}. 
#' 
#' @importFrom dplyr filter
#' @importFrom dplyr distinct
#' @importFrom dplyr pull
#' @importFrom rlang .data
#'
#' @param daily_checked Daily data tibble checked by \code{\link{checkDaily}}
#' @param species Species abbreviation, may be one of: 'WF', 'DV', 'SCRG', 'WK', or 'CR'.
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/HSestimate}

dailyErrorIDs <- 
  function(daily_checked, species) {
    failspp(species)
    
    if(species %in% c("WF", "DV")) {
      daily_checked |> 
        filter(!is.na(.data$error1) |
                 !is.na(.data$error2) | 
                 !is.na(.data$error3)) |>
        distinct(surveyID) |> 
        pull()
    } else if(species %in% c("SCRG", "WK", "CR")) {
      daily_checked |> 
        filter(!is.na(.data$error1) | !is.na(.data$error3)) |>
        distinct(surveyID) |> 
        pull()
    }
  }

#' Pull season data surveyIDs for records that contain errors
#'
#' Internal function used in \code{\link{editCheck}}. 
#' 
#' @importFrom dplyr filter
#' @importFrom dplyr distinct
#' @importFrom dplyr pull
#' @importFrom rlang .data
#'
#' @param season_checked Season data tibble checked by \code{\link{checkSeason}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/HSestimate}

seasonErrorIDs <-
  function(season_checked) {
    season_checked |> 
      filter(
        !is.na(.data$error1) |
          !is.na(.data$error2) |
          !is.na(.data$error3) | 
          !is.na(.data$error4)) |>
      distinct(surveyID) |> 
      pull()
  }