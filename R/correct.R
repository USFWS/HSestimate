#' Correct season data
#'
#' Internal function used in \code{\link{editCheck}}. 
#' 
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom rlang .data
#'
#' @param season_checked Season data tibble checked by \code{\link{checkSeason}}
#' @param season_error_ids Survey IDs of records with errors in the season data
#' @param daily_error_ids Survey IDs of records with errors in the daily data
#' @param species Species abbreviation, may be one of: 'WF', 'DV', 'SCRG', 'WK', or 'CR'.
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/HSestimate}

seasonCorrect <-
  function(season_checked, season_error_ids, daily_error_ids, species) {
    failspp(species)
    
    corrected_szn <-
      season_checked |> 
      # Do not include any survey IDs with an error in the season totals
      filter(!.data$surveyID %in% season_error_ids) |> 
      # Do not include any survey IDs with an error in the dailies
      filter(!.data$surveyID %in% daily_error_ids)
    
    if(species == "DV"){
      corrected_szn |> 
        select(-c("wwdo_error", "error1", "error2", "error3", "error4"))
    } else if (species %in% c("WF", "SCRG", "WK", "CR")) {
      corrected_szn |> 
        select(-c("error1", "error2", "error3", "error4"))
    }
  }

#' Correct daily data
#'
#' Internal function used in \code{\link{editCheck}}. 
#' 
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom rlang .data
#'
#' @param daily_checked Daily data tibble checked by \code{\link{checkDaily}}
#' @param daily_error_ids Survey IDs of records with errors in the daily data
#' @param species Species abbreviation, may be one of: 'WF', 'DV', 'SCRG', 'WK', or 'CR'.
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/HSestimate}

dailyCorrect <-
  function(daily_checked, daily_error_ids, species) {
    failspp(species)
    
    corrected_daily <-
      daily_checked |> 
      filter(!.data$surveyID %in% daily_error_ids)
    
    if(species == "WF"){
      corrected_daily |> 
        select(-c("error1", "error2", "error3"))
    } else if(species == "DV"){
      corrected_daily |> 
        select(-c("wwdo_error", "error1", "error2", "error3"))
    } else if (species %in% c("SCRG", "WK", "CR")) {
      corrected_daily |> 
        select(-c("error1", "error3"))
    }
  }
