#' Fail data
#'
#' Internal function used to repeat validation on season and daily survey data.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr distinct
#' @importFrom rlang .data
#'
#' @param season_df Season data tibble
#' @param dailies_df Daily data tibble
#' @param n_states Number of expected states in the data
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/HSestimate}

fails <-
  function(season_df, dailies_df, n_states) {
    # Fail if n_states is not double
    stopifnot("n_states must be type double" = is.double(n_states))
    
    # Fail if there are NA values in days_hunted
    stopifnot(
      "NA in totals days_hunted" = 
        nrow(filter(season_df, is.na(.data$days_hunted))) == 0)
    
    # Fail if there are NA values in retrieved
    stopifnot(
      "NA in totals retrieved" = 
        nrow(filter(season_df, is.na(.data$retrieved))) == 0)
    stopifnot(
      "NA in dailies retrieved" = 
        nrow(filter(dailies_df, is.na(.data$retrieved))) == 0)
    
    # Error message if there are not all n_states in the data
    if(nrow(distinct(season_df, .data$sampled_state)) != n_states) {
      message(paste0("Number of states in totals file != ", n_states, "."))
    }
    if(nrow(distinct(dailies_df, .data$sampled_state)) != n_states) {
      message(paste0("Number of states in dailies file != ", n_states, "."))
    }
  }

#' Fail species
#'
#' Internal function used to repeat species code validation.
#'
#' @param species Species code string, may be one of: 'WF', 'DV', 'SCRG', 'WK', or 'CR'.
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/HSestimate}

failspp <-
  function(species) {
    stopifnot(
      "`species` must be 'WF', 'DV', 'SCRG', 'WK', or 'CR'." = 
        species %in% c("WF", "DV", "SCRG", "WK", "CR"))
  }