#' Fail if NA values in days_hunted field
#'
#' Internal function that fails if \code{NA} values are found in the
#' \code{days_hunted} field of season data.
#'
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @param season_df Season data tibble
#'
#' @family failure functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

failNADaysHunted <-
  function(season_df) {

    # Fail if there are NA values in days_hunted
    stopifnot(
      "NA in totals days_hunted" =
        nrow(filter(season_df, is.na(.data$days_hunted))) == 0)
  }

#' Fail if NA values in retrieved field
#'
#' Internal function that fails if \code{NA} values are found in the
#' \code{retrieved} field of season and/or daily data.
#'
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @param data Season or daily data tibble
#'
#' @family failure functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

failNARetrieved <-
  function(data) {

    # Fail if there are NA values in retrieved
    stopifnot(
      "NA in retrieved" =
        nrow(filter(data, is.na(.data$retrieved))) == 0)
  }

#' Fail state count
#'
#' Internal function that fails if the number of states expected is not met.
#'
#' @importFrom dplyr distinct
#' @importFrom rlang .data
#'
#' @param season_df Season data tibble
#' @param dailies_df Daily data tibble
#' @param n_states Number of expected states in the data
#'
#' @family failure functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

failStateCount <-
  function(season_df, dailies_df, n_states) {
    # Fail if n_states is not double
    stopifnot("n_states must be type double" = is.double(n_states))

    # Error message if there are not all n_states in the data
    if (nrow(distinct(season_df, .data$sampled_state)) != n_states) {
      message(paste0("Number of states in totals file != ", n_states, "."))
    }
    if (nrow(distinct(dailies_df, .data$sampled_state)) != n_states) {
      message(paste0("Number of states in dailies file != ", n_states, "."))
    }
  }

#' Fail species
#'
#' Internal function used to validate the \code{species} provided.
#'
#' @param species Species abbreviation, may be one of: 'WF', 'DV', 'SCRG', 'WK',
#'   or 'CR'.
#'
#' @family failure functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

failspp <-
  function(species) {
    stopifnot(
      "`species` must be 'WF', 'DV', 'SCRG', 'WK', or 'CR'." =
        species %in% c("WF", "DV", "SCRG", "WK", "CR"))
  }

#' Fail type
#'
#' Internal function used to validate the \code{type} provided, which aids in
#' harvest estimation calculation.
#'
#' @param type Type of estimation being calculated, may be one of: 'Ducks',
#'   'Geese', 'Brant', 'SeaDucks', 'MODO', 'WWDO', 'SACR', 'Woodcock', 'Snipe',
#'   'Coots', 'Rails', 'Gallinules', 'BTPI'.
#'
#' @family failure functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

failtype <-
  function(type) {
    stopifnot(
      "Error: type must be one of: 'Ducks', 'Geese', 'Brant'... etc" =
        type %in% c(
          "Ducks",
          "Geese",
          "Brant",
          "SeaDucks",
          "MODO",
          "WWDO",
          "SACR",
          "Woodcock",
          "Snipe",
          "Coots",
          "Rails",
          "Gallinules",
          "BTPI"
        )
    )
  }
