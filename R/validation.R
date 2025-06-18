#' Find records with NA days hunted
#'
#' Internal function used in \code{\link{checkSeason}}. Find season records with NA in days_hunted field.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @param season_df Season data tibble
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/HSestimate}

naDaysHunted <- 
  function(season_df) {
    # Flag records that have NA in days_hunted from the season_df 
    totals_validated <-
      season_df |> 
      mutate(error1 = ifelse(is.na(.data$days_hunted), "NA_days_hunted", NA))
    
    message(
      paste(
        "Season: There are", nrow(filter(season_df, is.na(.data$days_hunted))), 
        "records with NA for days_hunted.", sep = " ")
    )
    return(totals_validated)
  }

#' Find records with too many days hunted
#'
#' Internal function used in \code{\link{checkSeason}}. Find season records with too many days hunted.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @param season_df Season data tibble
#' @param species Species code string, may be one of: 'WF', 'DV', 'SCRG', 'WK', or 'CR'.
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/HSestimate}

tooManyDaysHunted <-
  function(season_df, species) {
    failspp(species)
    
    if (species == "CR") {
      totals_validated <-
        season_df |> 
        mutate(
          error2 = 
            case_when(
              .data$sampled_state == "Alaska" & 
                .data$days_hunted > REF_DAY_LIMIT_CR_AK ~ 
                paste("too many days hunted (limit", REF_DAY_LIMIT_CR_AK, "in",
                      "AK)", sep = " "), 
              .data$sampled_state %in% REF_STATES_CR_SOUTH &
                .data$days_hunted > REF_DAY_LIMIT_CR_SOUTH ~ 
                paste0(
                  "too many days hunted (limit ", REF_DAY_LIMIT_CR_SOUTH, 
                  " in ", paste(REF_STATES_CR_SOUTH, collapse = ", "), ")"), 
              .data$sampled_state %in% REF_STATES_CR_NORTH & 
                .data$days_hunted > REF_DAY_LIMIT_CR_NORTH ~ 
                paste0(
                  "too many days hunted (limit ", REF_DAY_LIMIT_CR_NORTH, 
                  " in ", paste(REF_STATES_CR_NORTH, collapse = ", "), ")"),
              TRUE ~ NA_character_))
      
      message(
        paste(
          "Season: There are", 
          nrow(totals_validated |> filter(!is.na(.data$error2))), 
          "records exceeding the state limit for crane days_hunted.", sep = " ")
      )
    } else {
      if (species == "WF") {
        day_limit <- REF_DAY_LIMIT_WF
      }
      if (species == "DV") {
        day_limit <- REF_DAY_LIMIT_DV
      }
      if (species == "WK") {
        day_limit <- REF_DAY_LIMIT_WK
      }
      if (species == "SCRG") {
        day_limit <- REF_DAY_LIMIT_SCRG
      }
      
      # Flag records with value > day_limit in days_hunted from season_df
      totals_validated <-
        season_df |> 
        mutate(
          error2 = 
            ifelse(.data$days_hunted > day_limit, "too_many_days", NA))
      
      message(
        paste(
          "Season: There are", 
          nrow(season_df |> filter(.data$days_hunted > day_limit)), 
          "records with >", day_limit, "days_hunted.", sep = " ")
      )
    }
    return(totals_validated)
  }

#' Find season did-not-hunts
#'
#' Internal function used in \code{\link{checkSeason}}. Find season records with a sum of 0 for days_hunted and sum > 0 for retrieved.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @param season_df Season data tibble
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/HSestimate}

seasonDNH <- 
  function(season_df) {
    
    totals_validated <-
      season_df |> 
      mutate(
        sum_days_hunted = sum(.data$days_hunted),
        sum_retrieved = sum(.data$retrieved),
        .by = "surveyID") |> 
      mutate(
        error4 = 
          ifelse(
            .data$sum_days_hunted == 0 & .data$sum_retrieved > 0, 
            "sum_days_hunted_0", 
            NA)) |> 
      select(-c("sum_days_hunted", "sum_retrieved"))
    
    message(
      paste(
        "Season: There are", 
        nrow(filter(totals_validated, !is.na(.data$error4))), 
        "records with retrieved > 0 but sum of days hunted is 0.", sep = " ")
    )
    
    return(totals_validated)
  }