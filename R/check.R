#' Check survey data
#'
#' Check season and daily data for a given species and return corrections and audits.
#'
#' @importFrom rlang .data
#' @importFrom dplyr filter
#'
#' @param dailies_df Daily data tibble
#' @param season_df Season data tibble
#' @param maxbag_df Reference data tibble
#' @param species Species abbreviation, may be one of: 'WF', 'DV', 'SCRG', 'WK', or 'CR'.
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/HSestimate}
#' 
#' @export

editCheck <-
  function(dailies_df, season_df, maxbag_df, species) {
    failspp(species)
    
    # Fundamental data validation
    if (species != "WF") {
      if (species == "CR") {
        fails(season_df, dailies_df, REF_N_STATES_CR)
      }
      if (species == "DV") {
        fails(season_df, dailies_df, REF_N_STATES_DV)
      }
      if (species == "WK") {
        fails(season_df, dailies_df, REF_N_STATES_WK)
      }
      if (species == "SCRG") {
        fails(season_df, dailies_df, REF_N_STATES_SCRG)
      }
      
      # Check dailies
      daily_checked <- checkDaily(dailies_df, maxbag_df, species)
      daily_error_ids <- dailyErrorIDs(daily_checked, species)
      daily_corrected <- dailyCorrect(daily_checked, daily_error_ids, species)
      
      # Check season
      season_checked <- checkSeason(season_df, maxbag_df, species)
      season_error_ids <- seasonErrorIDs(season_checked)
      season_corrected <- 
        seasonCorrect(
          season_checked, season_error_ids, daily_error_ids, species)
      
    } else {
      fails(season_df, dailies_df, REF_N_STATES_WF)
      
      # Check dailies
      daily_checked <- checkDaily(dailies_df, maxbag_df, "WF")
      daily_error_ids <- dailyErrorIDs(daily_checked, "WF")
      daily_corrected <- dailyCorrect(daily_checked, daily_error_ids, "WF")
      
      # Check season 
      season_checked <- checkSeason(season_df, maxbag_df, "WF")
      season_error_ids <- seasonErrorIDs(season_checked)
      season_precorrected <- 
        seasonCorrect(season_checked, season_error_ids, daily_error_ids, "WF")
      
      season_corrected <- convertSDBR(daily_corrected, season_precorrected)
    }
    
    # Non-species-specific audits
    daily_audit <- dailyAudit(daily_checked, daily_error_ids)
    season_audit <- 
      seasonAudit(
        season_checked, season_corrected, season_error_ids, daily_error_ids)
    
    # Create list
    return_list <- 
      list(
        daily_corrected = daily_corrected, 
        daily_audit = daily_audit,
        season_corrected = season_corrected,
        season_audit = season_audit)
    
    return(return_list)
  }

#' Check season data
#'
#' Internal function used in \code{\link{editCheck}}. 
#' 
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom rlang .data
#'
#' @param season_df Season data tibble
#' @param maxbag_df Reference data tibble
#' @param species Species abbreviation, may be one of: 'WF', 'DV', 'SCRG', 'WK', or 'CR'.
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/HSestimate}

# Run the checks on the season data
checkSeason <-
  function(season_df, maxbag_df, species) {
    failspp(species)
    
    season_totals_df <-
      season_df |> 
      # Join in the maxbag table
      left_join(
        maxbag_df |> select(-c("state", "stateno")), 
        by = c("sampled_state", "sp_group_estimated")) |> 
      left_join(
        maxbag_df |> distinct(.data$sampled_state, .data$state, .data$stateno),
        by = "sampled_state")
    
    if (species %in% c("WF", "WK", "SCRG", "CR")) {
      season_check_NAdays <- naDaysHunted(season_totals_df)
      season_check_overdays <- tooManyDaysHunted(season_check_NAdays, species)
      season_check_overbag <- seasonOverBag(season_check_overdays, species)
      
    } else if (species == "DV") {
      # Proof WWDO days_hunted, retrieved, and unretrieved
      season_totals_df_wwdo <-
        season_totals_df |> 
        convertWWDO(type = "season")
      
      season_check_NAdays <- naDaysHunted(season_totals_df_wwdo)
      season_check_overdays <- tooManyDaysHunted(season_check_NAdays, species)
      season_check_overbag <- seasonOverBag(season_check_overdays, species)
    }
    
    season_check_DNH <- seasonDNH(season_check_overbag)
    return(season_check_DNH)
  }

#' Check daily data
#'
#' Internal function used in \code{\link{editCheck}}. 
#'
#' @param dailies_df Daily data tibble
#' @param maxbag_df Reference data tibble
#' @param species Species abbreviation, may be one of: 'WF', 'DV', 'SCRG', 'WK', or 'CR'.
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/HSestimate}

checkDaily <-
  function(dailies_df, maxbag_df, species) {
    failspp(species)
    
    if(species %in% c("CR", "WK", "SCRG")) {
      daily_check_party <- partyHuntFinder(dailies_df, maxbag_df)
      daily_check_overbag <- dailyOverBag(daily_check_party, maxbag_df)
      
    } else if(species == "DV") {
      dailies_wwdo_proofed <- convertWWDO(dailies_df, type = "daily")
      daily_check_party <- partyHuntFinder(dailies_wwdo_proofed, maxbag_df)
      daily_check_WWMOoverbag <- dailyOverBagWWMO(daily_check_party, maxbag_df)
      daily_check_overbag <- dailyOverBag(daily_check_WWMOoverbag, maxbag_df)
      
    } else if (species == "WF") {
      daily_check_party <- partyHuntFinder(dailies_df, maxbag_df)
      daily_check_DKSDoverbag <- dailyOverBagDKSD(daily_check_party, maxbag_df)
      daily_check_overbag <- dailyOverBag(daily_check_DKSDoverbag, maxbag_df)
    }
    
    return(daily_check_overbag)
  }
