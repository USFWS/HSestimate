#' Check survey data
#'
#' The \code{surveyCheck} function checks season and daily survey data for a
#' given species and returns corrections and audits.
#'
#' @param dailies_df Daily data tibble
#' @param season_df Season data tibble
#' @param maxbag_df Reference data tibble
#' @param species Species abbreviation, may be one of: 'WF', 'DV', 'SCRG', 'WK',
#'   or 'CR'.
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#'
#' @family checking functions
#' @family crane functions
#' @family dove functions
#' @family SCRG functions
#' @family waterfowl functions
#' @family woodcock functions
#'
#' @export

surveyCheck <-
  function(dailies_df, season_df, maxbag_df, species) {
    failspp(species)
    failNADaysHunted(season_df)
    failNARetrieved(season_df)
    failNARetrieved(dailies_df)

    if (species == "CR") {
      failStateCount(season_df, dailies_df, REF_N_STATES_CR)

      daily_check <- checkDailySCRGWKCR(dailies_df, maxbag_df)
      season_check <- checkSeasonCR(season_df, maxbag_df)
      audit(daily_check, season_check)

    } else if (species == "DV") {
      failStateCount(season_df, dailies_df, REF_N_STATES_DV)

      daily_check <- checkDailyDV(dailies_df, maxbag_df)
      season_check <- checkSeasonDV(season_df, maxbag_df)
      auditDV(daily_check, season_check)

    } else if (species == "WK") {
      failStateCount(season_df, dailies_df, REF_N_STATES_WK)

      day_limit <- REF_DAY_LIMIT_WK
      daily_check <- checkDailySCRGWKCR(dailies_df, maxbag_df)
      season_check <- checkSeasonWFSCRGWK(season_df, maxbag_df, day_limit)
      audit(daily_check, season_check)

    } else if (species == "SCRG") {
      failStateCount(season_df, dailies_df, REF_N_STATES_SCRG)

      day_limit <- REF_DAY_LIMIT_SCRG
      daily_check <- checkDailySCRGWKCR(dailies_df, maxbag_df)
      season_check <- checkSeasonWFSCRGWK(season_df, maxbag_df, day_limit)
      audit(daily_check, season_check)

    } else if (species == "WF") {
      failStateCount(season_df, dailies_df, REF_N_STATES_WF)
      checkWF(dailies_df, season_df, maxbag_df)

    }
  }

#' Check survey data
#'
#' Internal function that checks season and daily data to return corrections and
#' audits.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr if_any
#' @importFrom dplyr contains
#' @importFrom dplyr distinct
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom dplyr bind_rows
#' @importFrom rlang .data
#'
#' @param daily_check Tibble output from a \code{checkDaily} function
#' @param season_check Tibble output from a \code{checkSeason} function
#'
#' @family checking functions
#' @family crane functions
#' @family SCRG functions
#' @family woodcock functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

audit <-
  function(daily_check, season_check) {

    daily_error_ids <-
      daily_check |>
      filter(if_any(contains("error"), \(x) ! is.na(x))) |>
      distinct(.data$surveyID) |>
      pull()

    season_error_ids <-
      season_check |>
      filter(if_any(contains("error"), \(x) ! is.na(x))) |>
      distinct(.data$surveyID) |>
      pull()

    daily_corrected <-
      daily_check |>
      filter(!.data$surveyID %in% daily_error_ids) |>
      select(!contains("error"))

    daily_audit <-
      daily_check |>
      filter(.data$surveyID %in% daily_error_ids)

    season_corrected <-
      season_check |>
      # Do not include any survey IDs with an error in the season totals
      filter(!.data$surveyID %in% season_error_ids) |>
      # Do not include any survey IDs with an error in the dailies
      filter(!.data$surveyID %in% daily_error_ids) |>
      select(!contains("error"))

    season_audit <-
      season_check |>
      filter(.data$surveyID %in% season_error_ids) |>
      # Add in survey IDs with an error in the daily audit file too
      bind_rows(season_corrected |>
                  filter(.data$surveyID %in% daily_error_ids))

    # Return list
    list(
      daily_corrected = daily_corrected,
      daily_audit = daily_audit,
      season_corrected = season_corrected,
      season_audit = season_audit
    )
  }

#' Check dove survey data
#'
#' Internal function that checks season and daily data for doves to return
#' corrections and audits.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr if_any
#' @importFrom dplyr matches
#' @importFrom dplyr contains
#' @importFrom dplyr distinct
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom dplyr bind_rows
#' @importFrom rlang .data
#'
#' @param daily_check Tibble output from a \code{checkDaily} function
#' @param season_check Tibble output from a \code{checkSeason} function
#'
#' @family checking functions
#' @family dove functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

auditDV <-
  function(daily_check, season_check) {

    daily_error_ids <-
      daily_check |>
      filter(if_any(matches("error[1-4]{1}"), \(x) ! is.na(x))) |>
      distinct(.data$surveyID) |>
      pull()

    season_error_ids <-
      season_check |>
      filter(if_any(matches("error[1-4]{1}"), \(x) ! is.na(x))) |>
      distinct(.data$surveyID) |>
      pull()

    daily_corrected <-
      daily_check |>
      filter(!.data$surveyID %in% daily_error_ids) |>
      select(!contains("error"))

    daily_audit <-
      daily_check |>
      filter(.data$surveyID %in% daily_error_ids)

    season_corrected <-
      season_check |>
      # Do not include any survey IDs with an error in the season totals
      filter(!.data$surveyID %in% season_error_ids) |>
      # Do not include any survey IDs with an error in the dailies
      filter(!.data$surveyID %in% daily_error_ids) |>
      select(!contains("error"))

    season_audit <-
      season_check |>
      filter(.data$surveyID %in% season_error_ids) |>
      # Add in survey IDs with an error in the daily audit file too
      bind_rows(season_corrected |>
                  filter(.data$surveyID %in% daily_error_ids))

    # Return list
    list(
      daily_corrected = daily_corrected,
      daily_audit = daily_audit,
      season_corrected = season_corrected,
      season_audit = season_audit
    )
  }

#' Check waterfowl survey data
#'
#' Internal function that checks season and daily data for waterfowl and returns
#' corrections and audits.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr if_any
#' @importFrom dplyr contains
#' @importFrom dplyr distinct
#' @importFrom dplyr pull
#' @importFrom dplyr select
#' @importFrom dplyr bind_rows
#' @importFrom rlang .data
#'
#' @param dailies_df Daily data tibble
#' @param season_df Season data tibble
#' @param maxbag_df Reference data tibble
#'
#' @family checking functions
#' @family waterfowl functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

checkWF <-
  function(dailies_df, season_df, maxbag_df) {

    # Check dailies
    daily_check <- checkDailyWF(dailies_df, maxbag_df)

    # Check season
    season_check <- checkSeasonWFSCRGWK(season_df, maxbag_df, REF_DAY_LIMIT_WF)

    daily_error_ids <-
      daily_check |>
      filter(if_any(contains("error"), \(x) ! is.na(x))) |>
      distinct(.data$surveyID) |>
      pull()

    daily_corrected <-
      daily_check |>
      filter(!.data$surveyID %in% daily_error_ids) |>
      select(!contains("error"))

    daily_audit <-
      daily_check |>
      filter(.data$surveyID %in% daily_error_ids)

    season_error_ids <-
      season_check |>
      filter(if_any(contains("error"), \(x) ! is.na(x))) |>
      distinct(.data$surveyID) |>
      pull()

    season_corrected <-
      season_check |>
      # Do not include any survey IDs with an error in the season totals
      filter(!.data$surveyID %in% season_error_ids) |>
      # Do not include any survey IDs with an error in the dailies
      filter(!.data$surveyID %in% daily_error_ids) |>
      select(!contains("error"))

    season_audit <-
      season_check |>
      filter(.data$surveyID %in% season_error_ids) |>
      # Add in survey IDs with an error in the daily audit file too
      bind_rows(season_corrected |>
                  filter(.data$surveyID %in% daily_error_ids))

    # Convert seaducks to ducks and brant to geese

    # If there are any seaduck or brant records from non-seaduck or non-brant
    # states in the daily data, add them to duck and goose records,
    # respectively, in the season totals
    sdbr_converted <- convertSDBR(daily_corrected, season_corrected)

    season_out <-
      if (is.null(sdbr_converted)) {
        season_corrected
      } else {
        sdbr_converted
      }

    # Return list
    list(
      daily_corrected = daily_corrected,
      daily_audit = daily_audit,
      season_corrected = season_out,
      season_audit = season_audit
    )
  }

#' Check waterfowl daily data
#'
#' Internal function to check daily survey data for waterfowl.
#'
#' @param dailies_df Daily data tibble
#' @param maxbag_df Reference data tibble
#'
#' @family checking functions
#' @family daily data helpers
#' @family waterfowl functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

checkDailyWF <-
  function(dailies_df, maxbag_df) {
    daily_check1 <- partyHuntFinder(dailies_df, maxbag_df)
    daily_check2 <- dailyOverBagDKSD(daily_check1, maxbag_df)
    daily_check3 <- dailyOverBag(daily_check2, maxbag_df)

    return(daily_check3)
  }

#' Check daily data
#'
#' Internal function to check daily survey data for SC, RG, WK, CR.
#'
#' @param dailies_df Daily data tibble
#' @param maxbag_df Reference data tibble
#'
#' @family checking functions
#' @family daily data helpers
#' @family crane functions
#' @family SCRG functions
#' @family woodcock functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

checkDailySCRGWKCR <-
  function(dailies_df, maxbag_df) {
    daily_check1 <- partyHuntFinder(dailies_df, maxbag_df)
    daily_check3 <- dailyOverBag(daily_check1, maxbag_df)

    return(daily_check3)
  }

#' Check dove daily data
#'
#' Internal function to check daily survey data for doves.
#'
#' @param dailies_df Daily data tibble
#' @param maxbag_df Reference data tibble
#'
#' @family checking functions
#' @family daily data helpers
#' @family dove functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

# Run the checks on the daily data
checkDailyDV <-
  function(dailies_df, maxbag_df) {

    dailies_wwdo_proofed <- convertWWDO(dailies_df, type = "daily")
    daily_check1 <- partyHuntFinder(dailies_wwdo_proofed, maxbag_df)
    daily_check2 <- dailyOverBagWWMO(daily_check1, maxbag_df)
    daily_check3 <- dailyOverBag(daily_check2, maxbag_df)

    return(daily_check3)
  }

#' Check season data
#'
#' Internal function to check season survey data for waterfowl, snipe, coot,
#' rails, gallinules, and woodcock.
#'
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom rlang .data
#'
#' @param season_df Season data tibble
#' @param maxbag_df Reference data tibble
#' @param day_limit Day limit for species being checked
#'
#' @family checking functions
#' @family season data helpers
#' @family SCRG functions
#' @family waterfowl functions
#' @family woodcock functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

checkSeasonWFSCRGWK <-
  function(season_df, maxbag_df, day_limit) {

    season_totals_df <-
      season_df |>
      # Join in the maxbag table
      left_join(maxbag_df |>
                  select(-c("state", "stateno")),
                by = c("sampled_state", "sp_group_estimated")) |>
      left_join(maxbag_df |>
                  distinct(.data$sampled_state, .data$state, .data$stateno),
                by = "sampled_state")

    season_check1 <- naDaysHunted(season_totals_df)
    season_check2 <- tooManyDaysHunted(season_check1, day_limit)
    season_check3 <- seasonOverBag(season_check2)
    season_check4 <- seasonDNH(season_check3)

    return(season_check4)
  }

#' Check crane season data
#'
#' Internal function to check season survey data for cranes.
#'
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom rlang .data
#'
#' @param season_df Season data tibble
#' @param maxbag_df Reference data tibble
#'
#' @family checking functions
#' @family season data helpers
#' @family crane functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

checkSeasonCR <-
  function(season_df, maxbag_df) {

    season_totals_df <-
      season_df |>
      # Join in the maxbag table
      left_join(maxbag_df |>
                  select(-c("state", "stateno")),
                by = c("sampled_state", "sp_group_estimated")) |>
      left_join(maxbag_df |>
                  distinct(.data$sampled_state, .data$state, .data$stateno),
                by = "sampled_state")

    season_check1 <- naDaysHunted(season_totals_df)
    season_check2 <- tooManyDaysHuntedCR(season_check1)
    season_check3 <- seasonOverBag(season_check2)
    season_check4 <- seasonDNH(season_check3)

    return(season_check4)
  }

#' Check dove season data
#'
#' Internal function to check season survey data for doves.
#'
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom rlang .data
#'
#' @param season_df Season data tibble
#' @param maxbag_df Reference data tibble
#'
#' @family checking functions
#' @family season data helpers
#' @family dove functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

checkSeasonDV <-
  function(season_df, maxbag_df) {

    season_totals_df <-
      season_df |>
      # Join in the maxbag table
      left_join(maxbag_df |>
                  select(-c("state", "stateno")),
                by = c("sampled_state", "sp_group_estimated")) |>
      left_join(maxbag_df |>
                  distinct(.data$sampled_state, .data$state, .data$stateno),
                by = "sampled_state")

    season_totals_df_wwdo <- convertWWDO(season_totals_df, type = "season")
    season_check1 <- naDaysHunted(season_totals_df_wwdo)
    season_check2 <- tooManyDaysHunted(season_check1, REF_DAY_LIMIT_DV)
    season_check3 <- seasonOverBagDV(season_check2)
    season_check4 <- seasonDNH(season_check3)

    return(season_check4)
  }

#' Find records with NA days hunted
#'
#' Internal function used in the \code{checkSeason} family of functions. Finds
#' season records with \code{NA} values in \code{days_hunted} field.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @param season_df Season data tibble
#'
#' @family checking functions
#' @family season data helpers
#' @family error assigning functions
#' @family crane functions
#' @family dove functions
#' @family SCRG functions
#' @family waterfowl functions
#' @family woodcock functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

naDaysHunted <-
  function(season_df) {

    totals_validated <-
      season_df |>
      mutate(error1 = ifelse(is.na(.data$days_hunted), "NA_days_hunted", NA))

    message(paste(
      "Season: There are",
      nrow(filter(season_df, is.na(.data$days_hunted))),
      "records with NA for days_hunted.",
      sep = " "
    ))

    return(totals_validated)
  }

#' Find records with too many days hunted
#'
#' Internal function used in the \code{checkSeason} family of functions. Find
#' season records with too many days hunted.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @param season_df Season data tibble
#' @param day_limit Day limit for species being checked
#'
#' @family checking functions
#' @family season data helpers
#' @family error assigning functions
#' @family dove functions
#' @family SCRG functions
#' @family waterfowl functions
#' @family woodcock functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

tooManyDaysHunted <-
  function(season_df, day_limit) {

    # Flag records with value > day_limit in days_hunted from season_df
    totals_validated <-
      season_df |>
      mutate(error2 =
               ifelse(.data$days_hunted > day_limit, "too_many_days", NA))

    message(paste(
      "Season: There are",
      nrow(season_df |> filter(.data$days_hunted > day_limit)),
      "records with >",
      day_limit,
      "days_hunted.",
      sep = " "
    ))

    return(totals_validated)
  }

#' Find records with too many days hunting cranes
#'
#' Internal function used in the \code{checkSeason} family of functions. Find
#' season records with too many days hunted for cranes.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @param crtotals_df Crane data
#'
#' @family checking functions
#' @family season data helpers
#' @family error assigning functions
#' @family crane functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

tooManyDaysHuntedCR <-
  function(crtotals_df) {

    crtotals_validated <-
      crtotals_df |>
      mutate(
        error2 =
          case_when(
            .data$sampled_state == "Alaska" &
              .data$days_hunted > REF_DAY_LIMIT_CR_AK ~
              paste(
                "too many days hunted (limit",
                REF_DAY_LIMIT_CR_AK,
                "in",
                "AK)",
                sep = " "
              ),
            .data$sampled_state %in% REF_STATES_CR_SOUTH &
              .data$days_hunted > REF_DAY_LIMIT_CR_SOUTH ~
              paste0(
                "too many days hunted (limit ",
                REF_DAY_LIMIT_CR_SOUTH,
                " in ",
                paste(REF_STATES_CR_SOUTH, collapse = ", "),
                ")"
              ),
            .data$sampled_state %in% REF_STATES_CR_NORTH &
              .data$days_hunted > REF_DAY_LIMIT_CR_NORTH ~
              paste0(
                "too many days hunted (limit ",
                REF_DAY_LIMIT_CR_NORTH,
                " in ",
                paste(REF_STATES_CR_NORTH, collapse = ", "),
                ")"
              ),
            TRUE ~ NA_character_
          )
      )

    message(
      paste(
        "Season: There are",
        nrow(crtotals_validated |> filter(!is.na(.data$error2))),
        "records exceeding the state limit for crane days_hunted.",
        sep = " "
      )
    )

    return(crtotals_validated)
  }

#' Find season did-not-hunts
#'
#' Internal function used in the \code{checkSeason} family of functions. Find
#' season records with a sum of \code{0} for \code{days_hunted} and sum greater
#' than \code{0} for \code{retrieved}.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @param season_df Season data tibble
#'
#' @family checking functions
#' @family season data helpers
#' @family error assigning functions
#' @family crane functions
#' @family dove functions
#' @family SCRG functions
#' @family waterfowl functions
#' @family woodcock functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

seasonDNH <-
  function(season_df) {

    totals_validated <-
      season_df |>
      mutate(
        sum_days_hunted = sum(.data$days_hunted),
        sum_retrieved = sum(.data$retrieved),
        .by = "surveyID"
      ) |>
      mutate(error4 =
               ifelse(
                 .data$sum_days_hunted == 0 & .data$sum_retrieved > 0,
                 "sum_days_hunted_0",
                 NA
               )) |>
      select(-c("sum_days_hunted", "sum_retrieved"))

    message(
      paste(
        "Season: There are",
        nrow(filter(totals_validated, !is.na(.data$error4))),
        "records with retrieved > 0 but sum of days hunted is 0.",
        sep = " "
      )
    )

    return(totals_validated)
  }

#' Season over bag
#'
#' Internal function used in the \code{checkSeason} family of functions. Flag
#' records with average overbags in season totals more than the overbag
#' tolerance.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @param season_df Season data tibble
#'
#' @family checking functions
#' @family season data helpers
#' @family error assigning functions
#' @family crane functions
#' @family SCRG functions
#' @family waterfowl functions
#' @family woodcock functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

seasonOverBag <-
  function(season_df) {

    # For "WF", "WK", "SCRG", "CR"
    totals_validated <-
      season_df |>
      mutate(error3 =
               ifelse(
                 ((.data$retrieved / .data$days_hunted) - .data$maxbag) >
                   REF_BAG_TOLERANCE,
                 paste0(
                   "average_bag_too_high: ",
                   round(.data$retrieved / .data$days_hunted, 1)
                 ),
                 NA
               ))

    message(
      paste(
        "Season: There are",
        nrow(filter(totals_validated, !is.na(.data$error3))),
        "records with average overbag >",
        REF_BAG_TOLERANCE,
        "over the bag limit.",
        sep = " "
      )
    )

    return(totals_validated)
  }

#' Dove season over bag
#'
#' Internal function used in the \code{checkSeason} family of functions. Flag
#' records with an average overbag in season totals for doves more than the
#' overbag tolerance.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @param dvtotals_df Season data tibble
#'
#' @family checking functions
#' @family season data helpers
#' @family error assigning functions
#' @family dove functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

seasonOverBagDV <-
  function(dvtotals_df) {

    dvtotals_validated <-
      dvtotals_df |>
      mutate(
        modowwdo =
          (.data$retrieved[.data$sp_group_estimated == "White-Winged Dove"] +
             .data$retrieved[.data$sp_group_estimated == "Mourning Dove"]) /
          (.data$days_hunted[.data$sp_group_estimated == "White-Winged Dove"] +
             .data$days_hunted[.data$sp_group_estimated == "Mourning Dove"]),
        .by = "surveyID"
      ) |>
      mutate(
        error_three =
          ifelse(
            ((.data$retrieved / .data$days_hunted) - .data$maxbag) >
              REF_BAG_TOLERANCE,
            paste0(
              "average_bag_too_high: ",
              round(.data$retrieved / .data$days_hunted, 1)
            ),
            NA
          ),
        error_four =
          ifelse(
            .data$sp_group_estimated %in%
              c("White-Winged Dove", "Mourning Dove") &
              is.na(.data$error_three) &
              .data$modowwdo > (REF_BAG_LIMIT_MODOWWDO + REF_BAG_TOLERANCE),
            paste0("modo_plus_wwdo_too_high: ", round(.data$modowwdo, 1)),
            NA
          ),
        error3 =
          ifelse(
            !is.na(.data$error_three),
            .data$error_three,
            .data$error_four
          )
      ) |>
      select(-c("modowwdo", "error_three", "error_four"))

    message(
      paste(
        "Season: There are",
        nrow(filter(
          dvtotals_validated, !is.na(.data$error3)
        )),
        "records with average overbag >",
        REF_BAG_TOLERANCE,
        "over the bag limit.",
        sep = " "
      )
    )

    return(dvtotals_validated)
  }

#' Daily over bag
#'
#' Internal function used in the \code{checkDaily} family of functions. Flag
#' daily records where daily bag is more than the overbag tolerance over limit.
#'
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @param dailies_df Daily data tibble
#' @param maxbag_df Reference data tibble
#'
#' @family checking functions
#' @family daily data helpers
#' @family error assigning functions
#' @family crane functions
#' @family dove functions
#' @family SCRG functions
#' @family waterfowl functions
#' @family woodcock functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

dailyOverBag <-
  function(dailies_df, maxbag_df) {

    dailies_validated <-
      dailies_df |>
      left_join(
        maxbag_df |>
          select(.data$sampled_state, .data$sp_group_estimated, .data$maxbag),
        by = c("sampled_state", "sp_group_estimated")
      ) |>
      mutate(error3 =
               ifelse(
                 .data$retrieved - .data$maxbag > REF_BAG_TOLERANCE,
                 "over_bag",
                 NA
               )) |>
      select(-"maxbag")

    message(
      paste(
        "Daily: There are",
        nrow(filter(dailies_validated, !is.na(.data$error3))),
        "records with daily take over the respective bag limit.",
        sep = " "
      )
    )

    return(dailies_validated)
  }

#' Daily WWDO and MODO over bag
#'
#' Internal function used in the \code{checkDaily} family of functions. Used to
#' find daily overbag for WWDO and MODO.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr row_number
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#'
#' @param dailies_df Daily data tibble
#' @param maxbag_df Reference data tibble
#'
#' @family checking functions
#' @family daily data helpers
#' @family error assigning functions
#' @family dove functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

dailyOverBagWWMO <-
  function(dailies_df, maxbag_df) {

    dv_daily_wwmo_over <-
      dailies_df |>
      filter(.data$sp_group_estimated %in%
               c("White-Winged Dove", "Mourning Dove")) |>
      mutate(row_id = row_number()) |>
      pivot_wider(
        id_cols = c(
          "surveyID",
          "selected_hunterID",
          "sampled_state",
          "harvested_date"
        ),
        names_from = "sp_group_estimated",
        values_from = "retrieved",
        # Sum values because there are multiple entries per day sometimes if a
        # hunter visits more than one county
        values_fn = sum
      ) |>
      mutate(modo = .data$`Mourning Dove`,
             wwdo = .data$`White-Winged Dove`) |>
      # Join in state abbr
      left_join(REF_STATES_AND_ABBRS, by = "sampled_state") |>
      # Add max bag col (wwdo)
      left_join(
        maxbag_df |>
          filter(.data$sp_group_estimated == "White-Winged Dove") |>
          select(.data$sampled_state, wwdo_maxbag = .data$maxbag),
        by = c("sampled_state")
      ) |>
      # Add max bag col (modo)
      left_join(
        maxbag_df |>
          filter(.data$sp_group_estimated == "Mourning Dove") |>
          select(.data$sampled_state, modo_maxbag = .data$maxbag),
        by = c("sampled_state")
      ) |>
      mutate(error2 =
               ifelse(
                 .data$modo + .data$wwdo >
                   (REF_BAG_LIMIT_MODOWWDO + REF_BAG_TOLERANCE),
                 "modowwdo_limit_exceeded",
                 NA
               )) |>
      select(-c("modo", "wwdo", "wwdo_maxbag", "modo_maxbag", "state")) |>
      filter(!is.na(.data$error2))

    dvdailies_validated <-
      dailies_df |>
      left_join(
        dv_daily_wwmo_over |>
          pivot_longer(
            .data$`Mourning Dove`:.data$`White-Winged Dove`,
            values_to = "retrieved",
            names_to = "sp_group_estimated"
          ),
        by = c(
          "surveyID",
          "selected_hunterID",
          "sampled_state",
          "sp_group_estimated",
          "harvested_date",
          "retrieved"
        )
      )

    message(
      paste(
        "Daily: There are",
        nrow(filter(
          dvdailies_validated, !is.na(.data$error2)
        )),
        "hunters with daily take of MODO + WWDO over the combined limit.",
        sep = " "
      )
    )

    return(dvdailies_validated)
  }

#' Duck and sea duck over bag
#'
#' Internal function used in the \code{checkDaily} family of functions. Used to
#' find daily overbag for ducks and sea ducks.
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr row_number
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr case_when
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#'
#' @param dailies_df Daily data tibble
#' @param maxbag_df Reference data tibble
#'
#' @family checking functions
#' @family daily data helpers
#' @family error assigning functions
#' @family waterfowl functions
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}

dailyOverBagDKSD <-
  function(dailies_df, maxbag_df) {

    wf_daily_dksd_over <-
      dailies_df |>
      filter(.data$sp_group_estimated %in%
               c("Ducks", "Specially Regulated Sea Ducks")) |>
      mutate(row_id = row_number()) |>
      pivot_wider(
        id_cols = c(
          "surveyID",
          "selected_hunterID",
          "sampled_state",
          "harvested_date"
        ),
        names_from = "sp_group_estimated",
        values_from = "retrieved",
        # Sum values because there are multiple entries per day sometimes if a
        # hunter visits more than one county
        values_fn = sum
      ) |>
      mutate(ducks = .data$Ducks,
             seaducks = .data$`Specially Regulated Sea Ducks`) |>
      # Join in state abbr
      left_join(REF_STATES_AND_ABBRS, by = "sampled_state") |>
      # Add max bag col (ducks)
      left_join(
        maxbag_df |>
          filter(.data$sp_group_estimated == "Ducks") |>
          select(.data$sampled_state, duck_maxbag = .data$maxbag),
        by = c("sampled_state")
      ) |>
      # Add max bag col (sea ducks)
      left_join(
        maxbag_df |>
          filter(.data$sp_group_estimated == "Specially Regulated Sea Ducks") |>
          select(.data$sampled_state, sd_maxbag = .data$maxbag),
        by = c("sampled_state")
      ) |>
      mutate(
        error2 =
          case_when(
            # AK ducks
            .data$state == "AK" & .data$ducks >
              (.data$duck_maxbag + REF_BAG_TOLERANCE) ~
              "duck_limit_exceeded",
            # AK seaducks
            .data$state == "AK" & .data$seaducks >
              (.data$sd_maxbag + REF_BAG_TOLERANCE) ~
              "seaduck_limit_exceeded",
            # AK ducks + seaducks
            .data$state == "AK" & .data$ducks + .data$seaducks >
              (
                .data$duck_maxbag + REF_BAG_TOLERANCE + .data$sd_maxbag +
                  REF_BAG_TOLERANCE
              ) ~
              "duck_plus_seaduck_limit_exceeded",
            # Check PF states: See if duck+seaduck exceeds 9 (combined daily
            # limit is 7 ducks and/or seaducks)
            .data$state %in% c("CA", "OR", "WA") &
              .data$ducks + .data$seaducks >
              (.data$duck_maxbag + REF_BAG_TOLERANCE) ~
              "duck_plus_seaduck_limit_exceeded",
            # Check AF states: duck limit is 6 OR 11 and SD limit is 4
            # AF ducks + seaducks
            .data$state %in% REF_STATES_SD_AF &
              .data$ducks + .data$seaducks >
              (.data$duck_maxbag + REF_BAG_TOLERANCE) ~
              "duck_plus_seaduck_limit_exceeded",
            # AF ducks
            .data$state %in% REF_STATES_SD_AF &
              .data$ducks > (.data$duck_maxbag + REF_BAG_TOLERANCE) ~
              "duck_limit_exceeded",
            # AF seaducks
            .data$state %in% REF_STATES_SD_AF &
              .data$seaducks > (.data$sd_maxbag + REF_BAG_TOLERANCE) ~
              "seaduck_limit_exceeded",
            # Ducks only
            .data$ducks > (.data$duck_maxbag + REF_BAG_TOLERANCE) ~
              "duck_limit_exceeded",
            TRUE ~ NA_character_
          )
      ) |>
      select(-c("seaducks", "ducks", "sd_maxbag", "duck_maxbag", "state")) |>
      filter(!is.na(.data$error2))

    wfdailies_validated <-
      dailies_df |>
      left_join(
        wf_daily_dksd_over |>
          pivot_longer(
            .data$Ducks:.data$`Specially Regulated Sea Ducks`,
            values_to = "retrieved",
            names_to = "sp_group_estimated"
          ),
        by = c(
          "surveyID",
          "selected_hunterID",
          "sampled_state",
          "sp_group_estimated",
          "harvested_date",
          "retrieved"
        )
      )

    message(
      paste(
        "Daily: There are",
        nrow(filter(
          wfdailies_validated, !is.na(.data$error2)
        )),
        "hunters with daily take of duck + sea duck over the respective",
        "state limit.",
        sep = " "
      )
    )

    return(wfdailies_validated)
  }
