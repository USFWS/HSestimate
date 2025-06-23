#' Convert sea ducks to ducks and brant to geese
#'
#' Internal function used in \code{\link{editCheck}}. 
#' 
#' @param daily_corrected Daily data corrected by \code{\link{dailyCorrect}}
#' @param season_precorrected Season data pre-corrected by \code{\link{seasonCorrect}}
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/HSestimate}

convertSDBR <-
  function(daily_corrected, season_precorrected) {
    sd_converted <- convertSeaDuckToDuck(daily_corrected, season_precorrected)
    sdbr_converted <- convertBrantToGeese(daily_corrected, sd_converted)
    
    return(sdbr_converted)
  }

#' Convert sea ducks to ducks for sea duck records from non-sea duck states
#'
#' Internal function used in \code{\link{editCheck}}. 
#' 
#' @importFrom dplyr tibble
#' @importFrom dplyr left_join
#' @importFrom dplyr pull
#' @importFrom dplyr filter
#' @importFrom dplyr summarize
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom rlang .data
#' 
#' @param dailies_df Daily data tibble
#' @param season_df Season data tibble
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/HSestimate}

convertSeaDuckToDuck <-
  function(dailies_df, season_df) {
    
    # Return a message that we are temporarily excluding CA, OR, and WA from the
    # evaluation here
    message(
      paste(
        "[Important]: CA, OR, and WA are excluded from the list of SD & BR",
        "states; this is because their counties have not yet been evaluated",
        "and the is_SeaDuck col is not reliable yet.", sep = " "))
    
    # Define sea duck states
    sd_state_names <-
      tibble(state = REF_STATES_SD[!REF_STATES_SD %in% c("CA", "WA", "OR")]) |> 
      left_join(REF_STATES_AND_ABBRS, by = "state") |> 
      pull(.data$sampled_state)
    
    # Select sea duck records in non-seaduck counties
    nonsdtots <- 
      dailies_df |> 
      filter(
        # Use the is_SeaDuck col to filter to non-seaduck counties
        .data$is_SeaDuck == "N" & 
          .data$sp_group_estimated == "Specially Regulated Sea Ducks" &
          .data$retrieved > 0 & 
          .data$sampled_state %in% sd_state_names)
    
    # Turn these records into ducks and add to totals file
    if (nrow(nonsdtots) != 0) {
      
      # Summarize number of seaducks retrieved by surveyID
      sdbysurveyID <- 
        nonsdtots |> 
        summarize(sum_retrieved = sum(.data$retrieved), .by = "surveyID")
      
      season_df_orig <- season_df
      
      # Add the number of seaducks harvested to the total number of ducks
      # harvested in the season totals for each surveyID
      for (i in 1:nrow(sdbysurveyID))  {
        season_df <-
          season_df |> 
          mutate(
            retrieved = 
              ifelse(
                .data$surveyID == sdbysurveyID$surveyID[i] & 
                  .data$sp_group_estimated == "Ducks",
                .data$retrieved + sdbysurveyID$sum_retrieved[i],
                .data$retrieved)
          )
      }
      
      # Create a df to double check that the addition was conducted correctly
      # for each surveyID
      validate <-
        sdbysurveyID |> 
        rename(sum_bad_seaducks_retrieved = .data$sum_retrieved) |> 
        left_join(
          season_df_orig |> 
            filter(.data$surveyID %in% sdbysurveyID$surveyID &
                     .data$sp_group_estimated == "Ducks") |> 
            select(.data$surveyID, original_ducks_retrieved = .data$retrieved),
          by = "surveyID") |> 
        left_join(
          season_df |> 
            filter(.data$surveyID %in% sdbysurveyID$surveyID &
                     .data$sp_group_estimated == "Ducks") |> 
            select(.data$surveyID, new_ducks_retrieved = .data$retrieved),
          by = "surveyID") |> 
        mutate(
          check = 
            .data$original_ducks_retrieved + 
            .data$sum_bad_seaducks_retrieved) |> 
        filter(.data$check != .data$new_ducks_retrieved)
      
      if(nrow(validate) != 0) {
        message("Error in adding sea duck harvest to duck sums.")
      } else {
        message("Sea duck harvest correctly added to ducks.")
      }
      
      return(season_df)
    } else {
      message(
        paste0(
          "Daily data does not contain any seaducks harvested in non-seaduck ",
          "states."))
    }
  }

#' Convert brant to geese for brant records from non-brant states
#'
#' Internal function used in \code{\link{editCheck}}. 
#' 
#' @importFrom dplyr tibble
#' @importFrom dplyr left_join
#' @importFrom dplyr pull
#' @importFrom dplyr filter
#' @importFrom dplyr summarize
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom rlang .data
#' 
#' @param dailies_df Daily data tibble
#' @param season_df Season data tibble
#' 
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/HSestimate}

convertBrantToGeese <-
  function(dailies_df, season_df) {
    
    # Return a message that we are temporarily excluding CA, OR, and WA from the
    # evaluation here
    message(
      paste(
        "[Important]: CA, OR, and WA are excluded from the list of BR states;",
        "this is because their counties have not yet been evaluated and the",
        "is_Brant col is not reliable yet.", sep = " "))
    
    # Define brant states
    br_state_names <-
      tibble(state = REF_STATES_BR[!REF_STATES_BR %in% c("CA", "WA", "OR")]) |> 
      left_join(REF_STATES_AND_ABBRS, by = "state") |> 
      pull(.data$sampled_state)
    
    # Select brant records in non-brant counties
    nonbrtots <- 
      dailies_df |> 
      filter(
        # Use the is_Brant col to filter to non-Brant counties
        .data$is_Brant == "N" & 
          .data$sp_group_estimated == "Brant" & 
          .data$retrieved > 0 & 
          .data$sampled_state %in% br_state_names)
    
    # Turn these records into geese and add to totals file
    if (nrow(nonbrtots) != 0) {
      
      # Summarize number of brant retrieved by surveyID
      brantbysurveyID <- 
        nonbrtots |> 
        summarize(sum_retrieved = sum(.data$retrieved), by = "surveyID")
      
      season_df_orig <- season_df
      
      # Add the number of brant harvested to the total number of geese
      # harvested in the season totals for each surveyID
      for (i in 1:nrow(brantbysurveyID))  {
        
        season_df <-
          season_df |> 
          mutate(
            retrieved = 
              ifelse(
                .data$surveyID == brantbysurveyID$surveyID[i] & 
                  .data$sp_group_estimated == "Geese",
                .data$retrieved + brantbysurveyID$sum_retrieved[i],
                .data$retrieved)
          )
      }
      
      # Create a df to double check that the addition was conducted correctly
      # for each surveyID
      validate <-
        brantbysurveyID |> 
        rename(sum_bad_brant_retrieved = .data$sum_retrieved) |> 
        left_join(
          season_df_orig |> 
            filter(.data$surveyID %in% brantbysurveyID$surveyID &
                     .data$sp_group_estimated == "Geese") |> 
            select(.data$surveyID, original_geese_retrieved = .data$retrieved),
          by = "surveyID") |> 
        left_join(
          season_df |> 
            filter(.data$surveyID %in% brantbysurveyID$surveyID &
                     .data$sp_group_estimated == "Geese") |> 
            select(.data$surveyID, new_geese_retrieved = .data$retrieved),
          by = "surveyID") |> 
        mutate(
          check = 
            .data$original_geese_retrieved + .data$sum_bad_brant_retrieved) |> 
        filter(.data$check != .data$new_geese_retrieved)
      
      if(nrow(validate) != 0) {
        message("Error in adding brant harvest to geese sums.")
      } else {
        message("Brant harvest correctly added to geese.")
      }
      
      return(season_df)
    } else {
      message(
        paste0(
          "Daily data does not contain any brant harvested in non-brant ",
          "states."))
    }
  }

#' Find season or daily WWDO harvest effort in non-WWDO states and convert to zero
#'
#' Internal function used in \code{\link{checkSeason}} and \code{\link{checkDaily}}. Find any harvest of WWDO in non-wwdo states and WWDO in edge states that exceed the designated limit; for these cases, change WWDO days_hunted, retrieved, and unretrieved values to 0.
#' 
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr distinct
#' @importFrom stringr str_to_title
#' @importFrom rlang .data
#' 
#' @param data_df Season or daily data tibble
#' @param type "season" or "daily"
#' @param summary Whether a summary of WWDO errors should be returned; TRUE or FALSE
#' 
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/HSestimate}

convertWWDO <-
  function(data_df, type, summary = F) {
    
    stopifnot(
      "`type` must be 'season' or 'daily'." = type %in% c("season", "daily"))
    
    stopifnot(
      "`summary` must be TRUE or FALSE." = summary %in% c(T, F, TRUE, FALSE))
    
    errors_df <-
      data_df |>
      left_join(REF_STATES_WWDO_DF |> select(-"state"), by = "sampled_state") |>
      mutate(
        wwdo_error =
          case_when(
            # SEASON: flag WWDO records from non-WWDO states when days_hunted,
            # retrieved, or unretrieved is > 0
            type == "season" &
              .data$sp_group_estimated == "White-Winged Dove" &
              .data$wwdo_state_status == "none" &
              (.data$days_hunted > 0 | 
                 .data$retrieved > 0 | 
                 .data$unretrieved > 0) ~
              paste("non-WWDO state reported value(s) > 0 for days_hunted,",
                    "retrieved, and/or unretrieved"),
            # DAILY: flag WWDO records from non-WWDO states when days_hunted,
            # retrieved, or unretrieved is > 0
            type == "daily" &
              .data$sp_group_estimated == "White-Winged Dove" &
              .data$wwdo_state_status == "none" &
              (.data$retrieved > 0 | .data$unretrieved > 0) ~
              paste("non-WWDO state reported value(s) > 0 for retrieved and/or",
                    "unretrieved"),
            # Flag WWDO records from edge WWDO states with retrieved +
            # unretrieved > edge state limit
            .data$sp_group_estimated == "White-Winged Dove" &
              .data$wwdo_state_status == "edge" &
              (.data$retrieved + .data$unretrieved) > REF_BAG_LIMIT_WWDO_EDGE ~
              paste("edge WWDO state reported >", REF_BAG_LIMIT_WWDO_EDGE,
                    "for retrieved and/or unretrieved"),
            TRUE ~ NA_character_))
    
    if (type == "season") {
      wwdo_validated <-
        errors_df |> 
        # If non-WWDO state reported days_hunted, retrieved, and/or unretrieved
        # > 0, OR if edge WWDO state reported retrieved + unretrieved > edge
        # limit, change days_hunted, retrieved, and unretrieved to 0
        mutate(
          days_hunted = ifelse(!is.na(.data$wwdo_error), 0, .data$days_hunted),
          retrieved = ifelse(!is.na(.data$wwdo_error), 0, .data$retrieved),
          unretrieved = ifelse(!is.na(.data$wwdo_error), 0, .data$unretrieved)
        ) |> 
        select(-"wwdo_state_status")
      
    } else if(type == "daily") {
      wwdo_validated <-
        errors_df |> 
        # If non-WWDO state reported retrieved and/or unretrieved > 0, OR if
        # edge WWDO state reported retrieved + unretrieved > edge limit, change
        # retrieved and unretrieved to 0
        mutate(
          retrieved = ifelse(!is.na(.data$wwdo_error), 0, .data$retrieved),
          unretrieved = ifelse(!is.na(.data$wwdo_error), 0, .data$unretrieved)
        ) |> 
        select(-"wwdo_state_status")
    }
    
    # Create a tibble of the records with WWDO error
    errors_df_sm <- errors_df |> filter(!is.na(.data$wwdo_error))
    
    # Count the number of states involved in record changes
    n_states <- errors_df_sm |> distinct(.data$sampled_state) |> nrow()
    
    message(
      paste(
        paste0(str_to_title(type), ":"), "A total of", nrow(errors_df_sm), 
        "records from", n_states, "states had their WWDO days_hunted,",
        "retrieved, and unretrieved values changed to 0."))
    
    # Summarize errors
    if (summary == TRUE) {
      error_summary <- summarizeWWDO(errors_df, type = type)
      print(error_summary, n = nrow(error_summary))
    }
    return(wwdo_validated)
  }

#' Summarize WWDO errors
#'
#' Internal function used in \code{\link{convertWWDO}}.
#' 
#' @importFrom dplyr filter
#' @importFrom dplyr summarize
#' @importFrom dplyr n
#' @importFrom dplyr arrange
#' @importFrom rlang .data
#' 
#' @param errors_df Season or daily data tibble
#' @param type "season" or "daily"
#' 
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/HSestimate}

summarizeWWDO <-
  function(errors_df, type) {
    stopifnot(
      "`type` must be 'season' or 'daily'." = type %in% c("season", "daily"))
    
    if (type == "season") {
      errors_df |>
        filter(!is.na(.data$wwdo_error)) |>
        #group_by(sampled_state, wwdo_error) |>
        summarize(
          max_days_hunted = max(.data$days_hunted, na.rm = T),
          max_retrieved = max(.data$retrieved, na.rm = T),
          max_unretrieved = max(.data$unretrieved, na.rm = T),
          n = n(),
          .by = c("sampled_state", "wwdo_error"),
          .groups = "drop"
        ) |>
        arrange(.data$wwdo_error)
      
    } else if (type == "daily") {
      errors_df |>
        filter(!is.na(.data$wwdo_error)) |>
        #group_by(sampled_state, wwdo_error) |>
        # Due to smaller sample sizes, make sure the vector supplied to max()
        # does not contain only NAs; this ifelse avoids annoying warning and
        # -Inf result
        summarize(
          max_retrieved = 
            ifelse(
              !all(is.na(.data$retrieved)), 
              max(.data$retrieved, na.rm = T), 
              NA),
          max_unretrieved = 
            ifelse(
              !all(is.na(.data$unretrieved)), 
              max(.data$unretrieved, na.rm = T), 
              NA),
          n = n(),
          .by = c("sampled_state", "wwdo_error"),
          .groups = "drop"
          ) |>
        arrange(.data$wwdo_error)
    }
    
  }
