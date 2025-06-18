#' Season over bag
#'
#' Internal function used in \code{\link{checkSeason}}. 
#' 
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @param season_df Season data tibble
#' @param species Species abbreviation, may be one of: 'WF', 'DV', 'SCRG', 'WK', or 'CR'.
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/HSestimate}

# Internal function: Flag records with average overbags in season totals >
# overbag tolerance over the bag limit for the season
seasonOverBag <-
  function(season_df, species) {
    failspp(species)
    
    if(species == "DV") {
      totals_validated <-
        season_df |> 
        mutate(
          modowwdo = 
            (.data$retrieved[.data$sp_group_estimated == "White-Winged Dove"] + 
               .data$retrieved[.data$sp_group_estimated == "Mourning Dove"])/
            (.data$days_hunted[.data$sp_group_estimated == "White-Winged Dove"] + 
               .data$days_hunted[.data$sp_group_estimated == "Mourning Dove"]),
          .by = "surveyID") |> 
        mutate(
          error_three = 
            ifelse(
              ((.data$retrieved/.data$days_hunted) - .data$maxbag) > 
                REF_BAG_TOLERANCE,
              paste0("average_bag_too_high: ", 
                     round(.data$retrieved/.data$days_hunted, 1)), 
              NA),
          error_four = 
            ifelse(
              .data$sp_group_estimated %in% 
                c("White-Winged Dove", "Mourning Dove") &
                is.na(.data$error_three) &
                .data$modowwdo > (REF_BAG_LIMIT_MODOWWDO + REF_BAG_TOLERANCE), 
              paste0("modo_plus_wwdo_too_high: ", round(.data$modowwdo, 1)),
              NA),
          error3 = 
            ifelse(
              !is.na(.data$error_three), .data$error_three, .data$error_four)
        ) |> 
        select(-c("modowwdo", "error_three", "error_four"))
      
    } else if (species %in% c("WF", "WK", "SCRG", "CR")) {
      totals_validated <-
        season_df |> 
        mutate(
          error3 = 
            ifelse(
              ((.data$retrieved/.data$days_hunted) - .data$maxbag) > 
                REF_BAG_TOLERANCE, 
              paste0("average_bag_too_high: ", 
                     round(.data$retrieved/.data$days_hunted, 1)), 
              NA)) 
    }
    
    message(
      paste(
        "Season: There are", 
        nrow(filter(totals_validated, !is.na(.data$error3))), 
        "records with average overbag >", REF_BAG_TOLERANCE,
        "over the bag limit.", sep = " ")
    )
    return(totals_validated)
  }

#' Daily over bag
#'
#' Internal function used in \code{\link{checkDaily}}. 
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
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/HSestimate}

# Flag daily records where daily bag is > REF_BAG_TOLERANCE over limit
dailyOverBag <-
  function(dailies_df, maxbag_df) {
    dailies_validated <-
      dailies_df |>
      left_join(
        maxbag_df |> 
          select(.data$sampled_state, .data$sp_group_estimated, .data$maxbag),
        by = c("sampled_state", "sp_group_estimated")
      ) |>
      mutate(
        error3 = 
          ifelse(
            .data$retrieved - .data$maxbag > REF_BAG_TOLERANCE,
            "over_bag",
            NA)) |> 
      select(-"maxbag")
    
    message(
      paste(
        "Daily: There are", 
        nrow(filter(dailies_validated, !is.na(.data$error3))), 
        "records with daily take over the respective bag limit.", sep = " ")
    )
    return(dailies_validated)
  }

#' WWDO and MODO over bag
#'
#' Internal function used in \code{\link{checkDaily}}. 
#' 
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr row_number
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom rlang .data
#'
#' @param dailies_df Daily data tibble
#' @param maxbag_df Reference data tibble
#' 
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/HSestimate}

dailyOverBagWWMO <-
  function(dailies_df, maxbag_df) {
    
    dv_daily_wwmo_over <-
      dailies_df |>
      filter(
        .data$sp_group_estimated %in% 
          c("White-Winged Dove", "Mourning Dove")) |>
      mutate(row_id = row_number()) |>
      pivot_wider(
        id_cols = c("surveyID", "selected_hunterID", "sampled_state",
                    "harvested_date"),
        names_from = "sp_group_estimated",
        values_from = "retrieved",
        # Sum values because there are multiple entries per day sometimes if a
        # hunter visits more than one county
        values_fn = sum) |>
      mutate(modo = .data$`Mourning Dove`, wwdo = .data$`White-Winged Dove`) |>
      # Join in state abbr
      left_join(REF_STATES_AND_ABBRS, by = "sampled_state") |>
      # Add max bag col (wwdo)
      left_join(
        maxbag_df |>
          filter(.data$sp_group_estimated == "White-Winged Dove") |>
          select(.data$sampled_state, wwdo_maxbag = .data$maxbag),
        by = c("sampled_state")) |>
      # Add max bag col (modo)
      left_join(
        maxbag_df |>
          filter(.data$sp_group_estimated == "Mourning Dove") |>
          select(.data$sampled_state, modo_maxbag = .data$maxbag),
        by = c("sampled_state")) |>
      mutate(
        error2 = 
          ifelse(
            .data$modo + .data$wwdo > 
              (REF_BAG_LIMIT_MODOWWDO + REF_BAG_TOLERANCE), 
            "modowwdo_limit_exceeded", 
            NA)
      ) |>
      select(-c("modo", "wwdo", "wwdo_maxbag", "modo_maxbag", "state")) |>
      filter(!is.na(.data$error2))
    
    dvdailies_validated <-
      dailies_df |>
      left_join(
        dv_daily_wwmo_over |>
          pivot_longer(
            .data$`Mourning Dove`:.data$`White-Winged Dove`,
            values_to = "retrieved",
            names_to = "sp_group_estimated"),
        by = c("surveyID", "selected_hunterID", "sampled_state",
               "sp_group_estimated", "harvested_date", "retrieved")
      )
    
    message(
      paste(
        "Daily: There are", 
        nrow(filter(dvdailies_validated, !is.na(.data$error2))),
        "hunters with daily take of MODO + WWDO over the combined limit.", 
        sep = " ")
    )
    return(dvdailies_validated)
  }

#' Duck and sea duck over bag
#'
#' Internal function used in \code{\link{checkDaily}}. 
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
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/HSestimate}

dailyOverBagDKSD <-
  function(dailies_df, maxbag_df) {
    
    wf_daily_dksd_over <-
      dailies_df |> 
      filter(
        .data$sp_group_estimated %in% c("Ducks", "Specially Regulated Sea Ducks")) |> 
      mutate(row_id = row_number()) |> 
      pivot_wider(
        id_cols = c("surveyID", "selected_hunterID", "sampled_state", 
                    "harvested_date"),
        names_from = "sp_group_estimated",
        values_from = "retrieved",
        # Sum values because there are multiple entries per day sometimes if a
        # hunter visits more than one county
        values_fn = sum) |> 
      mutate(
        ducks = .data$Ducks, 
        seaducks = .data$`Specially Regulated Sea Ducks`) |> 
      # Join in state abbr
      left_join(REF_STATES_AND_ABBRS, by = "sampled_state") |> 
      # Add max bag col (ducks)
      left_join(
        maxbag_df |> 
          filter(.data$sp_group_estimated == "Ducks") |> 
          select(.data$sampled_state, duck_maxbag = .data$maxbag),
        by = c("sampled_state")) |> 
      # Add max bag col (sea ducks)
      left_join(
        maxbag_df |> 
          filter(.data$sp_group_estimated == "Specially Regulated Sea Ducks") |> 
          select(.data$sampled_state, sd_maxbag = .data$maxbag),
        by = c("sampled_state")) |> 
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
              (.data$duck_maxbag + REF_BAG_TOLERANCE + .data$sd_maxbag + 
                 REF_BAG_TOLERANCE)~ 
              "duck_plus_seaduck_limit_exceeded",
            # Check PF states: See if duck+seaduck exceeds 9 (combined daily
            # limit is 7 ducks and/or seaducks)
            .data$state %in% c("CA", "OR", "WA") & 
              .data$ducks + .data$seaducks > 
              (.data$duck_maxbag + REF_BAG_TOLERANCE) ~ 
              "duck_plus_seaduck_limit_exceeded",
            # Check AF states: duck limit is 6 OR 11 and SD limit is 4
            # AF ducks + seaducks 
            .data$state %in% REF_STATES_AF &
              .data$ducks + .data$seaducks > 
              (.data$duck_maxbag + REF_BAG_TOLERANCE) ~ 
              "duck_plus_seaduck_limit_exceeded",
            # AF ducks
            .data$state %in% REF_STATES_AF &
              .data$ducks > (.data$duck_maxbag + REF_BAG_TOLERANCE) ~ 
              "duck_limit_exceeded",
            # AF seaducks
            .data$state %in% REF_STATES_AF &
              .data$seaducks > (.data$sd_maxbag + REF_BAG_TOLERANCE) ~ 
              "seaduck_limit_exceeded",
            # Ducks only
            .data$ducks > (.data$duck_maxbag + REF_BAG_TOLERANCE) ~ 
              "duck_limit_exceeded",
            TRUE ~ NA_character_)
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
            names_to = "sp_group_estimated"),
        by = c("surveyID", "selected_hunterID", "sampled_state", 
               "sp_group_estimated", "harvested_date", "retrieved")
      )
    
    message(
      paste(
        "Daily: There are", 
        nrow(filter(wfdailies_validated, !is.na(.data$error2))), 
        "hunters with daily take of duck + sea duck over the respective", 
        "state limit.", sep = " ")
    )
    return(wfdailies_validated)
  }