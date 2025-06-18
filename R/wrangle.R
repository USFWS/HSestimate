#' Wrangle reference data
#'
#' Internal function used in \code{\link{wrangleMaxBag}} and \code{\link{wrangleDates}}, which does initial data cleaning. 
#' 
#' @importFrom dplyr rename_all
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom rlang .data
#'
#' @param ref_data Reference data
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/HSestimate}

wrangleRef <-
  function(ref_data) {
    
    ref_data |> 
      rename_all(~tolower(.)) |> 
      # Filter out Puerto Rico, Hawaii, and swans; use all season dates and lims
      filter(!.data$st %in% c("PR", "HI") & 
               !str_detect(.data$speciesgroup, "Swan")) |> 
      mutate(
        # Combine duck and merganser bag limit for certain states
        bag = 
          ifelse(
            .data$speciesgroup == "Ducks" & 
              .data$st %in% REF_STATES_DUCKMERG & 
              .data$species %in% c("Ducks", "Mergansers", 
                                   "Ducks and mergansers") & 
              !is.na(.data$bag),
            REF_BAG_LIMIT_DUCKMERG,
            .data$bag),
        # Combine duck and merganser possession limit for certain states
        possession = 
          ifelse(
            .data$speciesgroup == "Ducks" & 
              .data$st %in% REF_STATES_DUCKMERG & 
              .data$species %in% c("Ducks", "Mergansers", 
                                   "Ducks and mergansers") & 
              !is.na(.data$possession),
            REF_BAG_LIMIT_DUCKMERG*3,
            .data$possession),
        # Change Brant speciesgroup to "Brant" instead of "Goose"; change 
        # MODO-WWDO and MODO-WWDO-WTDO speciesgroup field to "MODO-WWDO" instead
        # of "Doves"
        speciesgroup = 
          case_when(
            .data$species == "Brant" ~ "Brant",
            str_detect(.data$species, "MODO-WWDO") ~ "MODO-WWDO",
            TRUE ~ .data$speciesgroup),
        # Create a spp field which makes each line more specific, e.g. NM
        # "COMO-PUGA" speciesgroup is for Gallinules, whereas "AMCO-COMO" is for
        # Coots; in addition, the "AMCO-COMO" category implies CootsGallinules
        # or just Coots depending on the state
        spp = 
          case_when(
            str_detect(.data$speciesgroup, "Crane") ~ "Sandhill Crane",
            .data$speciesgroup == "Brant" ~ "Brant",
            .data$speciesgroup == "Geese" ~ "Geese",
            .data$speciesgroup == "Ducks" ~ "Ducks",
            .data$speciesgroup == "AMWO" ~ "Woodcock",
            .data$speciesgroup == "COSN" ~ "Snipe",
            .data$speciesgroup == "BTPI" ~ "Band-tailed Pigeon",
            .data$speciesgroup == "Rails" ~ "Rails",
            .data$speciesgroup == "COMO-PUGA" ~ "Gallinules",
            .data$speciesgroup == "MODO-WWDO" ~ "MODO-WWDO",
            # For NM "AMCO-COMO", set as "Coots" (they have a separate
            # speciesgroup for "COMO-PUGA" that becomes "Gallinules", above)
            .data$speciesgroup == "AMCO-COMO" & 
              .data$st == "NM" ~ "Coots", 
            # For AZ, CA, MN, and NV: the "AMCO-COMO" category should apply to
            # "Coots" AND "Gallinules"
            .data$speciesgroup == "AMCO-COMO" & 
              .data$st %in% c("AZ", "CA", "MN", "NV") ~ 
              "CootsGallinules", 
            # For CO, ID, MT, OR, UT, WA, WY: the "AMCO-COMO" category should 
            # apply to "Coots" only
            .data$speciesgroup == "AMCO-COMO" & 
              .data$st %in% c("CO", "ID", "MT", "OR", "UT", "WA", "WY") ~ 
              "Coots", 
            .data$speciesgroup == "Coots" ~ "Coots",
            TRUE ~ NA_character_)
      ) 
  }

#' Wrangle max bag
#'
#' Wrangle the raw max bag reference table.
#' 
#' @importFrom dplyr filter
#' @importFrom dplyr distinct
#' @importFrom dplyr summarize
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr tibble
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr bind_rows
#' @importFrom rlang .data
#'
#' @param ref_data Reference data
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/HSestimate}
#' 
#' @export

wrangleMaxBag <-
  function(ref_data) {
    
    # Combine duck and merg bag/possession limits
    ref_return <- wrangleRef(ref_data)
    
    # Create a summarized table of bag limits per state and species
    maxbag_wrangled <-
      ref_return |>
      filter(!is.na(.data$spp) & !is.na(.data$bag)) |> 
      distinct(
        .data$seasonyear, state = .data$st, .data$speciesgroup, .data$spp, 
        .data$bag, .data$possession) |> 
      # Set maxbag and maxposs using the maximum bag limit per state and spp
      summarize(
        maxbag = max(.data$bag),
        maxposs = max(.data$possession),
        .by = c("seasonyear", "state", "spp")) |> 
      # Join in state names
      left_join(REF_STATES_AND_ABBRS, by = "state") |> 
      select(-c("state", "seasonyear")) |>
      rename(sp_group_estimated = spp)
    
    # Create a seaduck max bag table
    sd_maxbag <-
      tibble(
        sp_group_estimated = 
          rep(times = length(REF_STATES_SD), "Specially Regulated Sea Ducks"),
        maxbag = NA, 
        maxposs = NA,
        state = REF_STATES_SD) |> 
      # Set seaduck limits of 10 for AK; 7 for CA, OR, WA; 4 for AF states
      mutate(
        maxbag = 
          case_when(
            .data$state %in% REF_STATES_SD[!REF_STATES_SD %in% c("AK", "CA", "OR", "WA")] ~ 
              REF_BAG_LIMIT_SD_AF,
            .data$state %in% c("CA", "OR", "WA") ~ REF_BAG_LIMIT_SD_PF,
            .data$state == "AK" ~ REF_BAG_LIMIT_SD_AK,
            TRUE ~ .data$maxbag),
        maxposs = .data$maxbag*3) |> 
      # Join in state names
      left_join(REF_STATES_AND_ABBRS, by = "state") |> 
      select(-c("state"))
    
    # Duplicate the "Doves" lines so they apply to MODO and WWDO
    # Duplicate the "GeeseBrant" lines so they apply to Geese and Brant
    # Duplicate the "CootsGallinules" lines so they apply to Coots and 
    # Gallinules
    maxbag <-
      maxbag_wrangled |> 
      filter(.data$sp_group_estimated == "MODO-WWDO") |> 
      mutate(sp_group_estimated = "Mourning Dove") |> 
      bind_rows(
        maxbag_wrangled |> 
          filter(.data$sp_group_estimated == "MODO-WWDO") |> 
          mutate(sp_group_estimated = "White-Winged Dove")) |> 
      bind_rows(
        maxbag_wrangled |> 
          filter(.data$sp_group_estimated == "GeeseBrant") |> 
          mutate(sp_group_estimated = "Geese")) |> 
      bind_rows(
        maxbag_wrangled |> 
          filter(.data$sp_group_estimated == "GeeseBrant") |> 
          mutate(sp_group_estimated = "Brant")) |> 
      bind_rows(
        maxbag_wrangled |> 
          filter(.data$sp_group_estimated == "CootsGallinules") |> 
          mutate(sp_group_estimated = "Coots")) |> 
      bind_rows(
        maxbag_wrangled |> 
          filter(.data$sp_group_estimated == "CootsGallinules") |> 
          mutate(sp_group_estimated = "Gallinules")) |> 
      bind_rows(
        maxbag_wrangled |> 
          filter(
            !.data$sp_group_estimated %in% 
              c("CootsGallinules", "MODO-WWDO", "GeeseBrant"))) |> 
      bind_rows(sd_maxbag) |> 
      left_join(
        state_numbers |> 
          rename(
            sampled_state = .data$description,
            state = .data$abbreviation,
            stateno = .data$state), 
        by = "sampled_state")
    
    return(maxbag)
  }

#' Wrangle max bag
#'
#' Wrangle the raw max bag reference table.
#' 
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr summarize
#' @importFrom dplyr left_join
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @importFrom lubridate ymd
#' @importFrom rlang .data
#'
#' @param ref_data Reference data
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/HSestimate}
#' 
#' @export

wrangleDates <-
  function(ref_data) {
    
    # Start dates by flyway
    dates_wrangled <- 
      wrangleRef(ref_data) |> 
      select(
        .data$seasonyear, state = .data$st, .data$speciesgroup, .data$open, 
        .data$close, .data$spp) |>
      filter(!is.na(.data$spp) & !is.na(.data$open) & !is.na(.data$close)) |> 
      summarize(
        open = min(ymd(.data$open), na.rm = T),
        close = max(ymd(.data$close), na.rm = T),
        .by = c("seasonyear", "state", "spp")) |>
      left_join(REF_STATES_AND_ABBRS, by = "state") |> 
      select(-c("state", "seasonyear")) |> 
      rename(sp_group_estimated = .data$spp) |> 
      # Calculate season length in days
      mutate(season_length = as.numeric(.data$close - .data$open)) 
    
    # Duplicate the "Doves" lines so they apply to MODO and WWDO
    # Duplicate the "GeeseBrant" lines so they apply to Geese and Brant
    # Duplicate the "CootsGallinules" lines so they apply to Coots and 
    # Gallinules
    dates <-
      dates_wrangled |> 
      filter(.data$sp_group_estimated == "MODO-WWDO") |> 
      mutate(sp_group_estimated = "Mourning Dove") |> 
      bind_rows(
        dates_wrangled |> 
          filter(.data$sp_group_estimated == "MODO-WWDO") |> 
          mutate(sp_group_estimated = "White-Winged Dove")) |> 
      bind_rows(
        dates_wrangled |> 
          filter(.data$sp_group_estimated == "CootsGallinules") |> 
          mutate(sp_group_estimated = "Coots")) |> 
      bind_rows(
        dates_wrangled |> 
          filter(.data$sp_group_estimated == "CootsGallinules") |> 
          mutate(sp_group_estimated = "Gallinules")) |> 
      bind_rows(
        dates_wrangled |> 
          filter(!.data$sp_group_estimated %in% 
                   c("CootsGallinules", "MODO-WWDO"))) |> 
      summarize(
        earliest_open = min(.data$open),
        latest_close = max(.data$close),
        .by = "sampled_state") |> 
      left_join(
        state_numbers |> 
          rename(
            sampled_state = .data$description,
            SampleST = .data$abbreviation,
            stateno = .data$state), 
        by = "sampled_state")
    
    return(dates)
  }
