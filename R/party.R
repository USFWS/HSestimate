#' Party hunt finder
#'
#' Parse comment strings to check if a daily record was a party hunt and calculate revised retrieved field if value exceeds max bag.
#'
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr rename
#' @importFrom dplyr relocate
#' @importFrom rlang .data
#'
#' @param dailies_df Daily data tibble
#' @param maxbag_df Reference data tibble
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/HSestimate}
#' 
#' @export

partyHuntFinder <- 
  function(dailies_df, maxbag_df) {
    
    parties <-
      dailies_df |> 
      # Pull out obvious group sizes from strings and put them in a new
      # 'party_size' field
      getPartySize() |> 
      # Add max bag field
      left_join(
        maxbag_df |> 
          select(.data$sampled_state, .data$sp_group_estimated, .data$maxbag),
        by = c("sampled_state", "sp_group_estimated")) |> 
      # Edit the retrieved value to be divided by the party size for records
      # that are a party hunt and the retrieved bag is greater than the max
      # daily bag
      recalcPartyBag()
    
    message(
      paste(
        "Daily: There are", nrow(filter(parties, !is.na(.data$party_size))), 
        "parties. A total of", nrow(filter(parties, !is.na(.data$error1))), 
        "bag values were recalculated.", sep = " "))
    
    dailies_validated <-
      parties |> 
      select(-c("party_size", "maxbag", "retrieved")) |> 
      rename(retrieved = .data$retrieved2) |> 
      relocate(.data$retrieved, .before = "unretrieved")
    
    return(dailies_validated)
  }

#' Recalculate party bag
#'
#' Internal function used in \code{\link{partyHuntFinder}}. Recalculate retrieved value for party hunt over bags by dividing retrieved by party size.
#'
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#'
#' @param dailies_df Daily data tibble
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/HSestimate}

recalcPartyBag <-
  function(dailies_df) {
    dailies_df |> 
      mutate(
        retrieved2 = 
          ifelse(
            !is.na(.data$party_size) & .data$retrieved > .data$maxbag,
            round(.data$retrieved/as.numeric(.data$party_size), 0),
            .data$retrieved),
        error1 = 
          ifelse(
            !is.na(.data$party_size) & .data$retrieved > .data$maxbag,
            paste0(
              "retrieved_value_recalculated: ", 
              as.character(.data$retrieved), "/",
              as.character(.data$party_size)),
            NA)) 
  }

#' Get party size
#'
#' Internal function used in \code{\link{partyHuntFinder}}. Pull out obvious group sizes from strings and put them in a new 'party_size' field.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom rlang .data
#'
#' @param dailies_df Daily data tibble
#'
#' @author Abby Walter, \email{abby_walter@@fws.gov}
#' @references \url{https://github.com/USFWS/HSestimate}

getPartySize <-
  function(dailies_df) {
    
    dailies_df |> 
      mutate(
        # Pull out obvious group sizes from strings and put them in a new
        # 'party_size' field
        party_size = 
          case_when(
            str_detect(.data$comment, REGEX_PARTY_NUMERIC) & 
              .data$retrieved != 0 ~ 
              str_extract(.data$comment, REGEX_PARTY_NUMERIC),
            str_detect(.data$comment, "(party|group) of [0-9]{1,2}") & 
              .data$retrieved != 0~ 
              str_extract(.data$comment, "(?<=(party|group) of )[0-9]{1,2}"),
            str_detect(.data$comment, 
                       paste0("((party|group) of two)|(two(?= (m(a|e)n|person|",
                              "hunter|guy|people)))")) & .data$retrieved != 0 ~ 
              "2",
            str_detect(.data$comment, 
                       paste0("((party|group) of three)|(three(?= (m(a|e)n|per",
                              "son|hunter|guy|people)))")) & 
              .data$retrieved != 0 ~ 
              "3",
            str_detect(.data$comment, 
                       paste0("((party|group) of four)|(four(?= (m(a|e)n|perso",
                              "n|hunter|guy|people)))")) & 
              .data$retrieved != 0 ~ 
              "4",
            str_detect(.data$comment, 
                       paste0("((party|group) of four)|(four(?= (m(a|e)n|perso",
                              "n|hunter|guy|people)))")) & 
              .data$retrieved != 0 ~ 
              "5",
            str_detect(.data$comment, 
                       paste0("((party|group) of six)|(six(?= (m(a|e)n|person|",
                              "hunter|guy|people)))")) & .data$retrieved != 0 ~ 
              "6",
            str_detect(.data$comment, 
                       paste0("((party|group) of seven)|(seven(?= (m(a|e)n|per",
                              "son|hunter|guy|people)))")) & 
              .data$retrieved != 0 ~ 
              "7",
            str_detect(.data$comment, 
                       paste0("((party|group) of eight)|(eight(?= (m(a|e)n|per",
                              "son|hunter|guy|people)))")) & 
              .data$retrieved != 0 ~ 
              "8",
            str_detect(.data$comment, 
                       paste0("((party|group) of nine)|(nine(?= (m(a|e)n|perso",
                              "n|hunter|guy|people)))")) & 
              .data$retrieved != 0 ~ 
              "9",
            str_detect(.data$comment, 
                       paste0("((party|group) of ten)|(ten(?= (m(a|e)n|person|",
                              "hunter|guy|people)))")) & .data$retrieved != 0 ~ 
              "10",
            TRUE ~ NA_character_))
  }