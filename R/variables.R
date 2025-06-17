#' @importFrom dplyr tibble
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when

# Variables

# bag limits --------------------------------------------------------------

REF_BAG_LIMIT_DUCKMERG <- 11
REF_BAG_LIMIT_SD_AK <- 10
REF_BAG_LIMIT_SD_PF <- 7
REF_BAG_LIMIT_SD_AF <- 4
REF_BAG_LIMIT_WWDO_EDGE <- 10
REF_BAG_LIMIT_MODOWWDO <- 15
REF_BAG_TOLERANCE <- 2

# day limits --------------------------------------------------------------

REF_DAY_LIMIT_WF <- 107
REF_DAY_LIMIT_DV <- 107
REF_DAY_LIMIT_WK <- 107
REF_DAY_LIMIT_SCRG <- 107
REF_DAY_LIMIT_CR_AK <- 106
REF_DAY_LIMIT_CR_SOUTH <- 92
REF_DAY_LIMIT_CR_NORTH <- 60

# states ------------------------------------------------------------------

# Create a table of state names and their abbreviations
REF_STATES_AND_ABBRS <-
  tibble(
    state = state.abb,
    sampled_state = state.name) |> 
  filter(state != "HI")

REF_STATES_CR_SOUTH <- c("Texas", "New Mexico", "Oklahoma")

REF_STATES_CR_NORTH <- 
  c("South Dakota", "North Dakota", "Wyoming", "Colorado", "Utah", "Montana", 
    "Kansas")

REF_STATES_SD_BR <- 
  c("AK", "CA", "CT", "DE", "MA", "MD", "NC", "NH", "NJ", "NY", "RI", "VA")

REF_STATES_SD_ONLY <- "ME"

# Create a vector of SD states
REF_STATES_SD <- c(REF_STATES_SD_BR, REF_STATES_SD_ONLY, "OR", "WA")

# Create a vector of BR states
REF_STATES_BR <- c(REF_STATES_SD_BR, "OR", "WA")

# Define the states that have a combined duck and merganser season
REF_STATES_DUCKMERG <-
  c("AL", "AR", "CO", "DE", "FL", "IA", "IL", "IN", "KS", "KY", "LA", "MA", 
    "ME", "MI", "MN", "MS", "NC", "ND", "NE", "NH", "NJ", "OH", "OK", "PA", 
    "RI", "SC", "SD", "TN", "TX", "VA", "VT", "WI", "WV", "WY", "GA")

# Create a vector of White-winged Dove (WWDO) main/primary harvest states
REF_STATES_WWDO_MAIN <- 
  c("CA", "NV", "AZ", "NM", "TX", "OK", "LA", "MS", "AL", "FL")

# Create a vector of WWDO edge states (these states border the main states)
REF_STATES_WWDO_EDGE <- c("UT", "CO", "KS", "AR", "GA")

# Table of expected WWDO occurrence by state
REF_STATES_WWDO_DF <-
  REF_STATES_AND_ABBRS |> 
  mutate(
    wwdo_state_status = 
      case_when(
        state %in% REF_STATES_WWDO_MAIN ~ "main",
        state %in% REF_STATES_WWDO_EDGE ~ "edge",
        TRUE ~ "none"))

# flyways -----------------------------------------------------------------

REF_STATES_AF <- REF_STATES_SD[!REF_STATES_SD %in% c("AK", "CA", "OR", "WA")]

# number of expected states -----------------------------------------------

REF_N_STATES_WF <- 49
REF_N_STATES_DV <- 40
REF_N_STATES_CR <- 11
REF_N_STATES_SCRG <- 39
REF_N_STATES_WK <- 35

# party -------------------------------------------------------------------

REGEX_PARTY_NUMERIC <- "([0-9]{1,2})(?= (m(a|e)n|person|hunter|guy|people))"
