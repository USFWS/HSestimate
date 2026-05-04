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
REF_DAY_LIMIT_WK <- 63
REF_DAY_LIMIT_SCRG <- 107
REF_DAY_LIMIT_CR_AK <- 106
REF_DAY_LIMIT_CR_SOUTH <- 92
REF_DAY_LIMIT_CR_NORTH <- 60

# flyways -----------------------------------------------------------------

REF_STATES_AF <- 
  c("CT", "DE", "FL", "GA", "MA", "MD", "ME", "NC", "NH", "NJ", "NY", "PA", 
    "RI", "SC", "VA", "VT", "WV")

REF_STATES_MF <-
  c("AL", "AR", "IA", "IL", "IN", "KY", "LA", "MI", "MN", "MO", "MS", "OH", 
    "TN", "WI")

REF_STATES_CF <- 
  c("CO", "KS", "ND", "NE", "NM", "OK", "SD", "TX", "WY")

REF_STATES_PF <- 
  c("AZ", "CA", "ID", "MT", "NV", "OR", "UT", "WA")

# states ------------------------------------------------------------------

# Table of state names and their abbreviations
REF_STATES_AND_ABBRS <-
  tibble(
    state = state.abb,
    sampled_state = state.name) |> 
  filter(state != "HI")

# Southern states with a Sandhill Crane season
REF_STATES_CR_SOUTH <- 
  c("Texas", "New Mexico", "Oklahoma")

# Northern states with a Sandhill Crane season
REF_STATES_CR_NORTH <- 
  c("South Dakota", "North Dakota", "Wyoming", "Colorado", "Utah", "Montana", 
    "Kansas")

# States with a Band-tailed Pigeon (BTPI) season
REF_STATES_BTPI <- 
  c("AZ", "CA", "CO", "NM", "OR", "UT", "WA")

# States with a Sea Duck season
REF_STATES_SD <- 
  c("AK", "CA", "CT", "DE", "MA", "MD", "ME", "NH", "NJ", "NY", "OR", "RI", 
    "VA", "WA")

# Sea Duck states in the Atlantic Flyway
REF_STATES_SD_AF <- 
  REF_STATES_SD[REF_STATES_SD %in% REF_STATES_AF]

# States with a Brant season
REF_STATES_BR <- 
  c("AK", "CA", "CT", "DE", "MA", "MD", "ME", "NC", "NH", "NJ", "NY", "OR", 
    "RI", "VA", "WA")

# States with a combined duck and merganser season
REF_STATES_DUCKMERG <-
  c("AL", "AR", "CO", "DE", "FL", "GA", "IA", "IL", "IN", "KS", "KY", "LA", 
    "MA", "ME", "MI", "MN", "MS", "NC", "ND", "NE", "NH", "NJ", "OH", "OK", 
    "PA", "RI", "SC", "SD", "TN", "TX", "VA", "VT", "WI", "WV", "WY")
# AF and MF only... 
# must be changed CT, MD, NY, MO are "ducks including"...? ask RR

# White-winged Dove (WWDO) main/primary harvest states
REF_STATES_WWDO_MAIN <- 
  c("AL", "AZ", "CA", "FL", "LA", "MS", "NM", "NV", "OK", "TX")

# White-winged Dove (WWDO) edge states (these states border the main states)
REF_STATES_WWDO_EDGE <- 
  c("AR", "CO", "GA", "KS", "UT")

# Table of expected White-winged Dove (WWDO) occurrence by state
REF_STATES_WWDO_DF <-
  REF_STATES_AND_ABBRS |> 
  mutate(
    wwdo_state_status = 
      case_when(
        state %in% REF_STATES_WWDO_MAIN ~ "main",
        state %in% REF_STATES_WWDO_EDGE ~ "edge",
        TRUE ~ "none"))

# States with NO open season for American Coots
REF_STATES_NO_COOTS <- "AK"

# States with NO open season for rails
REF_STATES_NO_RAILS <- 
  c("AK", "AZ", "CA", "ID", "MT", "ND", "NH", "NV", "OR", "SD", "UT", "VT", 
    "WA")

# States with NO open season for gallinules
REF_STATES_NO_GALLS <- 
  c("AK", "CO", "CT", "IA", "ID", "IL", "IN", "KS", "MA", "MD", "ME", "MO", 
    "MT", "ND", "NE", "NH", "OR", "RI", "SD", "UT", "VT", "WA", "WY")

# number of expected states -----------------------------------------------

REF_N_STATES_WF <- 49
REF_N_STATES_DV <- 40
REF_N_STATES_CR <- 11
REF_N_STATES_SCRG <- 39
REF_N_STATES_WK <- 35

# party -------------------------------------------------------------------

REGEX_PARTY_NUMERIC <- 
  "([0-9]{1,2})(?=( )?((wo)?m(a|e)n|person|hunter|guy|people))"
