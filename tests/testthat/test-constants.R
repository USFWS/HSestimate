test_that("bag limits assigned as expected", {
  expect_all_true(REF_BAG_LIMIT_DUCKMERG == 11)
  expect_all_true(REF_BAG_LIMIT_SD_AK == 10)
  expect_all_true(REF_BAG_LIMIT_SD_PF == 7)
  expect_all_true(REF_BAG_LIMIT_SD_AF == 4)
  expect_all_true(REF_BAG_LIMIT_WWDO_EDGE == 10)
  expect_all_true(REF_BAG_LIMIT_MODOWWDO == 15)
  expect_all_true(REF_BAG_TOLERANCE == 2)
})

test_that("flyways assigned as expected", {
  AF <-
    c("CT", "DE", "FL", "GA", "MA", "MD", "ME", "NC", "NH", "NJ", "NY", "PA",
      "RI", "SC", "VA", "VT", "WV")
  
  MF <-
    c("AL", "AR", "IA", "IL", "IN", "KY", "LA", "MI", "MN", "MO", "MS", "OH",
      "TN", "WI")
  
  CF <- c("CO", "KS", "ND", "NE", "NM", "OK", "SD", "TX", "WY")
  
  PF <- c("AZ", "CA", "ID", "MT", "NV", "OR", "UT", "WA")
  
  expect_all_true(REF_STATES_AF == AF)
  expect_all_true(REF_STATES_CF == CF)
  expect_all_true(REF_STATES_MF == MF)
  expect_all_true(REF_STATES_PF == PF)
})

test_that("states assigned as expected", {
  all_states <- 
    c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DE", "FL", "GA", "IA", "ID", 
      "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", 
      "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", 
      "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", 
      "WY")
  
  no_dv <- c("AK", "CT", "MA", "ME", "MI", "NH", "NJ", "NY", "VT")
  
  wwdo_main <- c("AL", "AZ", "CA", "FL", "LA", "MS", "NM", "NV", "OK", "TX")
  
  no_wk <- 
    c("AK", "AZ", "CA", "CO", "ID", "MT", "ND", "NM", "NV", "OR", "SD", "UT", 
      "WA", "WY")
  
  no_rails <- 
    c("AK", "AZ", "CA", "ID", "MT", "ND", "NH", "NV", "OR", "SD", "UT", "VT", 
      "WA")
  
  no_galls <- 
    c("AK", "CO", "CT", "IA", "ID", "IL", "IN", "KS", "MA", "MD", "ME", "MO", 
      "MT", "ND", "NE", "NH", "OR", "RI", "SD", "UT", "VT", "WA", "WY")
  
  cr <- c("AK", "CO", "KS", "MN", "MT", "ND", "NM", "OK", "SD", "TX", "WY")
  
  bt <- c("AZ", "CA", "CO", "NM", "OR", "UT", "WA")
  
  br <- 
    c("AK", "CA", "CT", "DE", "MA", "MD", "NC", "NH", "NJ", "NY", "OR", "RI", 
      "VA", "WA")
  
  sd <- 
    c("AK", "CA", "CT", "DE", "MA", "MD", "ME", "NH", "NJ", "NY", "OR", "RI",
      "VA", "WA")
  
  duckmerg <-
    c("AL", "AR", "FL", "GA", "IA", "IL", "IN", "KY", "LA", "MA", "ME", "MI", 
      "MN", "MS", "NC", "NH", "NJ", "OH", "PA", "RI", "SC", "TN", "VA", "VT",
      "WI", "WV")
  
  expect_all_true(REF_STATES_ALL == all_states)
  expect_all_true(REF_STATES_DK == all_states)
  expect_all_true(REF_STATES_GS == all_states[all_states != "FL"])
  expect_all_true(REF_STATES_MODO == all_states[!all_states %in% no_dv])
  expect_all_true(REF_STATES_WWDO_MAIN == wwdo_main)
  expect_all_true(REF_STATES_WWDO_EDGE == c("AR", "CO", "GA", "KS", "UT"))
  expect_all_true(REF_STATES_WK  == all_states[!all_states %in% no_wk])
  expect_all_true(REF_STATES_SNIPE == all_states)
  expect_all_true(REF_STATES_COOTS == all_states[all_states != "AK"])
  expect_all_true(REF_STATES_RAILS == all_states[!all_states %in% no_rails])
  expect_all_true(REF_STATES_GALLS == all_states[!all_states %in% no_galls])
  expect_all_true(REF_STATES_CR == cr)
  expect_all_true(REF_STATES_BTPI == bt)
  expect_all_true(REF_STATES_BR == br)
  expect_all_true(REF_STATES_SD == sd)
  expect_all_true(REF_STATES_DUCKMERG == duckmerg)
})

test_that("number of states assigned as expected", {
  WF <- 49
  DV <- 40
  WK <- 35
  SCRG <- 49
  CR <- 11
  
  expect_all_true(REF_N_STATES_WF == WF)
  expect_all_true(REF_N_STATES_DV == DV)
  expect_all_true(REF_N_STATES_WK == WK)
  expect_all_true(REF_N_STATES_SCRG == SCRG)
  expect_all_true(REF_N_STATES_CR == CR)
})
