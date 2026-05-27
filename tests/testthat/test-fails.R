test_that("failNADaysHunted works as expected", {
  good_test_data <- tibble(days_hunted = c(1, 2, 3, 0))
  bad_test_data <- tibble(days_hunted = c(1, 2, 3, NA))

  expect_error(failNADaysHunted(bad_test_data))
  expect_no_error(failNADaysHunted(good_test_data))
})

test_that("failNARetrieved works as expected", {
  good_test_data <- tibble(retrieved = c(1, 2, 3, 0))
  bad_test_data <- tibble(retrieved = c(1, 2, 3, NA))

  expect_error(failNARetrieved(bad_test_data))
  expect_no_error(failNARetrieved(good_test_data))
})

test_that("failStateCount works as expected for waterfowl", {
  s_good <- tibble(sampled_state = REF_STATES_ALL)
  d_good <- tibble(sampled_state = REF_STATES_ALL)
  s_bad <- tibble(sampled_state = "HI")
  d_bad <- tibble(sampled_state = "HI")

  expect_no_message(failStateCount(s_good, d_good, REF_N_STATES_WF))
  suppressMessages(
    expect_message(failStateCount(s_bad, d_good, REF_N_STATES_WF)))
  suppressMessages(
    expect_message(failStateCount(s_good, d_bad, REF_N_STATES_WF)))
  suppressMessages(
    expect_message(failStateCount(s_bad, d_bad, REF_N_STATES_WF)))
})

test_that("failStateCount works as expected for doves", {
  s_good <- tibble(sampled_state = REF_STATES_ALL[1:REF_N_STATES_DV])
  d_good <- tibble(sampled_state = REF_STATES_ALL[1:REF_N_STATES_DV])
  s_bad <- tibble(sampled_state = "HI")
  d_bad <- tibble(sampled_state = "HI")

  expect_no_message(failStateCount(s_good, d_good, REF_N_STATES_DV))
  suppressMessages(
    expect_message(failStateCount(s_bad, d_good, REF_N_STATES_DV)))
  suppressMessages(
    expect_message(failStateCount(s_good, d_bad, REF_N_STATES_DV)))
  suppressMessages(
    expect_message(failStateCount(s_bad, d_bad, REF_N_STATES_DV)))
})

test_that("failStateCount works as expected for SCRG", {
  s_good <- tibble(sampled_state = REF_STATES_ALL[1:REF_N_STATES_SCRG])
  d_good <- tibble(sampled_state = REF_STATES_ALL[1:REF_N_STATES_SCRG])
  s_bad <- tibble(sampled_state = "HI")
  d_bad <- tibble(sampled_state = "HI")

  expect_no_message(failStateCount(s_good, d_good, REF_N_STATES_SCRG))
  suppressMessages(
    expect_message(failStateCount(s_bad, d_good, REF_N_STATES_SCRG)))
  suppressMessages(
    expect_message(failStateCount(s_good, d_bad, REF_N_STATES_SCRG)))
  suppressMessages(
    expect_message(failStateCount(s_bad, d_bad, REF_N_STATES_SCRG)))
})

test_that("failStateCount works as expected for WK", {
  s_good <- tibble(sampled_state = REF_STATES_ALL[1:REF_N_STATES_WK])
  d_good <- tibble(sampled_state = REF_STATES_ALL[1:REF_N_STATES_WK])
  s_bad <- tibble(sampled_state = "HI")
  d_bad <- tibble(sampled_state = "HI")

  expect_no_message(failStateCount(s_good, d_good, REF_N_STATES_WK))
  suppressMessages(
    expect_message(failStateCount(s_bad, d_good, REF_N_STATES_WK)))
  suppressMessages(
    expect_message(failStateCount(s_good, d_bad, REF_N_STATES_WK)))
  suppressMessages(
    expect_message(failStateCount(s_bad, d_bad, REF_N_STATES_WK)))
})

test_that("failStateCount works as expected for CR", {
  s_good <- tibble(sampled_state = REF_STATES_ALL[1:REF_N_STATES_CR])
  d_good <- tibble(sampled_state = REF_STATES_ALL[1:REF_N_STATES_CR])
  s_bad <- tibble(sampled_state = "HI")
  d_bad <- tibble(sampled_state = "HI")

  expect_no_message(failStateCount(s_good, d_good, REF_N_STATES_CR))
  suppressMessages(
    expect_message(failStateCount(s_bad, d_good, REF_N_STATES_CR)))
  suppressMessages(
    expect_message(failStateCount(s_good, d_bad, REF_N_STATES_CR)))
  suppressMessages(
    expect_message(failStateCount(s_bad, d_bad, REF_N_STATES_CR)))
})

test_that("failspp works as expected", {
  expect_no_error(failspp("WF"))
  expect_no_error(failspp("DV"))
  expect_no_error(failspp("SCRG"))
  expect_no_error(failspp("WK"))
  expect_no_error(failspp("CR"))
  expect_error(failspp(NA))
  expect_error(failspp(NULL))
  expect_error(failspp(0))
  expect_error(failspp(99))
  expect_error(failspp("0"))
  expect_error(failspp("A"))
  expect_error(failspp("WFF"))
  expect_error(failspp("wf"))
  expect_error(failspp("Wf"))
  expect_error(failspp("Waterfowl"))
  expect_error(failspp("WATERFOWL"))
  expect_error(failspp(" "))
})

test_that("failtype works as expected", {
  expect_no_error(failtype("Ducks"))
  expect_no_error(failtype("Geese"))
  expect_no_error(failtype("Brant"))
  expect_no_error(failtype("SeaDucks"))
  expect_no_error(failtype("MODO"))
  expect_no_error(failtype("WWDO"))
  expect_no_error(failtype("SACR"))
  expect_no_error(failtype("Woodcock"))
  expect_no_error(failtype("Snipe"))
  expect_no_error(failtype("Coots"))
  expect_no_error(failtype("Rails"))
  expect_no_error(failtype("Gallinules"))
  expect_no_error(failtype("BTPI"))
  expect_error(failtype(NA))
  expect_error(failtype(NULL))
  expect_error(failtype(0))
  expect_error(failtype(99))
  expect_error(failtype("0"))
  expect_error(failtype("A"))
  expect_error(failtype("WFF"))
  expect_error(failtype("wf"))
  expect_error(failtype("Wf"))
  expect_error(failtype("Waterfowl"))
  expect_error(failtype("WATERFOWL"))
  expect_error(failtype("seaducks"))
  expect_error(failtype("SEADUCKS"))
  expect_error(failtype(" "))
})

test_that("failSurveyStates works as expected for ducks", {
  good_data <-
    tibble(
      state =
        c(REF_STATES_DK, sample(REF_STATES_DK, size = 100, replace = TRUE)))
  bad_data <- tibble(state = rep("HI", 50))

  expect_no_error(failSurveyStates(good_data, "Ducks"))
  expect_error(failSurveyStates(bad_data, "Ducks"))
})

test_that("failSurveyStates works as expected for geese", {
  good_data <-
    tibble(
      state =
        c(REF_STATES_GS, sample(REF_STATES_GS, size = 100, replace = TRUE)))
  bad_data <- tibble(state = rep("HI", 50))

  expect_no_error(failSurveyStates(good_data, "Geese"))
  expect_error(failSurveyStates(bad_data, "Geese"))
})

test_that("failSurveyStates works as expected for mourning doves", {
  good_data <-
    tibble(
      state =
        c(REF_STATES_MODO,
          sample(REF_STATES_MODO, size = 100, replace = TRUE)))
  bad_data <- tibble(state = rep("HI", 50))

  expect_no_error(failSurveyStates(good_data, "MODO"))
  expect_error(failSurveyStates(bad_data, "MODO"))
})

test_that("failSurveyStates works as expected for white-winged doves", {
  good_data <-
    tibble(
      state =
        c(REF_STATES_WWDO_MAIN,
          REF_STATES_WWDO_EDGE,
          sample(REF_STATES_WWDO_MAIN, size = 50, replace = TRUE),
          sample(REF_STATES_WWDO_EDGE, size = 50, replace = TRUE)))
  bad_data <- tibble(state = rep("HI", 50))

  expect_no_error(failSurveyStates(good_data, "WWDO"))
  expect_error(failSurveyStates(bad_data, "WWDO"))
})

test_that("failSurveyStates works as expected for woodcock", {
  good_data <-
    tibble(
      state =
        c(REF_STATES_WK, sample(REF_STATES_WK, size = 100, replace = TRUE)))
  bad_data <- tibble(state = rep("HI", 50))

  expect_no_error(failSurveyStates(good_data, "Woodcock"))
  expect_error(failSurveyStates(bad_data, "Woodcock"))
})

test_that("failSurveyStates works as expected for snipe", {
  good_data <-
    tibble(
      state =
        c(REF_STATES_SNIPE,
          sample(REF_STATES_SNIPE, size = 100, replace = TRUE)))
  bad_data <- tibble(state = rep("HI", 50))

  expect_no_error(failSurveyStates(good_data, "Snipe"))
  expect_error(failSurveyStates(bad_data, "Snipe"))
})

test_that("failSurveyStates works as expected for coots", {
  good_data <-
    tibble(
      state =
        c(REF_STATES_COOTS,
          sample(REF_STATES_COOTS, size = 100, replace = TRUE)))
  bad_data <- tibble(state = rep("HI", 50))

  expect_no_error(failSurveyStates(good_data, "Coots"))
  expect_error(failSurveyStates(bad_data, "Coots"))
})

test_that("failSurveyStates works as expected for rails", {
  good_data <-
    tibble(
      state =
        c(REF_STATES_RAILS,
          sample(REF_STATES_RAILS, size = 100, replace = TRUE)))
  bad_data <- tibble(state = rep("HI", 50))

  expect_no_error(failSurveyStates(good_data, "Rails"))
  expect_error(failSurveyStates(bad_data, "Rails"))
})

test_that("failSurveyStates works as expected for gallinules", {
  good_data <-
    tibble(
      state =
        c(REF_STATES_GALLS,
          sample(REF_STATES_GALLS, size = 100, replace = TRUE)))
  bad_data <- tibble(state = rep("HI", 50))

  expect_no_error(failSurveyStates(good_data, "Gallinules"))
  expect_error(failSurveyStates(bad_data, "Gallinules"))
})

test_that("failSurveyStates works as expected for cranes", {
  good_data <-
    tibble(
      state =
        c(REF_STATES_CR, sample(REF_STATES_CR, size = 100, replace = TRUE)))
  bad_data <- tibble(state = rep("HI", 50))

  expect_no_error(failSurveyStates(good_data, "SACR"))
  expect_error(failSurveyStates(bad_data, "SACR"))
})

test_that("failSurveyStates works as expected for band-tailed pigeon", {
  good_data <-
    tibble(
      state =
        c(REF_STATES_BTPI, sample(REF_STATES_BTPI, size = 100, replace = TRUE)))
  bad_data <- tibble(state = rep("HI", 50))

  expect_no_error(failSurveyStates(good_data, "BTPI"))
  expect_error(failSurveyStates(bad_data, "BTPI"))
})

test_that("failSurveyStates works as expected for brant", {
  good_data <-
    tibble(
      state =
        c(REF_STATES_BR, sample(REF_STATES_BR, size = 100, replace = TRUE)))
  bad_data <- tibble(state = rep("HI", 50))

  expect_no_error(failSurveyStates(good_data, "Brant"))
  expect_error(failSurveyStates(bad_data, "Brant"))
})

test_that("failSurveyStates works as expected for sea ducks", {
  good_data <-
    tibble(
      state =
        c(REF_STATES_SD, sample(REF_STATES_SD, size = 100, replace = TRUE)))
  bad_data <- tibble(state = rep("HI", 50))

  expect_no_error(failSurveyStates(good_data, "SeaDucks"))
  expect_error(failSurveyStates(bad_data, "SeaDucks"))
})
