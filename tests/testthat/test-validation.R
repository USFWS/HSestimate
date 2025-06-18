test_that("naDaysHunted works", {
  test_na <-
    dplyr::tibble(
      days_hunted = c(0, 1, 5, NA, NA, 2)
    )
  
  expect_true(
    "NA_days_hunted" %in% suppressMessages(naDaysHunted(test_na)$error1))
})

test_that("tooManyDaysHunted works for WF, DV, SCRG, WK", {
  test_tmd <-
    dplyr::tibble(
      days_hunted = c(1:5, 10, 20, 40, 60, 100, 200, 300),
    )
  
  expect_true(
    "too_many_days" %in% 
      suppressMessages(tooManyDaysHunted(test_tmd, "WF")$error2))
  
  expect_true(
    "too_many_days" %in% 
      suppressMessages(tooManyDaysHunted(test_tmd, "DV")$error2))
  
  expect_true(
    "too_many_days" %in% 
      suppressMessages(tooManyDaysHunted(test_tmd, "SCRG")$error2))
  
  expect_true(
    "too_many_days" %in% 
      suppressMessages(tooManyDaysHunted(test_tmd, "WK")$error2))
})

test_that("tooManyDaysHunted works for Alaska CR", {
  test_tmd <-
    dplyr::tibble(
      sampled_state = "Alaska",
      days_hunted = c(1:5, 10, 20, 40, 60, 100, 200, 300),
    )

  tmd_result <-
    suppressMessages(tooManyDaysHunted(test_tmd, "CR")) |> 
    filter(!is.na(error2))
  
  expect_true(nrow(tmd_result) > 0)
})

test_that("tooManyDaysHunted works for North CR", {
  
  n_states <- list()
  for (i in 1:length(REF_STATES_CR_NORTH)) {
    n_states[[i]] <- rep(REF_STATES_CR_NORTH[i], 12)
  }
  
  test_tmd <-
    dplyr::tibble(
      sampled_state = unlist(n_states),
      days_hunted = 
        rep(c(1:5, 10, 20, 40, 60, 100, 200, 300), length(REF_STATES_CR_NORTH)),
    )
  
  tmd_result <-
    suppressMessages(tooManyDaysHunted(test_tmd, "CR")) |> 
    filter(!is.na(error2))
  
  expect_true(length(unique(tmd_result$sampled_state)) > 1)
})

test_that("tooManyDaysHunted works for North CR", {
  
  n_states <- list()
  for (i in 1:length(REF_STATES_CR_SOUTH)) {
    n_states[[i]] <- rep(REF_STATES_CR_SOUTH[i], 12)
  }
  
  test_tmd <-
    dplyr::tibble(
      sampled_state = unlist(n_states),
      days_hunted = 
        rep(c(1:5, 10, 20, 40, 60, 100, 200, 300), length(REF_STATES_CR_SOUTH)),
    )
  
  tmd_result <-
    suppressMessages(tooManyDaysHunted(test_tmd, "CR")) |> 
    filter(!is.na(error2))
  
  expect_true(length(unique(tmd_result$sampled_state)) > 1)
})

test_that("seasonDNH works", {
  test_dnh <-
    dplyr::tibble(
      surveyID = c(1, 1, 2, 2:8),
      retrieved =   c(5, 0, 1, 3, 0, 2, 4, 0, NA, 0),
      days_hunted = c(0, 0, 0, 0, 1, 5, NA, NA, NA, 0)
    )
  
  expect_true(
    "sum_days_hunted_0" %in% suppressMessages(seasonDNH(test_dnh)$error4))
})
