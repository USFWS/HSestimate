test_that("good data passes fails function", {
  season_df <-
    dplyr::tibble(
      days_hunted = c(0, 1, 5),
      retrieved = c(0, 0, 2),
      sampled_state = c("AK", "AK", "LA")
    )
  
  dailies_df <-
    dplyr::tibble(
      days_hunted = c(0, 1, 5),
      retrieved = c(0, 0, 2),
      sampled_state = c("AK", "AK", "LA")
    )
  
  expect_no_error(fails(season_df, dailies_df, 2))
})

test_that("fails works on n_states type", {
  season_df <-
    dplyr::tibble(
      days_hunted = c(0, 1, 5),
      retrieved = c(0, 0, 2),
      sampled_state = c("AK", "AK", "LA")
    )
  
  dailies_df <-
    dplyr::tibble(
      days_hunted = c(0, 1, 5),
      retrieved = c(0, 0, 2),
      sampled_state = c("AK", "AK", "LA")
    )
  
  expect_error(fails(season_df, dailies_df, "2"))
})

test_that("fails works on NA retrieved in season data", {
  season_df <-
    dplyr::tibble(
      days_hunted = c(0, 1, 5),
      retrieved = c(0, NA, 2),
      sampled_state = c("AK", "AK", "LA")
    )
  
  dailies_df <-
    dplyr::tibble(
      days_hunted = c(0, 1, 5),
      retrieved = c(0, 0, 2),
      sampled_state = c("AK", "AK", "LA")
    )
  
  expect_error(fails(season_df, dailies_df, 2))
})

test_that("fails works on NA retrieved in daily data", {
  season_df <-
    dplyr::tibble(
      days_hunted = c(0, 1, 5),
      retrieved = c(0, 1, 2),
      sampled_state = c("AK", "AK", "LA")
    )
  
  dailies_df <-
    dplyr::tibble(
      days_hunted = c(0, 1, 5),
      retrieved = c(0, NA, 2),
      sampled_state = c("AK", "AK", "LA")
    )
  
  expect_error(fails(season_df, dailies_df, 2))
})

test_that("fails works on good season and daily n_states count", {
  season_df <-
    dplyr::tibble(
      days_hunted = c(0, 1, 5),
      retrieved = c(0, 0, 2),
      sampled_state = c("AK", "CT", "LA")
    )
  
  dailies_df <-
    dplyr::tibble(
      days_hunted = c(0, 1, 5),
      retrieved = c(0, 0, 2),
      sampled_state = c("AK", "CT", "LA")
    )
  
  expect_no_message(fails(season_df, dailies_df, 3))
})

test_that("fails works on bad daily n_states count", {
  season_df <-
    dplyr::tibble(
      days_hunted = c(0, 1, 5),
      retrieved = c(0, 0, 2),
      sampled_state = c("AK", "CT", "LA")
    )
  
  dailies_df <-
    dplyr::tibble(
      days_hunted = c(0, 1, 5),
      retrieved = c(0, 0, 2),
      sampled_state = c("AK", "AK", "LA")
    )
  
  expect_message(fails(season_df, dailies_df, 3))
})

test_that("fails works on bad season n_states count", {
  season_df <-
    dplyr::tibble(
      days_hunted = c(0, 1, 5),
      retrieved = c(0, 0, 2),
      sampled_state = c("AK", "AK", "LA")
    )
  
  dailies_df <-
    dplyr::tibble(
      days_hunted = c(0, 1, 5),
      retrieved = c(0, 0, 2),
      sampled_state = c("AK", "CT", "LA")
    )
  
  expect_message(fails(season_df, dailies_df, 3))
})

test_that("fails works on bad season and daily n_states count", {
  season_df <-
    dplyr::tibble(
      days_hunted = c(0, 1, 5),
      retrieved = c(0, 0, 2),
      sampled_state = c("AK", "AK", "LA")
    )
  
  dailies_df <-
    dplyr::tibble(
      days_hunted = c(0, 1, 5),
      retrieved = c(0, 0, 2),
      sampled_state = c("AK", "CT", "LA")
    )
  
  suppressMessages(expect_message(fails(season_df, dailies_df, 4)))
})

test_that("failspp works on good species groups", {
  expect_no_error(failspp("WF"))
  expect_no_error(failspp("DV"))
  expect_no_error(failspp("SCRG"))
  expect_no_error(failspp("WK"))
  expect_no_error(failspp("CR"))
})

test_that("failspp works on bad species groups", {
  expect_error(failspp("WFF"))
  expect_error(failspp("V"))
  expect_error(failspp("SR"))
  expect_error(failspp("NO"))
})
