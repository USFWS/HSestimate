test_that("naDaysHunted works", {
  test_na <-
    dplyr::tibble(days_hunted = c(0, 1, 5, NA, NA, 2))

  expect_true(
    "NA_days_hunted" %in% suppressMessages(naDaysHunted(test_na)$error1))
})

# test_that("tooManyDaysHunted works", {
#
# })

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
