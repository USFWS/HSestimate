test_that("naDaysHunted works", {
  test_na <-
    dplyr::tibble(
      days_hunted = c(0, 1, 5, NA, NA, 2)
    )
  
  expect_true(NA %in% suppressMessages(naDaysHunted(test_na)$error1))
})
