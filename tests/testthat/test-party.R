test_that("getPartySize creates party_size field", {
  test_party <-
    dplyr::tibble(
      retrieved = rep(2, 6),
      comment = rep(NA, 6)
    )
  
  expect_true(ncol(getPartySize(test_party)) == 3)
})

test_that("getPartySize gets numeric party size", {
  test_party <-
    dplyr::tibble(
      retrieved = rep(2, 4),
      comment = 
        c("we hunted in a party of 6",
          "5 men hunt",
          "10 person group",
          "group of 7 hunters, 2 dogs")
    )
  
  expect_identical(getPartySize(test_party)$party_size, c("6", "5", "10", "7"))
})

test_that("getPartySize gets alphabetical party size", {
  test_party <-
    dplyr::tibble(
      retrieved = rep(2, 4),
      comment = 
        c("party of three",
          "seven man hunt",
          "group of seven hunters, 2 dogs",
          "party of eight, two days")
    )
  
  expect_identical(getPartySize(test_party)$party_size, c("3", "7", "7", "8"))
})

test_that("recalcPartyBag creates error field", {
  test_party <-
    dplyr::tibble(
      retrieved = c(40, 13, 3, 1),
      maxbag = c(6, 6, 6, 6),
      comment = 
        c("we hunted in a party of 6",
          "party of three",
          "seven man hunt",
          "5 men hunt")
    )
  
  expect_true(ncol(recalcPartyBag(getPartySize(test_party))) == 6)
})

test_that("recalcPartyBag recalculates retrieved over bag for parties", {
  test_party <-
    dplyr::tibble(
      retrieved = c(40, 13, 3, 1),
      maxbag = c(6, 6, 6, 6),
      comment = 
        c("we hunted in a party of 6",
          "party of three",
          "seven man hunt",
          "5 men hunt")
    )
  
  expect_identical(
    recalcPartyBag(getPartySize(test_party))$retrieved2, 
    c(7, 4, 3, 1))
})

test_that("recalcPartyBag does not recalculate retrieved over bag for non-parties", {
  test_party <-
    dplyr::tibble(
      retrieved = c(40, 10, 3, 1),
      maxbag = c(6, 6, 6, 6),
      comment = c("", "", "", "")
    )
  
  expect_identical(
    recalcPartyBag(getPartySize(test_party))$retrieved2, 
    test_party$retrieved)
})
