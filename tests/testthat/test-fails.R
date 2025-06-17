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
