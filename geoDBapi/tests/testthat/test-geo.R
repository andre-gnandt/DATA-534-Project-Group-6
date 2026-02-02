test_that("get countries", {
  geo <- GEO$new()
  countries <- geo$FindCountries()
  expect_gt(countries$count, 0)
})
