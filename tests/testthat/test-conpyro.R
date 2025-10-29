test_that("MCFFMC calc works", {
  expect_equal(fn_MCFFMC(0), 250)
  expect_equal(fn_MCFFMC(90), 10.8364515381)
  expect_equal(fn_MCFFMC(101), 0)
})
