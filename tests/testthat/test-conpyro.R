test_that("MCFFMC calc works", {
  expect_equal(fn_MCFFMC(0), 249.8689075630)
  expect_equal(fn_MCFFMC(90), 10.8307692308)
  expect_identical(fn_MCFFMC(101), 0)
})
