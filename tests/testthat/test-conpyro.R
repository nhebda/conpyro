test_that("MCFFMC calc works", {
  expect_equal(fn_MCFFMC(0), 249.8689075630)
  expect_equal(fn_MCFFMC(90), 10.8307692308)
  expect_identical(fn_MCFFMC(101), 0)
})

test_that("MCDMC calc works", {
  expect_equal(fn_MCDMC(0), 300.0070115918)
  expect_equal(fn_MCDMC(100), 48.0015203601)
  expect_equal(fn_MCDMC(200), 22.8002339585)
})

test_that("MCSA_idx calc works", {
  expect_identical(fn_MCSA_idx("spring", "light", "deciduous"), 111)
  expect_identical(fn_MCSA_idx("summer", "moderate", "douglas-fir"), 222)
  expect_identical(fn_MCSA_idx("fall", "dense", "mixedwood"), 333)
  expect_identical(fn_MCSA_idx("sp-su", "light", "pine"), 414)
  expect_identical(fn_MCSA_idx("summer", "moderate", "spruce"), 225)
})
