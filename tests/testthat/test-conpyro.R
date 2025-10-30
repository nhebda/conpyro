test_that("fn_MCFFMC() works", {
  expect_equal(fn_MCFFMC(0), 249.8689075630)
  expect_equal(fn_MCFFMC(90), 10.8307692308)
  expect_identical(fn_MCFFMC(101), 0)
})

test_that("fn_MCDMC() works", {
  expect_equal(fn_MCDMC(0), 300.0070115918)
  expect_equal(fn_MCDMC(100), 48.0015203601)
  expect_equal(fn_MCDMC(200), 22.8002339585)
})

test_that("fn_MCSA_idx() works", {
  expect_identical(fn_MCSA_idx("spring", "light", "deciduous"), 111)
  expect_identical(fn_MCSA_idx("summer", "moderate", "douglas-fir"), 222)
  expect_identical(fn_MCSA_idx("fall", "dense", "mixedwood"), 333)
  expect_identical(fn_MCSA_idx("sp-su", "light", "pine"), 414)
  expect_identical(fn_MCSA_idx("summer", "moderate", "spruce"), 225)
})

test_that("fn_MCSA() works", {
  expect_equal(fn_MCSA(111, 0, 0), 0)
  expect_equal(fn_MCSA(224, 10, 50), 8.7607376501)
  expect_equal(fn_MCSA(435, 10, 50), 8.8152118260)
  expect_equal(fn_MCSA(331, 250, 300), 2955.2094540452)
})

# test_that("fn_pCFO() works", {
#
# })
