# fn_mcsa_idx ----
test_that("fn_mcsa_idx returns correct codes for canonical inputs", {
  expect_equal(
    fn_mcsa_idx(
      FFMC = 90,
      season = "spring",
      density = "light",
      stand = "pine",
      model_mcsa = "original"
    ),
    114
  )
  expect_equal(
    fn_mcsa_idx(
      FFMC = 90,
      season = "summer",
      density = "moderate",
      stand = "spruce",
      model_mcsa = "original"
    ),
    225
  )
  expect_equal(
    fn_mcsa_idx(
      FFMC = 90,
      season = "fall",
      density = "dense",
      stand = "deciduous",
      model_mcsa = "original"
    ),
    331
  )
})

test_that("fn_mcsa_idx season, density, and stand aliases map correctly", {
  expect_equal(
    fn_mcsa_idx(
      FFMC = 90,
      season = "1",
      density = "1",
      stand = "d",
      model_mcsa = "original"
    ),
    111
  )
  expect_equal(
    fn_mcsa_idx(
      FFMC = 90,
      season = "2",
      density = "2",
      stand = "df",
      model_mcsa = "original"
    ),
    222
  )
  expect_equal(
    fn_mcsa_idx(
      FFMC = 90,
      season = "3",
      density = "3",
      stand = "m",
      model_mcsa = "original"
    ),
    333
  )
  expect_equal(
    fn_mcsa_idx(
      FFMC = 90,
      season = "1.5",
      density = "light",
      stand = "p",
      model_mcsa = "original"
    ),
    414
  )
})

test_that("fn_mcsa_idx inputs are case-insensitive", {
  expect_equal(
    fn_mcsa_idx(
      FFMC = 90,
      season = "SPRING",
      density = "LIGHT",
      stand = "PINE",
      model_mcsa = "ORIGINAL"
    ),
    114
  )
  expect_equal(
    fn_mcsa_idx(
      FFMC = 90,
      season = "Sp-Su",
      density = "Dense",
      stand = "Df",
      model_mcsa = "Corrected"
    ),
    432
  )
})

test_that("fn_mcsa_idx density adjustment is applied for corrected model", {
  expect_equal(
    fn_mcsa_idx(
      FFMC = 97,
      season = "spring",
      density = "light",
      stand = "pine",
      model_mcsa = "corrected"
    ),
    124
  )
  expect_equal(
    fn_mcsa_idx(
      FFMC = 96.15,
      season = "spring",
      density = "light",
      stand = "pine",
      model_mcsa = "corrected"
    ),
    114
  )
  expect_equal(
    fn_mcsa_idx(
      FFMC = 93,
      season = "summer",
      density = "dense",
      stand = "spruce",
      model_mcsa = "corrected"
    ),
    225
  )
  expect_equal(
    fn_mcsa_idx(
      FFMC = 92.93,
      season = "summer",
      density = "dense",
      stand = "spruce",
      model_mcsa = "corrected"
    ),
    235
  )
})

test_that("fn_mcsa_idx original model does not apply density adjustments", {
  expect_equal(
    fn_mcsa_idx(
      FFMC = 99,
      season = "spring",
      density = "light",
      stand = "pine",
      model_mcsa = "original"
    ),
    114
  )
  expect_equal(
    fn_mcsa_idx(
      FFMC = 99,
      season = "summer",
      density = "dense",
      stand = "spruce",
      model_mcsa = "original"
    ),
    235
  )
})

test_that("fn_mcsa_idx invalid FFMC values fail validation", {
  expect_error(
    fn_mcsa_idx(
      FFMC = 79,
      season = "spring",
      density = "light",
      stand = "pine",
      model_mcsa = "original"
    ),
    "FFMC"
  )
  expect_error(
    fn_mcsa_idx(
      FFMC = 100,
      season = "spring",
      density = "light",
      stand = "pine",
      model_mcsa = "original"
    ),
    "FFMC"
  )
  expect_error(
    fn_mcsa_idx(
      FFMC = NA,
      season = "spring",
      density = "light",
      stand = "pine",
      model_mcsa = "original"
    ),
    "FFMC"
  )
})

test_that("fn_mcsa_idx invalid inputs fail with informative errors", {
  expect_error(
    fn_mcsa_idx(
      FFMC = 90,
      season = "winter",
      density = "light",
      stand = "pine",
      model_mcsa = "original"
    ),
    "season"
  )
  expect_error(
    fn_mcsa_idx(
      FFMC = 90,
      season = "spring",
      density = "heavy",
      stand = "pine",
      model_mcsa = "original"
    ),
    "density"
  )
  expect_error(
    fn_mcsa_idx(
      FFMC = 90,
      season = "spring",
      density = "light",
      stand = "oak",
      model_mcsa = "original"
    ),
    "stand"
  )
  expect_error(
    fn_mcsa_idx(
      FFMC = 90,
      season = "spring",
      density = "light",
      stand = "pine",
      model_mcsa = "new"
    ),
    "model_mcsa"
  )
})

test_that("fn_mcsa_idx vector inputs are rejected", {
  expect_error(
    fn_mcsa_idx(
      FFMC = c(90, 91),
      season = "spring",
      density = "light",
      stand = "pine",
      model_mcsa = "original"
    ),
    "FFMC"
  )
  expect_error(
    fn_mcsa_idx(
      FFMC = 90,
      season = c("spring", "summer"),
      density = "light",
      stand = "pine",
      model_mcsa = "original"
    ),
    "season"
  )
})

test_that("fn_mcsa_idx output is a single numeric scalar", {
  result <- fn_mcsa_idx(
    FFMC = 90,
    season = "spring",
    density = "light",
    stand = "pine",
    model_mcsa = "original"
  )
  expect_type(result, "double")
  expect_length(result, 1)
  expect_true(is.finite(result))
})
