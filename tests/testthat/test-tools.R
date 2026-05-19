# t_mcSeason() ----
test_that("t_mcSeason returns a single numeric season code", {
  out <- t_mcSeason(month = 5, day = 11)

  expect_type(out, "double")
  expect_length(out, 1L)
  expect_true(out %in% c(1, 1.5, 2, 3))
})

test_that("t_mcSeason maps dates to the correct season buckets", {
  # Spring: Jan 1 to May 31
  expect_identical(t_mcSeason(month = 1, day = 1), 1)
  expect_identical(t_mcSeason(month = 5, day = 31), 1)

  # Spring-summer transition: Jun 1 to Jun 15
  expect_identical(t_mcSeason(month = 6, day = 1), 1.5)
  expect_identical(t_mcSeason(month = 6, day = 15), 1.5)

  # Summer: Jun 16 to Aug 31
  expect_identical(t_mcSeason(month = 6, day = 16), 2)
  expect_identical(t_mcSeason(month = 8, day = 31), 2)

  # Fall: Sep 1 to Dec 31
  expect_identical(t_mcSeason(month = 9, day = 1), 3)
  expect_identical(t_mcSeason(month = 12, day = 31), 3)
})

test_that("t_mcSeason treats Feb 29 as a valid date (leap-year handling)", {
  expect_identical(t_mcSeason(month = 2, day = 29), 1)
})

test_that("t_mcSeason accepts integerish inputs and normalizes them", {
  expect_identical(t_mcSeason(month = "6", day = "7"), 1.5)
  expect_identical(t_mcSeason(month = 6.0, day = 7.0), 1.5)
  expect_identical(t_mcSeason(month = 9L, day = 24L), 3)
})

test_that("t_mcSeason errors on invalid month/day values", {
  # Month out of bounds
  expect_error(
    t_mcSeason(month = 0, day = 1),
    regexp = "month|Month|\\b0\\b|lower|upper",
    fixed = FALSE
  )
  expect_error(
    t_mcSeason(month = 13, day = 1),
    regexp = "month|Month|\\b13\\b|lower|upper",
    fixed = FALSE
  )

  # Day out of bounds
  expect_error(
    t_mcSeason(month = 1, day = 0),
    regexp = "day|Day|\\b0\\b|lower|upper",
    fixed = FALSE
  )
  expect_error(
    t_mcSeason(month = 1, day = 32),
    regexp = "day|Day|\\b32\\b|lower|upper",
    fixed = FALSE
  )

  # Non-integerish
  expect_error(
    t_mcSeason(month = 6.2, day = 7),
    regexp = "month|integerish|Must be|integer",
    fixed = FALSE
  )
  expect_error(
    t_mcSeason(month = 6, day = "7.1"),
    regexp = "day|integerish|Must be|integer",
    fixed = FALSE
  )

  # Missing values
  expect_error(
    t_mcSeason(month = NA, day = 1),
    regexp = "month|missing|NA|integerish",
    fixed = FALSE
  )
  expect_error(
    t_mcSeason(month = 1, day = NA),
    regexp = "day|missing|NA|integerish",
    fixed = FALSE
  )
})

test_that("t_mcSeason errors on invalid calendar date combinations", {
  # April 31 does not exist
  expect_error(
    t_mcSeason(month = 4, day = 31),
    regexp = "Invalid month/day combination\\.",
    fixed = FALSE
  )

  # Feb 30 does not exist (Feb 29 allowed)
  expect_error(
    t_mcSeason(month = 2, day = 30),
    regexp = "Invalid month/day combination\\.",
    fixed = FALSE
  )
})

test_that("t_mcSeason enforces scalar inputs", {
  expect_error(
    t_mcSeason(month = c(6, 7), day = 1),
    regexp = "Must have length 1",
    fixed = FALSE
  )
  expect_error(
    t_mcSeason(month = 6, day = c(1, 2)),
    regexp = "Must have length 1",
    fixed = FALSE
  )
})

# t_mcDensity() ----
test_that("t_mcDensity returns a scalar integer code", {
  out <- t_mcDensity(canopy_closure = 50)

  expect_type(out, "integer")
  expect_length(out, 1L)
  expect_true(out %in% c(1L, 2L, 3L))
})

test_that("t_mcDensity maps canopy closure to density categories", {
  # Light: <= 45
  expect_identical(t_mcDensity(20), 1L)
  expect_identical(t_mcDensity(45), 1L)

  # Moderate: 46-60
  expect_identical(t_mcDensity(46), 2L)
  expect_identical(t_mcDensity(60), 2L)

  # Dense: >= 61
  expect_identical(t_mcDensity(61), 3L)
  expect_identical(t_mcDensity(100), 3L)
})

test_that("t_mcDensity errors on invalid canopy_closure values", {
  expect_error(
    t_mcDensity(19),
    regexp = "canopy_closure|lower|20|Must be",
    fixed = FALSE
  )
  expect_error(
    t_mcDensity(101),
    regexp = "canopy_closure|upper|100|Must be",
    fixed = FALSE
  )
  expect_error(
    t_mcDensity(NA_real_),
    regexp = "canopy_closure|missing|NA|Must be",
    fixed = FALSE
  )
})

test_that("t_mcDensity enforces numeric input type (strict policy)", {
  expect_error(
    t_mcDensity("45"),
    regexp = "canopy_closure|numeric|number|Must be",
    fixed = FALSE
  )
  expect_error(
    t_mcDensity("abc"),
    regexp = "canopy_closure|numeric|number|Must be",
    fixed = FALSE
  )
})

test_that("t_mcDensity enforces scalar input", {
  expect_error(
    t_mcDensity(c(30, 40)),
    regexp = "Must have length 1",
    fixed = FALSE
  )
})

# t_mcF() ----
test_that("t_mcF returns a scalar numeric value", {
  out <- t_mcF(FFMC = 85)

  expect_type(out, "double")
  expect_length(out, 1L)
  expect_true(is.finite(out))
})

test_that("t_mcF matches mcFFMC() with rounding to 2 decimals", {
  ffmc_vals <- c(80, 84, 89, 92, 99)

  for (ffmc in ffmc_vals) {
    expect_equal(
      t_mcF(FFMC = ffmc),
      round(mcFFMC(ffmc), 2)
    )
  }
})

test_that("t_mcF rounds output to 2 decimals", {
  out <- t_mcF(FFMC = 85)
  expect_equal(out, round(out, 2))
})

test_that("t_mcF is monotone decreasing with FFMC", {
  m80 <- t_mcF(FFMC = 80)
  m85 <- t_mcF(FFMC = 85)
  m90 <- t_mcF(FFMC = 90)
  m99 <- t_mcF(FFMC = 99)

  expect_true(m80 > m85)
  expect_true(m85 > m90)
  expect_true(m90 > m99)
})

test_that("t_mcF errors on invalid FFMC values", {
  expect_error(
    t_mcF(FFMC = 79.9),
    regexp = "FFMC|lower|80|Must be",
    fixed = FALSE
  )
  expect_error(
    t_mcF(FFMC = 99.1),
    regexp = "FFMC|upper|99|Must be",
    fixed = FALSE
  )
  expect_error(
    t_mcF(FFMC = NA_real_),
    regexp = "FFMC|missing|NA|Must be",
    fixed = FALSE
  )
})

test_that("t_mcF enforces strict numeric type and scalar length", {
  expect_error(
    t_mcF(FFMC = "80"),
    regexp = "FFMC|numeric|number|Must be",
    fixed = FALSE
  )

  expect_error(
    t_mcF(FFMC = c(80, 81)),
    regexp = "Inputs must have length 1|Must have length 1",
    fixed = FALSE
  )
})

# t_mcsa() ----
test_that("t_mcsa returns a scalar numeric value rounded to 2 decimals", {
  out <- t_mcsa(
    FFMC = 85,
    DMC = 30,
    season = 1,
    density = 2,
    stand = "p"
  )

  expect_type(out, "double")
  expect_length(out, 1L)
  expect_true(is.finite(out))
  expect_equal(out, round(out, 2))
})

test_that("t_mcsa accepts label or numeric equivalents for season/density", {
  out_num <- t_mcsa(
    FFMC = 85,
    DMC = 30,
    season = 1,
    density = 2,
    stand = "p"
  )

  out_lbl <- t_mcsa(
    FFMC = 85,
    DMC = 30,
    season = "spring",
    density = "moderate",
    stand = "pine"
  )

  expect_equal(out_num, out_lbl)
})

test_that("t_mcsa accepts stand abbreviations", {
  out_full <- t_mcsa(
    FFMC = 85,
    DMC = 30,
    season = "spring",
    density = "moderate",
    stand = "spruce"
  )

  out_abbr <- t_mcsa(
    FFMC = 85,
    DMC = 30,
    season = "spring",
    density = "moderate",
    stand = "s"
  )

  expect_equal(out_full, out_abbr)
})

test_that("t_mcsa matches known values for the default model", {
  expect_equal(
    t_mcsa(FFMC = 80, DMC = 20, season = 1, density = 3, stand = "s"),
    23.45
  )

  expect_equal(
    t_mcsa(FFMC = 84, DMC = 30, season = 1, density = 3, stand = "p"),
    16.33
  )

  expect_equal(
    t_mcsa(FFMC = 89, DMC = 40, season = 2, density = 2, stand = "p"),
    12.30
  )

  expect_equal(
    t_mcsa(FFMC = 92, DMC = 60, season = 2, density = 1, stand = "p"),
    7.50
  )
})

test_that("t_mcsa handles sp-su seasonal averaging as expected", {
  expect_equal(
    t_mcsa(
      FFMC = 90,
      DMC = 30,
      season = "sp-su",
      density = "moderate",
      stand = "p"
    ),
    10.80
  )
})

test_that("model_mcsa affects output when corrected density adjustment
          triggers", {
            # Trigger case: light density becomes moderate when FFMC > 96.15
            out_orig <- t_mcsa(
              FFMC = 97,
              DMC = 30,
              season = "spring",
              density = "light",
              stand = "p",
              model_mcsa = "original"
            )

            out_corr <- t_mcsa(
              FFMC = 97,
              DMC = 30,
              season = "spring",
              density = "light",
              stand = "p",
              model_mcsa = "corrected"
            )

            expect_equal(out_orig, 5.48)
            expect_equal(out_corr, 5.28)
            expect_true(out_orig != out_corr)
          })

test_that("t_mcsa errors on unknown ... argument names", {
  expect_error(
    t_mcsa(
      FFMC = 85,
      DMC = 30,
      season = 1,
      density = 2,
      stand = "p",
      model_msca = "original"
    ),
    regexp = "Unknown argument\\(s\\) in \\.\\.\\.",
    fixed = FALSE
  )
})

test_that("t_mcsa enforces strict numeric FFMC and DMC", {
  expect_error(
    t_mcsa(
      FFMC = "85",
      DMC = 30,
      season = 1,
      density = 2,
      stand = "p"
    ),
    regexp = "FFMC|numeric|number|Must be",
    fixed = FALSE
  )

  expect_error(
    t_mcsa(
      FFMC = 85,
      DMC = "30",
      season = 1,
      density = 2,
      stand = "p"
    ),
    regexp = "DMC|numeric|number|Must be",
    fixed = FALSE
  )
})

test_that("t_mcsa errors on invalid categorical inputs", {
  expect_error(
    t_mcsa(
      FFMC = 85,
      DMC = 30,
      season = "winter",
      density = 2,
      stand = "p"
    ),
    regexp = "season|subset|choice|Must be",
    fixed = FALSE
  )

  expect_error(
    t_mcsa(
      FFMC = 85,
      DMC = 30,
      season = 1,
      density = "heavy",
      stand = "p"
    ),
    regexp = "density|subset|choice|Must be",
    fixed = FALSE
  )

  expect_error(
    t_mcsa(
      FFMC = 85,
      DMC = 30,
      season = 1,
      density = 2,
      stand = "fir"
    ),
    regexp = "stand|subset|choice|Must be",
    fixed = FALSE
  )
})

test_that("t_mcsa enforces scalar inputs", {
  expect_error(
    t_mcsa(
      FFMC = c(85, 86),
      DMC = 30,
      season = 1,
      density = 2,
      stand = "p"
    ),
    regexp = "Must have length 1|Inputs must have length 1",
    fixed = FALSE
  )

  expect_error(
    t_mcsa(
      FFMC = 85,
      DMC = c(30, 31),
      season = 1,
      density = 2,
      stand = "p"
    ),
    regexp = "Must have length 1|Inputs must have length 1",
    fixed = FALSE
  )
})

# t_pCFO ----
test_that("t_pCFO returns a scalar numeric probability in [0, 1]", {
  out <- t_pCFO(WS10 = 11, mcsa = 10, FSG = 6, SFC = 2)

  expect_type(out, "double")
  expect_length(out, 1L)
  expect_true(is.finite(out))
  expect_true(out >= 0 && out <= 1)
  expect_equal(out, round(out, 2))
})

test_that("t_pCFO errors if both mcF and mcsa are NULL", {
  expect_error(
    t_pCFO(WS10 = 11, FSG = 6, SFC = 2),
    regexp = "Either `mcsa` or `mcF` must be non-NULL",
    fixed = FALSE
  )
})

test_that("mcF overrides mcsa when both are provided", {
  out_mcsa <- t_pCFO(WS10 = 13, mcsa = 12.30, FSG = 6, SFC = 2)
  out_mcf  <- t_pCFO(WS10 = 13, mcF = 11.89, FSG = 6, SFC = 2)

  out_both <- t_pCFO(
    WS10 = 13,
    mcF = 11.89,
    mcsa = 12.30,
    FSG = 6,
    SFC = 2
  )

  expect_equal(out_both, out_mcf)
  expect_true(out_mcf != out_mcsa)
})

test_that("t_pCFO uses the expected default model by mc type", {
  out_def_mcsa <- t_pCFO(WS10 = 11, mcsa = 10, FSG = 6, SFC = 2)
  out_m11 <- t_pCFO(
    WS10 = 11,
    mcsa = 10,
    FSG = 6,
    SFC = 2,
    model_pCFO = 11L
  )

  expect_equal(out_def_mcsa, out_m11)

  out_def_mcf <- t_pCFO(WS10 = 13, mcF = 11.89, FSG = 6, SFC = 2)
  out_m10 <- t_pCFO(
    WS10 = 13,
    mcF = 11.89,
    FSG = 6,
    SFC = 2,
    model_pCFO = 10L
  )

  expect_equal(out_def_mcf, out_m10)
})

test_that("t_pCFO matches known values for common examples", {
  expect_equal(
    t_pCFO(WS10 = 11, mcsa = 10, FSG = 6, SFC = 2),
    0.18
  )

  expect_equal(
    t_pCFO(WS10 = 12, mcsa = 9, FSG = 6, SFC = 2),
    0.51
  )

  expect_equal(
    t_pCFO(WS10 = 13, mcsa = 8, FSG = 6, SFC = 2),
    0.86
  )

  expect_equal(
    t_pCFO(WS10 = 13, mcF = 11.89, FSG = 6, SFC = 2),
    0.19
  )

  expect_equal(
    t_pCFO(WS10 = 13, mcsa = 12.30, FSG = 6, SFC = 2),
    0.10
  )
})

test_that("model_pCFO changes output when explicitly set", {
  out_10 <- t_pCFO(
    WS10 = 13,
    mcF = 11.89,
    FSG = 6,
    SFC = 2,
    model_pCFO = 10L
  )

  out_7 <- t_pCFO(
    WS10 = 13,
    mcF = 11.89,
    FSG = 6,
    SFC = 2,
    model_pCFO = 7L
  )

  expect_true(out_10 != out_7)
})

test_that("t_pCFO errors on unknown ... argument names", {
  expect_error(
    t_pCFO(
      WS10 = 11,
      mcsa = 10,
      FSG = 6,
      SFC = 2,
      model_pCFOO = 11L
    ),
    regexp = "Unknown argument\\(s\\) in \\.\\.\\.",
    fixed = FALSE
  )
})

test_that("t_pCFO enforces strict numeric type for continuous inputs", {
  expect_error(
    t_pCFO(WS10 = "11", mcsa = 10, FSG = 6, SFC = 2),
    regexp = "WS10|numeric|number|Must be",
    fixed = FALSE
  )

  expect_error(
    t_pCFO(WS10 = 11, mcsa = "10", FSG = 6, SFC = 2),
    regexp = "\\bmc\\b|numeric|number|Must be",
    fixed = FALSE
  )

  expect_error(
    t_pCFO(WS10 = 11, mcsa = 10, FSG = "6", SFC = 2),
    regexp = "FSG|numeric|number|Must be",
    fixed = FALSE
  )

  expect_error(
    t_pCFO(WS10 = 11, mcsa = 10, FSG = 6, SFC = "2"),
    regexp = "SFC|numeric|number|Must be",
    fixed = FALSE
  )
})

test_that("t_pCFO enforces scalar inputs", {
  expect_error(
    t_pCFO(WS10 = c(11, 12), mcsa = 10, FSG = 6, SFC = 2),
    regexp = "Must have length 1|Inputs must have length 1",
    fixed = FALSE
  )

  expect_error(
    t_pCFO(WS10 = 11, mcsa = c(10, 11), FSG = 6, SFC = 2),
    regexp = "Must have length 1|Inputs must have length 1",
    fixed = FALSE
  )
})

# t_FT() ----
test_that("t_FT returns a single fire type code", {
  out <- t_FT(WS10 = 11, mcsa = 9, FSG = 6, SFC = 2, CBD = 0.1)

  expect_type(out, "character")
  expect_length(out, 1L)
  expect_true(out %in% c("S", "PC", "AC"))
})

test_that("t_FT matches expected fire types for basic examples", {
  expect_identical(
    t_FT(WS10 = 11, mcsa = 9, FSG = 6, SFC = 2, CBD = 0.1),
    "S"
  )

  expect_identical(
    t_FT(WS10 = 11, mcsa = 8, FSG = 6, SFC = 2, CBD = 0.1),
    "PC"
  )

  expect_identical(
    t_FT(WS10 = 11, mcsa = 8, FSG = 6, SFC = 2, CBD = 0.2),
    "AC"
  )
})

test_that("t_FT responds to CF_thresh as expected", {
  # At very low threshold, most cases should crown
  out_low <- t_FT(
    WS10 = 11,
    mcsa = 9,
    FSG = 6,
    SFC = 2,
    CBD = 0.2,
    CF_thresh = 0.01
  )

  # At very high threshold, most cases should stay surface
  out_high <- t_FT(
    WS10 = 11,
    mcsa = 9,
    FSG = 6,
    SFC = 2,
    CBD = 0.2,
    CF_thresh = 0.99
  )

  expect_true(out_low %in% c("PC", "AC"))
  expect_identical(out_high, "S")
})

test_that("mcF overrides mcsa when both are provided", {
  out_mcf <- t_FT(WS10 = 11, mcF = 8, FSG = 6, SFC = 2, CBD = 0.1)

  out_both <- t_FT(
    WS10 = 11,
    mcF = 8,
    mcsa = 20,
    FSG = 6,
    SFC = 2,
    CBD = 0.1
  )

  expect_identical(out_both, out_mcf)
})

test_that("t_FT errors on unknown ... argument names", {
  expect_error(
    t_FT(
      WS10 = 11,
      mcsa = 9,
      FSG = 6,
      SFC = 2,
      CBD = 0.1,
      model_cROs = 1L
    ),
    regexp = "Unknown argument\\(s\\) in \\.\\.\\.",
    fixed = FALSE
  )
})

test_that("t_FT enforces strict numeric types for continuous inputs", {
  expect_error(
    t_FT(WS10 = "11", mcsa = 9, FSG = 6, SFC = 2, CBD = 0.1),
    regexp = "WS10|numeric|number|Must be",
    fixed = FALSE
  )

  expect_error(
    t_FT(WS10 = 11, mcsa = "9", FSG = 6, SFC = 2, CBD = 0.1),
    regexp = "\\bmc\\b|numeric|number|Must be",
    fixed = FALSE
  )

  expect_error(
    t_FT(WS10 = 11, mcsa = 9, FSG = "6", SFC = 2, CBD = 0.1),
    regexp = "FSG|numeric|number|Must be",
    fixed = FALSE
  )

  expect_error(
    t_FT(WS10 = 11, mcsa = 9, FSG = 6, SFC = "2", CBD = 0.1),
    regexp = "SFC|numeric|number|Must be",
    fixed = FALSE
  )

  expect_error(
    t_FT(WS10 = 11, mcsa = 9, FSG = 6, SFC = 2, CBD = "0.1"),
    regexp = "CBD|numeric|number|Must be",
    fixed = FALSE
  )
})

test_that("t_FT enforces scalar inputs", {
  expect_error(
    t_FT(WS10 = c(11, 12), mcsa = 9, FSG = 6, SFC = 2, CBD = 0.1),
    regexp = "Must have length 1|Inputs must have length 1",
    fixed = FALSE
  )

  expect_error(
    t_FT(WS10 = 11, mcsa = 9, FSG = 6, SFC = c(2, 3), CBD = 0.1),
    regexp = "Must have length 1|Inputs must have length 1",
    fixed = FALSE
  )
})

test_that("t_FT fire type changes with model_pCFO in a known case", {
  # Same conditions; only pCFO model changes crown decision.
  # For these inputs, model 7 yields pCFO >= 0.5 (crown),
  # while model 11 yields pCFO < 0.5 (surface).
  out_7 <- t_FT(
    WS10 = 11,
    mcsa = 9,
    FSG = 6,
    SFC = 2,
    CBD = 0.1,
    model_pCFO = 7L,
    model_cROS = 1L,
    CF_thresh = 0.5
  )

  out_11 <- t_FT(
    WS10 = 11,
    mcsa = 9,
    FSG = 6,
    SFC = 2,
    CBD = 0.1,
    model_pCFO = 11L,
    model_cROS = 1L,
    CF_thresh = 0.5
  )

  expect_identical(out_7, "PC")
  expect_identical(out_11, "S")
})

test_that("t_FT fire type changes with model_cROS near CAC = 1 boundary", {
  # Choose CBD so CAC straddles 1 across cROS models.
  # With these inputs and model_pCFO = 11, pCFO >= 0.5 (crown).
  # model_cROS = 1 -> CAC > 1 (AC)
  # model_cROS = 2 -> CAC < 1 (PC)
  out_1 <- t_FT(
    WS10 = 11,
    mcsa = 8,
    FSG = 6,
    SFC = 2,
    CBD = 0.165,
    model_pCFO = 11L,
    model_cROS = 1L,
    CF_thresh = 0.5
  )

  out_2 <- t_FT(
    WS10 = 11,
    mcsa = 8,
    FSG = 6,
    SFC = 2,
    CBD = 0.165,
    model_pCFO = 11L,
    model_cROS = 2L,
    CF_thresh = 0.5
  )

  expect_identical(out_1, "AC")
  expect_identical(out_2, "PC")
})

# t_ROS() ----
test_that("t_ROS returns a named list with expected types", {
  out <- t_ROS(WS10 = 11, mcsa = 9, FSG = 6, SFC = 2, CBD = 0.1)

  expect_type(out, "list")
  expect_length(out, 2L)
  expect_named(out, c("Type of Fire", "Composite Rate of Spread (m/min)"))

  expect_type(out[["Type of Fire"]], "character")
  expect_length(out[["Type of Fire"]], 1L)
  expect_true(out[["Type of Fire"]] %in% c("S", "PC", "AC"))

  expect_type(out[["Composite Rate of Spread (m/min)"]], "double")
  expect_length(out[["Composite Rate of Spread (m/min)"]], 1L)
  expect_true(is.finite(out[["Composite Rate of Spread (m/min)"]]))
})

test_that("t_ROS matches fire type from t_FT for same inputs", {
  ft <- t_FT(WS10 = 11, mcsa = 9, FSG = 6, SFC = 2, CBD = 0.1)

  out <- t_ROS(WS10 = 11, mcsa = 9, FSG = 6, SFC = 2, CBD = 0.1)

  expect_identical(out[["Type of Fire"]], ft)
})

test_that("t_ROS matches documented example fire types", {
  out_s <- t_ROS(WS10 = 11, mcsa = 9, FSG = 6, SFC = 2, CBD = 0.1)
  out_pc <- t_ROS(WS10 = 11, mcsa = 8, FSG = 6, SFC = 2, CBD = 0.1)
  out_ac <- t_ROS(WS10 = 11, mcsa = 8, FSG = 6, SFC = 2, CBD = 0.2)

  expect_identical(out_s[["Type of Fire"]], "S")
  expect_identical(out_pc[["Type of Fire"]], "PC")
  expect_identical(out_ac[["Type of Fire"]], "AC")
})

test_that("t_ROS uses expected ROS component when smooth_CFO is FALSE", {
  # Surface: ROS == sROS
  out_s <- t_ROS(WS10 = 11, mcsa = 9, FSG = 6, SFC = 2, CBD = 0.1)
  sros <- sROS(WS10 = 11, mc = 9, SFC = 2, model_sROS = 13L)

  expect_identical(out_s[["Type of Fire"]], "S")
  expect_equal(out_s[["Composite Rate of Spread (m/min)"]], round(sros, 1))

  # Active crown: ROS == cROS_A
  out_ac <- t_ROS(WS10 = 11, mcsa = 8, FSG = 6, SFC = 2, CBD = 0.2)
  crosa <- cROS_A(WS10 = 11, mc = 8, CBD = 0.2, model_cROS = 1L)

  expect_identical(out_ac[["Type of Fire"]], "AC")
  expect_equal(out_ac[["Composite Rate of Spread (m/min)"]], round(crosa, 1))

  # Passive crown: ROS == cROS_P
  out_pc <- t_ROS(WS10 = 11, mcsa = 8, FSG = 6, SFC = 2, CBD = 0.1)
  crosa2 <- cROS_A(WS10 = 11, mc = 8, CBD = 0.1, model_cROS = 1L)
  cac <- CAC(cROS_A = crosa2, CBD = 0.1)
  crosp <- cROS_P(cROS_A = crosa2, CAC = cac)

  expect_identical(out_pc[["Type of Fire"]], "PC")
  expect_equal(out_pc[["Composite Rate of Spread (m/min)"]], round(crosp, 1))
})

test_that("mcF overrides mcsa and changes default model_sROS", {
  out_mcsa <- t_ROS(WS10 = 13, mcsa = 12.3, FSG = 6, SFC = 2, CBD = 0.2)

  out_mcf <- t_ROS(WS10 = 13, mcF = 12.3, FSG = 6, SFC = 2, CBD = 0.2)

  out_both <- t_ROS(
    WS10 = 13,
    mcF = 12.3,
    mcsa = 8,
    FSG = 6,
    SFC = 2,
    CBD = 0.2
  )

  expect_identical(out_both, out_mcf)
  expect_false(isTRUE(all.equal(out_mcf, out_mcsa)))
})

test_that("t_ROS errors on unknown ... argument names", {
  expect_error(
    t_ROS(
      WS10 = 11,
      mcsa = 9,
      FSG = 6,
      SFC = 2,
      CBD = 0.1,
      model_sROs = 13L
    ),
    regexp = "Unknown argument\\(s\\) in \\.\\.\\.",
    fixed = FALSE
  )
})

test_that("t_ROS enforces strict numeric types and scalar inputs", {
  expect_error(
    t_ROS(WS10 = "11", mcsa = 9, FSG = 6, SFC = 2, CBD = 0.1),
    regexp = "WS10|numeric|number|Must be",
    fixed = FALSE
  )

  expect_error(
    t_ROS(WS10 = 11, mcsa = "9", FSG = 6, SFC = 2, CBD = 0.1),
    regexp = "\\bmc\\b|numeric|number|Must be",
    fixed = FALSE
  )

  expect_error(
    t_ROS(WS10 = c(11, 12), mcsa = 9, FSG = 6, SFC = 2, CBD = 0.1),
    regexp = "Must have length 1|Inputs must have length 1",
    fixed = FALSE
  )
})

test_that("t_ROS uses ROS_smooth when smooth_CFO is TRUE", {
  # Passive crown case
  out <- t_ROS(
    WS10 = 11,
    mcsa = 8,
    FSG = 6,
    SFC = 2,
    CBD = 0.1,
    smooth_CFO = TRUE
  )

  pcfo <- pCFO(
    WS10 = 11,
    mc = 8,
    FSG = 6,
    SFC = 2,
    model_pCFO = 11L,
    coefs = sysdata$coefs_pCFO
  )

  sros <- sROS(
    WS10 = 11,
    mc = 8,
    SFC = 2,
    model_sROS = 13L
  )

  crosa <- cROS_A(
    WS10 = 11,
    mc = 8,
    CBD = 0.1,
    model_cROS = 1L
  )

  cac <- CAC(cROS_A = crosa, CBD = 0.1)
  crosp <- cROS_P(cROS_A = crosa, CAC = cac)

  passive <- pcfo >= 0.5 && cac < 1

  expect_identical(out[["Type of Fire"]], "PC")

  expect_equal(
    out[["Composite Rate of Spread (m/min)"]],
    round(ROS_smooth(pcfo, passive, sros, crosp, crosa), 1)
  )
})

test_that("smooth_CFO changes ROS relative to instantaneous selection", {
  out_inst <- t_ROS(
    WS10 = 11,
    mcsa = 8,
    FSG = 6,
    SFC = 2,
    CBD = 0.1,
    smooth_CFO = FALSE
  )

  out_smooth <- t_ROS(
    WS10 = 11,
    mcsa = 8,
    FSG = 6,
    SFC = 2,
    CBD = 0.1,
    smooth_CFO = TRUE
  )

  expect_identical(out_inst[["Type of Fire"]], out_smooth[["Type of Fire"]])

  expect_false(
    isTRUE(
      all.equal(
        out_inst[["Composite Rate of Spread (m/min)"]],
        out_smooth[["Composite Rate of Spread (m/min)"]]
      )
    )
  )
})

# t_FMC() ----
test_that("t_FMC returns a scalar numeric FMC value", {
  skip_if_not_installed("cffdrs")

  out <- t_FMC(LAT = 54.13, LONG = 116.89, Dj = 178, ELV = NA_real_)

  expect_type(out, "double")
  expect_length(out, 1L)
  expect_true(is.finite(out))
  expect_equal(out, round(out, 2))
})

test_that("t_FMC supports NA vs numeric elevation", {
  skip_if_not_installed("cffdrs")

  out_na <- t_FMC(LAT = 54.13, LONG = 116.89, Dj = 178, ELV = NA_real_)
  out_elv <- t_FMC(LAT = 54.13, LONG = 116.89, Dj = 178, ELV = 1000)

  expect_true(is.finite(out_na))
  expect_true(is.finite(out_elv))
})

test_that("t_FMC enforces strict numeric types", {
  skip_if_not_installed("cffdrs")

  expect_error(
    t_FMC(LAT = "54.13", LONG = 116.89, Dj = 178, ELV = NA_real_),
    regexp = "LAT|numeric|number|Must be",
    fixed = FALSE
  )

  expect_error(
    t_FMC(LAT = 54.13, LONG = "116.89", Dj = 178, ELV = NA_real_),
    regexp = "LONG|numeric|number|Must be",
    fixed = FALSE
  )

  expect_error(
    t_FMC(LAT = 54.13, LONG = 116.89, Dj = "178", ELV = NA_real_),
    regexp = "Dj|integerish|numeric|Must be",
    fixed = FALSE
  )
})

test_that("t_FMC errors on invalid ranges", {
  skip_if_not_installed("cffdrs")

  expect_error(
    t_FMC(LAT = 40, LONG = 116.89, Dj = 178, ELV = NA_real_),
    regexp = "LAT|lower|41|Must be",
    fixed = FALSE
  )

  expect_error(
    t_FMC(LAT = 54.13, LONG = 116.89, Dj = 0, ELV = NA_real_),
    regexp = "Dj|lower|1|Must be",
    fixed = FALSE
  )

  expect_error(
    t_FMC(LAT = 54.13, LONG = 116.89, Dj = 367, ELV = NA_real_),
    regexp = "Dj|upper|366|Must be",
    fixed = FALSE
  )
})

test_that("t_FMC enforces scalar Dj input", {
  skip_if_not_installed("cffdrs")

  expect_error(
    t_FMC(LAT = 54.13, LONG = 116.89, Dj = c(178, 179), ELV = NA_real_),
    regexp = "Must have length 1|Inputs must have length 1",
    fixed = FALSE
  )
})

# t_SFC_FBP() ----
test_that("t_SFC_FBP returns a named list of length 10", {
  skip_if_not_installed("cffdrs")

  out <- t_SFC_FBP(BUI = 81, FFMC = 92, PC = 55)

  expect_type(out, "list")
  expect_length(out, 10L)

  expect_named(
    out,
    c(
      "C1",
      "C2/M3/M4",
      "C3/C4",
      "C5/C6",
      "C7",
      "D1/D2",
      "M1/M2",
      "S1",
      "S2",
      "S3"
    )
  )

  for (nm in names(out)) {
    expect_type(out[[nm]], "double")
    expect_length(out[[nm]], 1L)
    expect_true(is.finite(out[[nm]]))
    expect_equal(out[[nm]], round(out[[nm]], 2))
  }
})

test_that("t_SFC_FBP enforces strict numeric types", {
  skip_if_not_installed("cffdrs")

  expect_error(
    t_SFC_FBP(BUI = "81", FFMC = 92, PC = 55),
    regexp = "BUI|numeric|number|Must be",
    fixed = FALSE
  )

  expect_error(
    t_SFC_FBP(BUI = 81, FFMC = "92", PC = 55),
    regexp = "FFMC|numeric|number|Must be",
    fixed = FALSE
  )

  expect_error(
    t_SFC_FBP(BUI = 81, FFMC = 92, PC = "55"),
    regexp = "PC|numeric|number|Must be",
    fixed = FALSE
  )
})

test_that("t_SFC_FBP errors on invalid ranges", {
  skip_if_not_installed("cffdrs")

  expect_error(
    t_SFC_FBP(BUI = -1, FFMC = 92, PC = 55),
    regexp = "BUI|lower|Must be",
    fixed = FALSE
  )

  expect_error(
    t_SFC_FBP(BUI = 81, FFMC = 79, PC = 55),
    regexp = "FFMC|lower|80|Must be",
    fixed = FALSE
  )

  expect_error(
    t_SFC_FBP(BUI = 81, FFMC = 92, PC = 101),
    regexp = "PC|upper|100|Must be",
    fixed = FALSE
  )
})

test_that("t_SFC_FBP enforces scalar FFMC", {
  skip_if_not_installed("cffdrs")

  expect_error(
    t_SFC_FBP(BUI = 81, FFMC = c(92, 93), PC = 55),
    regexp = "Must have length 1|Inputs must have length 1",
    fixed = FALSE
  )
})

# t_SFC_deGroot() ----
test_that("t_SFC_deGroot returns a named list of length 2", {
  out <- t_SFC_deGroot(BUI = 78, FFL = 2.8, FWFL = 0.4)

  expect_type(out, "list")
  expect_length(out, 2L)

  expect_named(
    out,
    c("Forest Floor Fuel Consumption", "SFC")
  )

  expect_type(out[["Forest Floor Fuel Consumption"]], "double")
  expect_length(out[["Forest Floor Fuel Consumption"]], 1L)

  expect_type(out[["SFC"]], "double")
  expect_length(out[["SFC"]], 1L)

  expect_true(is.finite(out[["Forest Floor Fuel Consumption"]]))
  expect_true(is.finite(out[["SFC"]]))

  expect_equal(
    out[["Forest Floor Fuel Consumption"]],
    round(out[["Forest Floor Fuel Consumption"]], 2)
  )
  expect_equal(out[["SFC"]], round(out[["SFC"]], 2))
})

test_that("t_SFC_deGroot matches the documented equation", {
  BUI <- 78
  FFL <- 2.8
  FWFL <- 0.4

  out <- t_SFC_deGroot(BUI = BUI, FFL = FFL, FWFL = FWFL)

  fffc <- -0.176 + 0.156 * FFL + 0.015 * BUI
  sfc <- FWFL + fffc

  expect_equal(out[["Forest Floor Fuel Consumption"]], round(fffc, 2))
  expect_equal(out[["SFC"]], round(sfc, 2))
})

test_that("t_SFC_deGroot enforces strict numeric types", {
  expect_error(
    t_SFC_deGroot(BUI = "78", FFL = 2.8, FWFL = 0.4),
    regexp = "BUI|numeric|number|Must be",
    fixed = FALSE
  )

  expect_error(
    t_SFC_deGroot(BUI = 78, FFL = "2.8", FWFL = 0.4),
    regexp = "FFL|numeric|number|Must be",
    fixed = FALSE
  )

  expect_error(
    t_SFC_deGroot(BUI = 78, FFL = 2.8, FWFL = "0.4"),
    regexp = "FWFL|numeric|number|Must be",
    fixed = FALSE
  )
})

test_that("t_SFC_deGroot errors on invalid ranges", {
  expect_error(
    t_SFC_deGroot(BUI = 4, FFL = 2.8, FWFL = 0.4),
    regexp = "BUI|lower|5|Must be",
    fixed = FALSE
  )

  expect_error(
    t_SFC_deGroot(BUI = 78, FFL = 0.9, FWFL = 0.4),
    regexp = "FFL|lower|1|Must be",
    fixed = FALSE
  )

  expect_error(
    t_SFC_deGroot(BUI = 78, FFL = 2.8, FWFL = 2.1),
    regexp = "FWFL|upper|2|Must be",
    fixed = FALSE
  )
})

# ladder_standing_dead() ----
test_that("ladder_standing_dead returns a named list of length 2", {
  out <- ladder_standing_dead(cons = 1.1, cl = 5.1, FSG = 6.3)

  expect_type(out, "list")
  expect_length(out, 2L)

  expect_named(
    out,
    c("LFSG (m)", "Scaled SFC contribution, small snags (kg/m^2)")
  )

  expect_type(out[["LFSG (m)"]], "double")
  expect_length(out[["LFSG (m)"]], 1L)
  expect_true(is.finite(out[["LFSG (m)"]]))
  expect_equal(out[["LFSG (m)"]], round(out[["LFSG (m)"]], 2))

  expect_type(out[["Scaled SFC contribution, small snags (kg/m^2)"]], "double")
  expect_length(out[["Scaled SFC contribution, small snags (kg/m^2)"]], 1L)
  expect_true(is.finite(out[["Scaled SFC contribution, small snags (kg/m^2)"]]))
  expect_equal(
    out[["Scaled SFC contribution, small snags (kg/m^2)"]],
    round(out[["Scaled SFC contribution, small snags (kg/m^2)"]], 2)
  )
})

test_that("ladder_standing_dead behaves sensibly in key branches", {
  # Branch: cl < FSG - 0.5 -> effective_cl = cl -> zl = FSG - cl
  out1 <- ladder_standing_dead(cons = 1, cl = 4, FSG = 6)
  expect_equal(out1[["LFSG (m)"]], round(6 - 4, 2))

  # Branch: cl >= FSG - 0.5 -> effective_cl = FSG - 0.5
  # Therefore zl = 0.5
  out2 <- ladder_standing_dead(cons = 1, cl = 15, FSG = 6)
  expect_equal(out2[["LFSG (m)"]], 0.5)
})

test_that("ladder_standing_dead scales linearly with cons", {
  out1 <- ladder_standing_dead(cons = 1, cl = 5, FSG = 6)
  out2 <- ladder_standing_dead(cons = 2, cl = 5, FSG = 6)

  s1 <- out1[["Scaled SFC contribution, small snags (kg/m^2)"]]
  s2 <- out2[["Scaled SFC contribution, small snags (kg/m^2)"]]

  # Allow tiny rounding tolerance
  expect_equal(s2, round(2 * s1, 2))
})

test_that("ladder_standing_dead enforces strict numeric types", {
  expect_error(
    ladder_standing_dead(cons = "1.1", cl = 5.1, FSG = 6.3),
    regexp = "cons|numeric|number|Must be",
    fixed = FALSE
  )

  expect_error(
    ladder_standing_dead(cons = 1.1, cl = "5.1", FSG = 6.3),
    regexp = "cl|numeric|number|Must be",
    fixed = FALSE
  )

  expect_error(
    ladder_standing_dead(cons = 1.1, cl = 5.1, FSG = "6.3"),
    regexp = "FSG|numeric|number|Must be",
    fixed = FALSE
  )
})

test_that("ladder_standing_dead errors on invalid ranges", {
  expect_error(
    ladder_standing_dead(cons = 0.09, cl = 5.1, FSG = 6.3),
    regexp = "cons|lower|0\\.1|Must be",
    fixed = FALSE
  )

  expect_error(
    ladder_standing_dead(cons = 1.1, cl = 0.49, FSG = 6.3),
    regexp = "cl|lower|0\\.5|Must be",
    fixed = FALSE
  )

  expect_error(
    ladder_standing_dead(cons = 1.1, cl = 5.1, FSG = 0.49),
    regexp = "FSG|lower|0\\.5|Must be",
    fixed = FALSE
  )
})

# ladder_midstory_saplings() ----
test_that("ladder_midstory_saplings returns a named list of length 5", {
  out <- ladder_midstory_saplings(
    hs = 4.5,
    zs = 1.3,
    zp = 7,
    lnfl = 0.4,
    sapling_FMC = 120,
    actual_SFC = 2.8
  )

  expect_type(out, "list")
  expect_length(out, 5L)

  expect_named(
    out,
    c(
      "Sapling crown centroid (m)",
      "FSG (m)",
      "Sapling false-SFC (kg/m^2)",
      "SFC scaled to crown centroid (kg/m^2)",
      "Total false-SFC (kg/m^2)"
    )
  )

  for (nm in names(out)) {
    expect_type(out[[nm]], "double")
    expect_length(out[[nm]], 1L)
    expect_true(is.finite(out[[nm]]))
    expect_equal(out[[nm]], round(out[[nm]], 2))
  }
})

test_that("ladder_midstory_saplings matches its documented equations", {
  hs <- 4.5
  zs <- 1.3
  zp <- 7
  lnfl <- 0.4
  sapling_FMC <- 120
  actual_SFC <- 2.8

  out <- ladder_midstory_saplings(
    hs = hs,
    zs = zs,
    zp = zp,
    lnfl = lnfl,
    sapling_FMC = sapling_FMC,
    actual_SFC = actual_SFC
  )

  cs <- zs + (hs - zs) / 2
  fsg <- if ((zp - cs) < 0.5) 0.5 else (zp - cs)
  deltah <- (16.52 - 0.057 * sapling_FMC) / 16
  sap_sfcf <- deltah * lnfl * 1.5 * 3.1
  sfc_cs <- (fsg / zp)^1.5 * actual_SFC
  total <- sap_sfcf + sfc_cs

  expect_equal(out[["Sapling crown centroid (m)"]], round(cs, 2))
  expect_equal(out[["FSG (m)"]], round(fsg, 2))
  expect_equal(out[["Sapling false-SFC (kg/m^2)"]], round(sap_sfcf, 2))
  expect_equal(
    out[["SFC scaled to crown centroid (kg/m^2)"]],
    round(sfc_cs, 2)
  )
  expect_equal(out[["Total false-SFC (kg/m^2)"]], round(total, 2))
})

test_that("ladder_midstory_saplings enforces hs > zs", {
  expect_error(
    ladder_midstory_saplings(
      hs = 2,
      zs = 2,
      zp = 7,
      lnfl = 0.4,
      sapling_FMC = 120,
      actual_SFC = 2.8
    ),
    regexp = "hs.*greater than.*zs|greater than",
    fixed = FALSE
  )

  expect_error(
    ladder_midstory_saplings(
      hs = 2,
      zs = 2.1,
      zp = 7,
      lnfl = 0.4,
      sapling_FMC = 120,
      actual_SFC = 2.8
    ),
    regexp = "hs.*greater than.*zs|greater than",
    fixed = FALSE
  )
})

test_that("ladder_midstory_saplings enforces scalar inputs", {
  expect_error(
    ladder_midstory_saplings(
      hs = c(4.5, 4.6),
      zs = 1.3,
      zp = 7,
      lnfl = 0.4,
      sapling_FMC = 120,
      actual_SFC = 2.8
    ),
    regexp = "Must have length 1|Inputs must have length 1",
    fixed = FALSE
  )
})

test_that("ladder_midstory_saplings errors on invalid ranges", {
  expect_error(
    ladder_midstory_saplings(
      hs = 0.49,
      zs = 1.3,
      zp = 7,
      lnfl = 0.4,
      sapling_FMC = 120,
      actual_SFC = 2.8
    ),
    regexp = "hs|lower|0\\.5|Must be",
    fixed = FALSE
  )

  expect_error(
    ladder_midstory_saplings(
      hs = 4.5,
      zs = 1.3,
      zp = 7,
      lnfl = 0.09,
      sapling_FMC = 120,
      actual_SFC = 2.8
    ),
    regexp = "lnfl|lower|0\\.1|Must be",
    fixed = FALSE
  )

  expect_error(
    ladder_midstory_saplings(
      hs = 4.5,
      zs = 1.3,
      zp = 7,
      lnfl = 0.4,
      sapling_FMC = 121,
      actual_SFC = 2.8
    ),
    regexp = "sapling_FMC|upper|120|Must be",
    fixed = FALSE
  )
})
