# Comprehensive unit test suite for exported functions in tools.R

# t_mcSeason ----
test_that("t_mcSeason returns a length-1 numeric scalar", {
  out <- t_mcSeason(5, 11)
  expect_type(out, "double")
  expect_length(out, 1L)
})

test_that("t_mcSeason correctly classifies spring dates (Jan 1 – May 31)", {
  expect_equal(t_mcSeason(1, 1), 1)
  expect_equal(t_mcSeason(3, 15), 1)
  expect_equal(t_mcSeason(5, 31), 1)
})

test_that("t_mcSeason correctly classifies spring–summer transition (Jun 1 –
          Jun 15)", {
  expect_equal(t_mcSeason(6, 1), 1.5)
  expect_equal(t_mcSeason(6, 7), 1.5)
  expect_equal(t_mcSeason(6, 15), 1.5)
})

test_that("t_mcSeason correctly classifies summer dates (Jun 16 – Aug 31)", {
  expect_equal(t_mcSeason(6, 16), 2)
  expect_equal(t_mcSeason(7, 10), 2)
  expect_equal(t_mcSeason(8, 31), 2)
})

test_that("t_mcSeason correctly classifies fall dates (Sep 1 – Dec 31)", {
  expect_equal(t_mcSeason(9, 1), 3)
  expect_equal(t_mcSeason(10, 15), 3)
  expect_equal(t_mcSeason(12, 31), 3)
})

test_that("t_mcSeason correctly handles boundary transitions", {
  expect_equal(t_mcSeason(5, 31), 1)
  expect_equal(t_mcSeason(6, 1), 1.5)

  expect_equal(t_mcSeason(6, 15), 1.5)
  expect_equal(t_mcSeason(6, 16), 2)

  expect_equal(t_mcSeason(8, 31), 2)
  expect_equal(t_mcSeason(9, 1), 3)
})

test_that("t_mcSeason allows February 29 (leap year handling)", {
  expect_equal(t_mcSeason(2, 29), 1)
})

test_that("t_mcSeason rejects invalid calendar dates", {
  expect_error(
    t_mcSeason(2, 30),
    "Invalid month/day combination"
  )
  expect_error(
    t_mcSeason(4, 31),
    "Invalid month/day combination"
  )
  expect_error(
    t_mcSeason(11, 31),
    "Invalid month/day combination"
  )
})

test_that("t_mcSeason rejects invalid month values", {
  expect_error(t_mcSeason(0, 15), "month")
  expect_error(t_mcSeason(13, 15), "month")
})

test_that("t_mcSeason rejects invalid day values", {
  expect_error(t_mcSeason(6, 0), "day")
  expect_error(t_mcSeason(6, 32), "day")
})

test_that("t_mcSeason rejects non-integerish inputs", {
  expect_error(t_mcSeason(6.5, 10), "month")
  expect_error(t_mcSeason(6, 10.2), "day")
  expect_error(t_mcSeason("6", 10), "month")
  expect_error(t_mcSeason(6, "10"), "day")
})

test_that("t_mcSeason rejects vector inputs", {
  expect_error(t_mcSeason(c(6, 7), 10), "month")
  expect_error(t_mcSeason(6, c(10, 11)), "day")
})

test_that("t_mcSeason is deterministic for identical inputs", {
  expect_identical(t_mcSeason(8, 16), t_mcSeason(8, 16))
})

# t_mcDensity ----
test_that("t_mcDensity returns a length-1 numeric scalar", {
  out <- t_mcDensity(50)
  expect_type(out, "double")
  expect_length(out, 1L)
})

test_that("t_mcDensity classifies light canopy density (<= 45)", {
  expect_equal(t_mcDensity(20), 1)
  expect_equal(t_mcDensity(30), 1)
  expect_equal(t_mcDensity(45), 1)
})

test_that("t_mcDensity classifies moderate canopy density (46–60)", {
  expect_equal(t_mcDensity(46), 2)
  expect_equal(t_mcDensity(50), 2)
  expect_equal(t_mcDensity(60), 2)
})

test_that("t_mcDensity classifies dense canopy density (> 60)", {
  expect_equal(t_mcDensity(61), 3)
  expect_equal(t_mcDensity(70), 3)
  expect_equal(t_mcDensity(100), 3)
})

test_that("t_mcDensity handles boundary transitions correctly", {
  expect_equal(t_mcDensity(45), 1)
  expect_equal(t_mcDensity(46), 2)
  expect_equal(t_mcDensity(60), 2)
  expect_equal(t_mcDensity(61), 3)
})

test_that("t_mcDensity rejects values below minimum canopy closure", {
  expect_error(
    t_mcDensity(19),
    "canopy_closure"
  )
})

test_that("t_mcDensity rejects values above maximum canopy closure", {
  expect_error(
    t_mcDensity(101),
    "canopy_closure"
  )
})

test_that("t_mcDensity rejects non-numeric inputs", {
  expect_error(t_mcDensity("50"), "canopy_closure")
  expect_error(t_mcDensity(TRUE), "canopy_closure")
  expect_error(t_mcDensity(factor(50)), "canopy_closure")
})

test_that("t_mcDensity rejects NA, NaN, and infinite values", {
  expect_error(t_mcDensity(NA), "canopy_closure")
  expect_error(t_mcDensity(NaN), "canopy_closure")
  expect_error(t_mcDensity(Inf), "canopy_closure")
  expect_error(t_mcDensity(-Inf), "canopy_closure")
})

test_that("t_mcDensity rejects vector inputs", {
  expect_error(
    t_mcDensity(c(30, 40)),
    "canopy_closure"
  )
})

test_that("t_mcDensity is deterministic for identical inputs", {
  expect_identical(t_mcDensity(70), t_mcDensity(70))
})

# t_mcF ----
test_that("t_mcF returns correct numeric values for representative FFMC
          inputs", {
  expect_equal(t_mcF(80), round(147.2 * (101 - 80) / (59.5 + 80), 2))
  expect_equal(t_mcF(84), round(147.2 * (101 - 84) / (59.5 + 84), 2))
  expect_equal(t_mcF(89), round(147.2 * (101 - 89) / (59.5 + 89), 2))
  expect_equal(t_mcF(92), round(147.2 * (101 - 92) / (59.5 + 92), 2))
})

test_that("t_mcF returns a length-1 numeric value rounded to two decimals", {
  out <- t_mcF(85)
  expect_type(out, "double")
  expect_length(out, 1)
  expect_equal(out, round(out, 2))
})

test_that("t_mcF enforces lower FFMC bound", {
  expect_error(
    t_mcF(79.999),
    "FFMC.*>=\\s*80"
  )
})

test_that("t_mcF enforces upper FFMC bound", {
  expect_error(
    t_mcF(99.001),
    "FFMC.*<=\\s*99"
  )
})

test_that("t_mcF rejects NA, NaN, and infinite FFMC values", {
  expect_error(t_mcF(NA), "FFMC")
  expect_error(t_mcF(NaN), "FFMC")
  expect_error(t_mcF(Inf), "FFMC")
  expect_error(t_mcF(-Inf), "FFMC")
})

test_that("t_mcF rejects non-numeric FFMC inputs", {
  expect_error(t_mcF("85"), "FFMC")
  expect_error(t_mcF(TRUE), "FFMC")
  expect_error(t_mcF(factor(85)), "FFMC")
})

test_that("t_mcF rejects vector inputs", {
  expect_error(
    t_mcF(c(85, 86)),
    "FFMC"
  )
})

test_that("t_mcF produces monotonically decreasing moisture with increasing
          FFMC", {
  mc_low <- t_mcF(80)
  mc_mid <- t_mcF(85)
  mc_high <- t_mcF(90)
  expect_gt(mc_low, mc_mid)
  expect_gt(mc_mid, mc_high)
})

test_that("t_mcF is deterministic for identical inputs", {
  expect_identical(t_mcF(87), t_mcF(87))
})

# t_mcsa ----
test_that("t_mcsa() works", {
  expect_identical(t_mcsa(80, 20, 1, 3, "s"), 23.45)
  expect_identical(t_mcsa(84, 30, 1, 3, "p"), 16.33)
  expect_identical(t_mcsa(89, 40, 2, 2, "p"), 12.3)
  expect_identical(t_mcsa(92, 60, 2, 1, "p"), 7.5)
  expect_error(t_mcsa(79, 60, 2, 1, "p"))
  expect_error(t_mcsa(100, 60, 2, 1, "p"))
  expect_error(t_mcsa(92, 4, 2, 1, "p"))
  expect_error(t_mcsa(92, 251, 2, 1, "p"))
  expect_error(t_mcsa(92, 60, 0, 1, "p"))
  expect_error(t_mcsa(92, 60, 1.6, 1, "p"))
  expect_error(t_mcsa(92, 60, 4, 1, "p"))
  expect_error(t_mcsa(92, 60, 2, 0, "p"))
  expect_error(t_mcsa(92, 60, 2, 1.6, "p"))
  expect_error(t_mcsa(92, 60, 2, 4, "p"))
  expect_error(t_mcsa(92, 60, 2, 1, 1))
  expect_error(t_mcsa(92, 60, 2, 1, "douglas fir"))
  expect_error(t_mcsa(NULL, 60, 2, 1, "p"))
  expect_error(t_mcsa(92, NULL, 2, 1, "p"))
  expect_error(t_mcsa(92, 60, NULL, 1, "p"))
  expect_error(t_mcsa(92, 60, 2, NULL, "p"))
  expect_error(t_mcsa(92, 60, 2, 1, NULL))
  expect_error(t_mcsa(NA, 60, 2, 1, "p"))
  expect_error(t_mcsa(92, NA, 2, 1, "p"))
  expect_error(t_mcsa(92, 60, NA, 1, "p"))
  expect_error(t_mcsa(92, 60, 2, NA, "p"))
  expect_error(t_mcsa(92, 60, 2, 1, NA))
  expect_error(t_mcsa("", 60, 2, 1, "p"))
  expect_error(t_mcsa(92, "", 2, 1, "p"))
  expect_error(t_mcsa(92, 60, "", 1, "p"))
  expect_error(t_mcsa(92, 60, 2, "", "p"))
  expect_error(t_mcsa(92, 60, 2, 1, ""))
})

test_that("t_pCFO() works", {
  expect_identical(t_pCFO(mcsa = 10, FSG = 6, SFC = 2, ws = 11), 0.18)
  expect_identical(t_pCFO(mcsa = 9, FSG = 6, SFC = 2, ws = 12), 0.51)
  expect_identical(t_pCFO(mcsa = 8, FSG = 6, SFC = 2, ws = 13), 0.86)
  expect_identical(t_pCFO(mcF = 10, FSG = 6, SFC = 2, ws = 11), 0.25)
  expect_identical(t_pCFO(mcF = 9, FSG = 6, SFC = 2, ws = 12), 0.6)
  expect_identical(t_pCFO(mcF = 8, FSG = 6, SFC = 2, ws = 13), 0.88)
  expect_identical(t_pCFO(mcsa = 8, mcF = 8, FSG = 6, SFC = 2, ws = 13), 0.88)
  expect_error(t_pCFO(mcsa = 2.9, FSG = 6, SFC = 2, ws = 13))
  expect_error(t_pCFO(mcsa = 20.1, FSG = 6, SFC = 2, ws = 13))
  expect_error(t_pCFO(mcF = 2.9, FSG = 6, SFC = 2, ws = 13))
  expect_error(t_pCFO(mcF = 20.1, FSG = 6, SFC = 2, ws = 13))
  expect_error(t_pCFO(mcsa = 8, FSG = 0.4, SFC = 2, ws = 13))
  expect_error(t_pCFO(mcsa = 8, FSG = 20.1, SFC = 2, ws = 13))
  expect_error(t_pCFO(mcsa = 8, FSG = 6, SFC = 0, ws = 13))
  expect_error(t_pCFO(mcsa = 8, FSG = 6, SFC = 6.1, ws = 13))
  expect_error(t_pCFO(mcsa = 8, FSG = 6, SFC = 2, ws = -1))
  expect_error(t_pCFO(mcsa = 8, FSG = 6, SFC = 2, ws = 61))
  expect_error(t_pCFO(mcsa = NULL, mcF = NULL, FSG = 6, SFC = 2, ws = 13))
  expect_error(t_pCFO(mcsa = 8, FSG = NULL, SFC = 2, ws = 13))
  expect_error(t_pCFO(mcsa = 8, FSG = 6, SFC = NULL, ws = 13))
  expect_error(t_pCFO(mcsa = 8, FSG = 6, SFC = 2, ws = NULL))
  expect_error(t_pCFO(mcsa = NA, mcF = NA, FSG = 6, SFC = 2, ws = 13))
  expect_error(t_pCFO(mcsa = 8, FSG = NA, SFC = 2, ws = 13))
  expect_error(t_pCFO(mcsa = 8, FSG = 6, SFC = NA, ws = 13))
  expect_error(t_pCFO(mcsa = 8, FSG = 6, SFC = 2, ws = NA))
  expect_error(t_pCFO(mcsa = "", mcF = "", FSG = 6, SFC = 2, ws = 13))
  expect_error(t_pCFO(mcsa = 8, FSG = "", SFC = 2, ws = 13))
  expect_error(t_pCFO(mcsa = 8, FSG = 6, SFC = "", ws = 13))
  expect_error(t_pCFO(mcsa = 8, FSG = 6, SFC = 2, ws = ""))
})

test_that("t_FT() works", {
  expect_identical(t_FT(mcsa = 9, FSG = 6, SFC = 2, CBD = 0.1, ws = 11), "S")
  expect_identical(t_FT(mcsa = 8, FSG = 6, SFC = 2, CBD = 0.1, ws = 11), "PC")
  expect_identical(t_FT(mcsa = 8, FSG = 6, SFC = 2, CBD = 0.2, ws = 11), "AC")
  expect_identical(t_FT(mcsa = 8, CBD = 0.2, ws = 11, CF_thresh = 0.6), "S")
  expect_error(t_FT(mcsa = 2.9, FSG = 6, SFC = 2, CBD = 0.1, ws = 13))
  expect_error(t_FT(mcsa = 20.1, FSG = 6, SFC = 2, CBD = 0.1, ws = 13))
  expect_error(t_FT(mcF = 2.9, FSG = 6, SFC = 2, CBD = 0.1, ws = 13))
  expect_error(t_FT(mcF = 20.1, FSG = 6, SFC = 2, CBD = 0.1, ws = 13))
  expect_error(t_FT(mcsa = 8, FSG = 0.4, SFC = 2, CBD = 0.1, ws = 13))
  expect_error(t_FT(mcsa = 8, FSG = 20.1, SFC = 2, CBD = 0.1, ws = 13))
  expect_error(t_FT(mcsa = 8, FSG = 6, SFC = 0, CBD = 0.1, ws = 13))
  expect_error(t_FT(mcsa = 8, FSG = 6, SFC = 6.1, CBD = 0.1, ws = 13))
  expect_error(t_FT(mcsa = 8, FSG = 6, SFC = 2, CBD = 0, ws = 13))
  expect_error(t_FT(mcsa = 8, FSG = 6, SFC = 2, CBD = 0.9, ws = 13))
  expect_error(t_FT(mcsa = 8, FSG = 6, SFC = 2, CBD = 0.1, ws = -1))
  expect_error(t_FT(mcsa = 8, FSG = 6, SFC = 2, CBD = 0.1, ws = 61))
  expect_error(t_FT(mcsa = 8, CBD = 0.2, ws = 11, CF_thresh = -1))
  expect_error(t_FT(mcsa = 8, CBD = 0.2, ws = 11, CF_thresh = 1.1))
  expect_error(t_FT(mcsa = NULL, mcF = NULL, FSG = 6, SFC = 2, ws = 13))
  expect_error(t_FT(mcsa = 8, FSG = NULL, SFC = 2, CBD = 0.1, ws = 13))
  expect_error(t_FT(mcsa = 8, FSG = 6, SFC = NULL, CBD = 0.1, ws = 13))
  expect_error(t_FT(mcsa = 8, FSG = 6, SFC = 2, CBD = NULL, ws = 13))
  expect_error(t_FT(mcsa = 8, FSG = 6, SFC = 2, CBD = 0.1, ws = NULL))
  expect_error(t_FT(mcsa = 8, CBD = 0.2, ws = 11, CF_thresh = NULL))
  expect_error(t_FT(mcsa = NA, mcF = NA, FSG = 6, SFC = 2, ws = 13))
  expect_error(t_FT(mcsa = 8, FSG = NA, SFC = 2, CBD = 0.1, ws = 13))
  expect_error(t_FT(mcsa = 8, FSG = 6, SFC = NA, CBD = 0.1, ws = 13))
  expect_error(t_FT(mcsa = 8, FSG = 6, SFC = 2, CBD = NA, ws = 13))
  expect_error(t_FT(mcsa = 8, FSG = 6, SFC = 2, CBD = 0.1, ws = NA))
  expect_error(t_FT(mcsa = 8, CBD = 0.2, ws = 11, CF_thresh = NA))
  expect_error(t_FT(mcsa = "", mcF = "", FSG = 6, SFC = 2, ws = 13))
  expect_error(t_FT(mcsa = 8, FSG = "", SFC = 2, CBD = 0.1, ws = 13))
  expect_error(t_FT(mcsa = 8, FSG = 6, SFC = "", CBD = 0.1, ws = 13))
  expect_error(t_FT(mcsa = 8, FSG = 6, SFC = 2, CBD = "", ws = 13))
  expect_error(t_FT(mcsa = 8, FSG = 6, SFC = 2, CBD = 0.1, ws = ""))
  expect_error(t_FT(mcsa = 8, CBD = 0.2, ws = 11, CF_thresh = ""))
})

test_that("t_ROS() works", {
  expect_snapshot(t_ROS(mcsa = 9, FSG = 6, SFC = 2, CBD = 0.1, ws = 11))
  expect_snapshot(t_ROS(mcsa = 8, FSG = 6, SFC = 2, CBD = 0.1, ws = 11))
  expect_snapshot(t_ROS(mcsa = 8, FSG = 6, SFC = 2, CBD = 0.2, ws = 11))
  expect_snapshot(t_ROS(mcsa = 8, CBD = 0.2, ws = 11, CF_thresh = 0.6))
  expect_error(t_ROS(mcsa = 2.9, FSG = 6, SFC = 2, CBD = 0.1, ws = 13))
  expect_error(t_ROS(mcsa = 20.1, FSG = 6, SFC = 2, CBD = 0.1, ws = 13))
  expect_error(t_ROS(mcF = 2.9, FSG = 6, SFC = 2, CBD = 0.1, ws = 13))
  expect_error(t_ROS(mcF = 20.1, FSG = 6, SFC = 2, CBD = 0.1, ws = 13))
  expect_error(t_ROS(mcsa = 8, FSG = 0.4, SFC = 2, CBD = 0.1, ws = 13))
  expect_error(t_ROS(mcsa = 8, FSG = 20.1, SFC = 2, CBD = 0.1, ws = 13))
  expect_error(t_ROS(mcsa = 8, FSG = 6, SFC = 0, CBD = 0.1, ws = 13))
  expect_error(t_ROS(mcsa = 8, FSG = 6, SFC = 6.1, CBD = 0.1, ws = 13))
  expect_error(t_ROS(mcsa = 8, FSG = 6, SFC = 2, CBD = 0, ws = 13))
  expect_error(t_ROS(mcsa = 8, FSG = 6, SFC = 2, CBD = 0.9, ws = 13))
  expect_error(t_ROS(mcsa = 8, FSG = 6, SFC = 2, CBD = 0.1, ws = -1))
  expect_error(t_ROS(mcsa = 8, FSG = 6, SFC = 2, CBD = 0.1, ws = 61))
  expect_error(t_ROS(mcsa = 8, CBD = 0.2, ws = 11, CF_thresh = -1))
  expect_error(t_ROS(mcsa = 8, CBD = 0.2, ws = 11, CF_thresh = 1.1))
  expect_error(t_ROS(mcsa = NULL, mcF = NULL, FSG = 6, SFC = 2, ws = 13))
  expect_error(t_ROS(mcsa = 8, FSG = NULL, SFC = 2, CBD = 0.1, ws = 13))
  expect_error(t_ROS(mcsa = 8, FSG = 6, SFC = NULL, CBD = 0.1, ws = 13))
  expect_error(t_ROS(mcsa = 8, FSG = 6, SFC = 2, CBD = NULL, ws = 13))
  expect_error(t_ROS(mcsa = 8, FSG = 6, SFC = 2, CBD = 0.1, ws = NULL))
  expect_error(t_ROS(mcsa = 8, CBD = 0.2, ws = 11, CF_thresh = NULL))
  expect_error(t_ROS(mcsa = NA, mcF = NA, FSG = 6, SFC = 2, ws = 13))
  expect_error(t_ROS(mcsa = 8, FSG = NA, SFC = 2, CBD = 0.1, ws = 13))
  expect_error(t_ROS(mcsa = 8, FSG = 6, SFC = NA, CBD = 0.1, ws = 13))
  expect_error(t_ROS(mcsa = 8, FSG = 6, SFC = 2, CBD = NA, ws = 13))
  expect_error(t_ROS(mcsa = 8, FSG = 6, SFC = 2, CBD = 0.1, ws = NA))
  expect_error(t_ROS(mcsa = 8, CBD = 0.2, ws = 11, CF_thresh = NA))
  expect_error(t_ROS(mcsa = "", mcF = "", FSG = 6, SFC = 2, ws = 13))
  expect_error(t_ROS(mcsa = 8, FSG = "", SFC = 2, CBD = 0.1, ws = 13))
  expect_error(t_ROS(mcsa = 8, FSG = 6, SFC = "", CBD = 0.1, ws = 13))
  expect_error(t_ROS(mcsa = 8, FSG = 6, SFC = 2, CBD = "", ws = 13))
  expect_error(t_ROS(mcsa = 8, FSG = 6, SFC = 2, CBD = 0.1, ws = ""))
  expect_error(t_ROS(mcsa = 8, CBD = 0.2, ws = 11, CF_thresh = ""))
})

test_that("t_FMC() works", {
  expect_identical(t_FMC(LAT = 48, LONG = 83.3, ELV = 100, Dj = 200), 120)
  expect_error(t_FMC(LAT = 40, LONG = 83.3, ELV = 100, Dj = 200))
  expect_error(t_FMC(LAT = 71, LONG = 83.3, ELV = 100, Dj = 200))
  expect_error(t_FMC(LAT = 48, LONG = 51, ELV = 100, Dj = 200))
  expect_error(t_FMC(LAT = 48, LONG = 142, ELV = 100, Dj = 200))
  expect_error(t_FMC(LAT = 48, LONG = 83.3, ELV = -1, Dj = 200))
  expect_error(t_FMC(LAT = 48, LONG = 83.3, ELV = 2501, Dj = 200))
  expect_error(t_FMC(LAT = 48, LONG = 83.3, ELV = 100, Dj = 0))
  expect_error(t_FMC(LAT = 48, LONG = 83.3, ELV = 100, Dj = 367))
  expect_error(t_FMC(LAT = "", LONG = 83.3, ELV = 100, Dj = 200))
  expect_error(t_FMC(LAT = 48, LONG = "", ELV = 100, Dj = 200))
  expect_error(t_FMC(LAT = 48, LONG = 83.3, ELV = "", Dj = 200))
  expect_error(t_FMC(LAT = 48, LONG = 83.3, ELV = 100, Dj = ""))
  expect_error(t_FMC(LAT = NA, LONG = 83.3, ELV = 100, Dj = 200))
  expect_error(t_FMC(LAT = 48, LONG = NA, ELV = 100, Dj = 200))
  expect_error(t_FMC(LAT = 48, LONG = 83.3, ELV = 100, Dj = NA))
  expect_error(t_FMC(LAT = NULL, LONG = 83.3, ELV = 100, Dj = 200))
  expect_error(t_FMC(LAT = 48, LONG = NULL, ELV = 100, Dj = 200))
  expect_error(t_FMC(LAT = 48, LONG = 83.3, ELV = NULL, Dj = 200))
  expect_error(t_FMC(LAT = 48, LONG = 83.3, ELV = 100, Dj = NULL))
})

test_that("t_SFC_FBP() works", {
  expect_snapshot(t_SFC_FBP(BUI = 85, FFMC = 91, PC = 40))
  expect_error(t_SFC_FBP(BUI = -1, FFMC = 91, PC = 40))
  expect_error(t_SFC_FBP(BUI = 201, FFMC = 91, PC = 40))
  expect_error(t_SFC_FBP(BUI = 85, FFMC = 79, PC = 40))
  expect_error(t_SFC_FBP(BUI = 85, FFMC = 100, PC = 40))
  expect_error(t_SFC_FBP(BUI = 85, FFMC = 91, PC = -1))
  expect_error(t_SFC_FBP(BUI = 85, FFMC = 91, PC = 101))
  expect_error(t_SFC_FBP(BUI = "", FFMC = 91, PC = 40))
  expect_error(t_SFC_FBP(BUI = 85, FFMC = "", PC = 40))
  expect_error(t_SFC_FBP(BUI = 85, FFMC = 91, PC = ""))
  expect_error(t_SFC_FBP(BUI = NA, FFMC = 91, PC = 40))
  expect_error(t_SFC_FBP(BUI = 85, FFMC = NA, PC = 40))
  expect_error(t_SFC_FBP(BUI = 85, FFMC = 91, PC = NA))
  expect_error(t_SFC_FBP(BUI = NULL, FFMC = 91, PC = 40))
  expect_error(t_SFC_FBP(BUI = 85, FFMC = NULL, PC = 40))
  expect_error(t_SFC_FBP(BUI = 85, FFMC = 91, PC = NULL))
})

test_that("t_SFC_deGroot() works", {
  expect_snapshot(t_SFC_deGroot(BUI = 85, FFL = 3.5, FWFL = 0.3))
  expect_error(t_SFC_deGroot(BUI = -1, FFL = 3.5, FWFL = 0.3))
  expect_error(t_SFC_deGroot(BUI = 201, FFL = 3.5, FWFL = 0.3))
  expect_error(t_SFC_deGroot(BUI = 85, FFL = 0.5, FWFL = 0.3))
  expect_error(t_SFC_deGroot(BUI = 85, FFL = 5.5, FWFL = 0.3))
  expect_error(t_SFC_deGroot(BUI = 85, FFL = 3.5, FWFL = -1))
  expect_error(t_SFC_deGroot(BUI = 85, FFL = 3.5, FWFL = 2.5))
  expect_error(t_SFC_deGroot(BUI = "", FFL = 3.5, FWFL = 0.3))
  expect_error(t_SFC_deGroot(BUI = 85, FFL = "", FWFL = 0.3))
  expect_error(t_SFC_deGroot(BUI = 85, FFL = 3.5, FWFL = ""))
  expect_error(t_SFC_deGroot(BUI = NA, FFL = 3.5, FWFL = 0.3))
  expect_error(t_SFC_deGroot(BUI = 85, FFL = NA, FWFL = 0.3))
  expect_error(t_SFC_deGroot(BUI = 85, FFL = 3.5, FWFL = NA))
  expect_error(t_SFC_deGroot(BUI = NULL, FFL = 3.5, FWFL = 0.3))
  expect_error(t_SFC_deGroot(BUI = 85, FFL = NULL, FWFL = 0.3))
  expect_error(t_SFC_deGroot(BUI = 85, FFL = 3.5, FWFL = NULL))
})

test_that("ladder_standing_dead() works", {
  expect_snapshot(ladder_standing_dead(cons = 0.2, cl = 4, FSG = 6))
  expect_error(ladder_standing_dead(cons = 0, cl = 4, FSG = 6))
  expect_error(ladder_standing_dead(cons = 11, cl = 4, FSG = 6))
  expect_error(ladder_standing_dead(cons = 0.2, cl = 0, FSG = 6))
  expect_error(ladder_standing_dead(cons = 0.2, cl = 16, FSG = 6))
  expect_error(ladder_standing_dead(cons = 0.2, cl = 4, FSG = 0))
  expect_error(ladder_standing_dead(cons = 0.2, cl = 4, FSG = 21))
  expect_error(ladder_standing_dead(cons = "", cl = 4, FSG = 6))
  expect_error(ladder_standing_dead(cons = 0.2, cl = "", FSG = 6))
  expect_error(ladder_standing_dead(cons = 0.2, cl = 4, FSG = ""))
  expect_error(ladder_standing_dead(cons = NA, cl = 4, FSG = 6))
  expect_error(ladder_standing_dead(cons = 0.2, cl = NA, FSG = 6))
  expect_error(ladder_standing_dead(cons = 0.2, cl = 4, FSG = NA))
  expect_error(ladder_standing_dead(cons = NULL, cl = 4, FSG = 6))
  expect_error(ladder_standing_dead(cons = 0.2, cl = NULL, FSG = 6))
  expect_error(ladder_standing_dead(cons = 0.2, cl = 4, FSG = NULL))
})

test_that("ladder_midstory_saplings() works", {
  expect_snapshot(ladder_midstory_saplings(
    hs          = 5,
    zs          = 1,
    zp          = 6,
    lnfl        = 0.5,
    sapling_FMC = 120,
    actual_SFC  = 2.7
  ))
  expect_error(ladder_midstory_saplings(
    hs          = "",
    zs          = 1,
    zp          = 6,
    lnfl        = 0.5,
    sapling_FMC = 120,
    actual_SFC  = 2.7
  ))
  expect_error(ladder_midstory_saplings(
    hs          = 5,
    zs          = "",
    zp          = 6,
    lnfl        = 0.5,
    sapling_FMC = 120,
    actual_SFC  = 2.7
  ))
  expect_error(ladder_midstory_saplings(
    hs          = 5,
    zs          = 1,
    zp          = "",
    lnfl        = 0.5,
    sapling_FMC = 120,
    actual_SFC  = 2.7
  ))
  expect_error(ladder_midstory_saplings(
    hs          = 5,
    zs          = 1,
    zp          = 6,
    lnfl        = "",
    sapling_FMC = 120,
    actual_SFC  = 2.7
  ))
  expect_error(ladder_midstory_saplings(
    hs          = 5,
    zs          = 1,
    zp          = 6,
    lnfl        = 0.5,
    sapling_FMC = "",
    actual_SFC  = 2.7
  ))
  expect_error(ladder_midstory_saplings(
    hs          = 5,
    zs          = 1,
    zp          = 6,
    lnfl        = 0.5,
    sapling_FMC = 120,
    actual_SFC  = ""
  ))
  expect_error(ladder_midstory_saplings(
    hs          = NA,
    zs          = 1,
    zp          = 6,
    lnfl        = 0.5,
    sapling_FMC = 120,
    actual_SFC  = 2.7
  ))
  expect_error(ladder_midstory_saplings(
    hs          = 5,
    zs          = NA,
    zp          = 6,
    lnfl        = 0.5,
    sapling_FMC = 120,
    actual_SFC  = 2.7
  ))
  expect_error(ladder_midstory_saplings(
    hs          = 5,
    zs          = 1,
    zp          = NA,
    lnfl        = 0.5,
    sapling_FMC = 120,
    actual_SFC  = 2.7
  ))
  expect_error(ladder_midstory_saplings(
    hs          = 5,
    zs          = 1,
    zp          = 6,
    lnfl        = NA,
    sapling_FMC = 120,
    actual_SFC  = 2.7
  ))
  expect_error(ladder_midstory_saplings(
    hs          = 5,
    zs          = 1,
    zp          = 6,
    lnfl        = 0.5,
    sapling_FMC = NA,
    actual_SFC  = 2.7
  ))
  expect_error(ladder_midstory_saplings(
    hs          = 5,
    zs          = 1,
    zp          = 6,
    lnfl        = 0.5,
    sapling_FMC = 120,
    actual_SFC  = NA
  ))
  expect_error(ladder_midstory_saplings(
    hs          = NULL,
    zs          = 1,
    zp          = 6,
    lnfl        = 0.5,
    sapling_FMC = 120,
    actual_SFC  = 2.7
  ))
  expect_error(ladder_midstory_saplings(
    hs          = 5,
    zs          = NULL,
    zp          = 6,
    lnfl        = 0.5,
    sapling_FMC = 120,
    actual_SFC  = 2.7
  ))
  expect_error(ladder_midstory_saplings(
    hs          = 5,
    zs          = 1,
    zp          = NULL,
    lnfl        = 0.5,
    sapling_FMC = 120,
    actual_SFC  = 2.7
  ))
  expect_error(ladder_midstory_saplings(
    hs          = 5,
    zs          = 1,
    zp          = 6,
    lnfl        = NULL,
    sapling_FMC = 120,
    actual_SFC  = 2.7
  ))
  expect_error(ladder_midstory_saplings(
    hs          = 5,
    zs          = 1,
    zp          = 6,
    lnfl        = 0.5,
    sapling_FMC = NULL,
    actual_SFC  = 2.7
  ))
  expect_error(ladder_midstory_saplings(
    hs          = 5,
    zs          = 1,
    zp          = 6,
    lnfl        = 0.5,
    sapling_FMC = 120,
    actual_SFC  = NULL
  ))
})
