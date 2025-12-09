test_that("t_mcSeason() works", {
  expect_identical(t_mcSeason(1, 1), 1)
  expect_identical(t_mcSeason(2, 29), 1)
  expect_identical(t_mcSeason(5, 31), 1)
  expect_identical(t_mcSeason(6, 1), 1.5)
  expect_identical(t_mcSeason(6, 15), 1.5)
  expect_identical(t_mcSeason(6, 16), 2)
  expect_identical(t_mcSeason(8, 31), 2)
  expect_identical(t_mcSeason(9, 1), 3)
  expect_error(t_mcSeason(0, 0))
  expect_error(t_mcSeason(0, 1))
  expect_error(t_mcSeason(1, 0))
  expect_error(t_mcSeason(2, 30))
  expect_error(t_mcSeason(13, 1))
  expect_error(t_mcSeason(1, 32))
  expect_error(t_mcSeason(1.5, 1))
  expect_error(t_mcSeason(1, 1.5))
  expect_error(t_mcSeason("", ""))
  expect_error(t_mcSeason(NA, NA))
  expect_error(t_mcSeason(NULL, NULL))
})

test_that("t_mcF() works", {
  expect_identical(t_mcF(80), 22.16)
  expect_identical(t_mcF(90), 10.83)
  expect_identical(t_mcF(91.5), 9.26)
  expect_identical(t_mcF(99), 1.86)
  expect_error(t_mcF(79))
  expect_error(t_mcF(100))
  expect_error(t_mcF(""))
  expect_error(t_mcF(NA))
  expect_error(t_mcF(NULL))
})

test_that("t_FMC() works", {
  expect_identical(t_FMC(), list(FMC = 120))
  expect_error(t_FMC(LAT = 41))
  expect_error(t_FMC(LAT = 71))
  expect_error(t_FMC(LONG = 52))
  expect_error(t_FMC(LONG = 142))
  expect_error(t_FMC(ELV = -1))
  expect_error(t_FMC(ELV = 2501))
  expect_error(t_FMC(Dj = -1))
  expect_error(t_FMC(Dj = 367))
})

test_that("t_SFC_FBP() works", {
  expect_snapshot(t_SFC_FBP())
  expect_error(t_SFC_FBP(BUI = -1))
  expect_error(t_SFC_FBP(BUI = 201))
  expect_error(t_SFC_FBP(FFMC = 79))
  expect_error(t_SFC_FBP(FFMC = 101))
  expect_error(t_SFC_FBP(PC = -1))
  expect_error(t_SFC_FBP(PC = 101))
})

test_that("t_SFC_deGroot() works", {
  expect_identical(
    t_SFC_deGroot(),
    list("Forest Floor Fuel Consumption" = 1.65, "SFC" = 1.95)
  )
  expect_error(t_SFC_FBP(BUI = -1))
  expect_error(t_SFC_FBP(BUI = 201))
  expect_error(t_SFC_FBP(FFL = 0.5))
  expect_error(t_SFC_FBP(FFL = 5.5))
  expect_error(t_SFC_FBP(FWFL = -1))
  expect_error(t_SFC_FBP(FWFL = 2.5))
})

test_that("ladder_standing_dead() works", {
  expect_identical(
    ladder_standing_dead(),
    list(
      "LFSG [m]" = 4,
      "Scaled SFC contribution, small snags [kg/m^2]" = 1.14
    )
  )
  expect_error(ladder_standing_dead(consumption = 0))
  expect_error(ladder_standing_dead(consumption = 11))
  expect_error(ladder_standing_dead(cl = 0))
  expect_error(ladder_standing_dead(cl = 16))
  expect_error(ladder_standing_dead(FSG = 0))
  expect_error(ladder_standing_dead(FSG = 21))
})

test_that("ladder_midstory_saplings() works", {
  expect_snapshot(ladder_midstory_saplings())
})
