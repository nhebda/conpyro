test_that("tool_pCFO() works", {
  expect_identical(tool_pCFO(), list(pCFO = 0.55))
  expect_error(tool_pCFO(MC = 4))
  expect_error(tool_pCFO(MC = 21))
  expect_error(tool_pCFO(WS = -1))
  expect_error(tool_pCFO(WS = 61))
  expect_error(tool_pCFO(FSG = 0))
  expect_error(tool_pCFO(FSG = 21))
  expect_error(tool_pCFO(SFC = 0))
  expect_error(tool_pCFO(SFC = 7))
  expect_error(tool_pCFO(model = 9))
  expect_error(tool_pCFO(model = 13))
})

test_that("tool_MC() works", {
  expect_identical(tool_MC(), list(MCFFMC = 9.57, MCSA = 8.8))
  expect_error(tool_MC(FFMC = 79))
  expect_error(tool_MC(FFMC = 101))
  expect_error(tool_MC(DMC = 4))
  expect_error(tool_MC(DMC = 201))
  expect_error(tool_MC(season = ""))
  expect_error(tool_MC(season = NULL))
  expect_error(tool_MC(season = NA))
  expect_error(tool_MC(density = ""))
  expect_error(tool_MC(density = NULL))
  expect_error(tool_MC(density = NA))
  expect_error(tool_MC(stand = ""))
  expect_error(tool_MC(stand = NULL))
  expect_error(tool_MC(stand = NA))
})

test_that("tool_FMC() works", {
  expect_identical(tool_FMC(), list(FMC = 120))
  expect_error(tool_FMC(LAT = 41))
  expect_error(tool_FMC(LAT = 71))
  expect_error(tool_FMC(LONG = 52))
  expect_error(tool_FMC(LONG = 142))
  expect_error(tool_FMC(ELV = -1))
  expect_error(tool_FMC(ELV = 2501))
  expect_error(tool_FMC(Dj = -1))
  expect_error(tool_FMC(Dj = 367))
})

test_that("tool_SFC_FBP() works", {
  expect_snapshot(tool_SFC_FBP())
  expect_error(tool_SFC_FBP(BUI = -1))
  expect_error(tool_SFC_FBP(BUI = 201))
  expect_error(tool_SFC_FBP(FFMC = 79))
  expect_error(tool_SFC_FBP(FFMC = 101))
  expect_error(tool_SFC_FBP(PC = -1))
  expect_error(tool_SFC_FBP(PC = 101))
})

test_that("tool_SFC_deGroot() works", {
  expect_identical(
    tool_SFC_deGroot(),
    list("Forest Floor Fuel Consumption" = 1.65, "SFC" = 1.95)
  )
  expect_error(tool_SFC_FBP(BUI = -1))
  expect_error(tool_SFC_FBP(BUI = 201))
  expect_error(tool_SFC_FBP(FFL = 0.5))
  expect_error(tool_SFC_FBP(FFL = 5.5))
  expect_error(tool_SFC_FBP(FWFL = -1))
  expect_error(tool_SFC_FBP(FWFL = 2.5))
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
