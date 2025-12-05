#' Determine season for [t_mcsa()]
#'
#' `t_mcSeason()` takes a month and day as input and calculates the "season"
#' argument to be used in [t_mcsa()].
#'  * Jan 1 to May 31  = 1
#'  * Jun 1 to Jun 15  = 1.5
#'  * Jun 16 to Aug 31 = 2
#'  * Sep 1 to Dec 31  = 3
#'
#' @param month A single integer value from `1` to `12`.
#' @param day A single integer value from `1` to `31`.
#'
#' @returns One of `1`, `1.5`, `2`, or `3`, corresponding to spring, sp-su,
#'   summer, or fall.
#' @export
#'
#' @examples
#' # TODO
#'
#' @importFrom checkmate assert_integerish
#'
t_mcSeason <- function(month = 7, day = 1) {
  # Check input
  assert_integerish(month, lower = 1, upper = 12)
  assert_integerish(day, lower = 1, upper = 31)
  # Calculate
  if (
    inherits(
      try(as.Date(paste(2024, month, day, sep = "-")), silent = TRUE),
      "try-error"
    )
  ) {
    cat("Please enter a valid date\n")
    return(invisible())
  } else {
    date <- as.Date(paste(2024, month, day, sep = "-"))
  }
  season <- if (date >= as.Date("2024-01-01") & date < as.Date("2024-06-01")) {
    1
  } else if (date >= as.Date("2024-06-01") & date < as.Date("2024-06-16")) {
    1.5
  } else if (date >= as.Date("2024-06-16") & date < as.Date("2024-09-01")) {
    2
  } else if (date >= as.Date("2024-09-01") & date <= as.Date("2024-12-31")) {
    3
  }
  # Output
  return(season)
}

#' Estimate fine dead litter moisture content (mcF)
#'
#' `t_mcF()` estimates fine dead surface litter moisture content from the
#' Fine Fuel Moisture Code (FFMC).
#'
#' @inheritParams conpyro
#'
#' @returns A single numeric value.
#' @export
#'
#' @examples
#' # TODO
#'
#' @importFrom checkmate assert_number
#'
t_mcF <- function(FFMC = 90) {
  # Check input
  assert_number(FFMC, lower = 80, upper = 99)
  # Calculate
  mcF <- fn_mcFFMC(FFMC)
  # Output
  out <- round(mcF, 2)
  return(out)
}

#' Estimate fine dead litter moisture content (mcsa)
#'
#' `t_mcsa()` estimates fine dead surface litter moisture content using the
#' stand-adjusted model (mcsa).
#'
#' @inheritParams conpyro
#' @param season One of `spring`, `sp-su`, `summer`, or `fall`, or the numeric
#'   equivalents `1`, `1.5`, `2`, or `3`, respectively.
#' @param density One of `light`, `moderate`, or `dense`, or the numeric
#'   equivalents `1`, `2`, or `3`, respectively.
#' @param stand One of `pine`, `spruce`, `Douglas-fir`, `deciduous`, or
#'   `mixedwood`, or the abbreviated equivalents `p`, `s`, `df`, `d`, or `m`,
#'   respectively.
#'
#' @returns A single numeric value.
#' @export
#'
#' @examples
#' # TODO
#'
#' @importFrom checkmate assert_number assert_choice
#'
t_mcsa <- function(
    FFMC    = 90,
    DMC     = 85,
    season  = 2,
    density = 2,
    stand   = "p"
) {
  # Check input
  assert_number(FFMC, lower = 80, upper = 99)
  assert_number(DMC, lower = 5, upper = 250)
  assert_choice(
    tolower(season),
    c("spring","sp-su", "summer", "fall", 1, 1.5, 2, 3),
    .var.name = "season"
  )
  assert_choice(
    tolower(density),
    c("light", "moderate", "dense", 1, 2, 3),
    .var.name = "density"
  )
  assert_choice(
    tolower(stand),
    c(
      "deciduous",
      "douglas-fir",
      "mixedwood",
      "pine",
      "spruce",
      "d",
      "df",
      "m",
      "p",
      "s"
    ),
    .var.name = "stand"
  )
  # Calculate
  idx   <- fn_mcsa_idx(tolower(season), tolower(density), tolower(stand))
  mcF   <- fn_mcFFMC(FFMC)
  mcDMC <- fn_mcDMC(DMC)
  mcsa  <- fn_mcsa(idx, mcF, mcDMC)
  # Output
  out   <- round(mcsa, 2)
  return(out)
}

#' Calculate probability of crown fire occurrence (pCFO) for a single set of
#' conditions
#'
#' `t_pCFO()` calculates crown fire occurrence probability for a single set of
#' fuel and fire weather conditions using the Conifer Pyrometrics (ConPyro) fire
#' behaviour modelling system. See Perrakis et al. (2023) for details.
#'
#' @param mcsa A numeric value between `3` and `20` (inclusive). Fine dead
#'   surface litter moisture content calculated using the stand-adjusted model
#'   (mcsa). May be estimated using [t_mcsa()]. If `mcF` is anything other than
#'   NULL, it will override `mcsa`.
#' @param mcF A numeric value between `3` and `20` (inclusive). Fine dead
#'   surface litter moisture content calculated using the Fine Fuel Moisture
#'   Code (FFMC). May be estimated using [t_mcF()]. If NULL, `mcsa` will be used
#'   instead.
#' @param FSG A numeric value between `0.5` and `20` (inclusive). Fuel strata
#'   gap in metres. The vertical distance between the top of the surface fuels
#'   and the lower limit of the canopy fuels. Analogous to crown base height
#'   (CBH) in the absence of mid-story ladder fuels.
#' @param SFC A numeric value between `0.1` and `6` (inclusive). Surface fuel
#'   consumption in kg/m^2. May be estimated using [t_SFC_FBP()] or
#'   [t_SFC_deGroot()].
#' @param ws A numeric value between `0` and `60` (inclusive). Wind speed in
#'   km/h.
#'
#' @returns A single numeric value.
#' @export
#'
#' @examples
#' # TODO
#'
#' @importFrom checkmate assert_number
#'
t_pCFO <- function(mcsa = 10, mcF = NULL, FSG = 6, SFC = 2, ws = 12) {
  # Check input
  assert_number(mcsa, lower = 3, upper = 20, null.ok = TRUE)
  assert_number(mcF, lower = 3, upper = 20, null.ok = TRUE)
  assert_number(FSG, lower = 0.5, upper = 20)
  assert_number(SFC, lower = 0.1, upper = 6)
  assert_number(ws, lower = 0, upper = 60)
  # Calculate
  model <- if (is.null(mcF)) 11 else 10
  mc    <- if (is.null(mcF)) mcsa else mcF
  pCFO  <- fn_pCFO(model, ws, FSG, SFC, mc)
  # Output
  out <- round(pCFO, 2)
  return(out)
}

#' Calculate fire type
#'
#' `t_FT()` calculates fire type from a single set of fuel and fire weather
#' conditions using the Conifer Pyrometrics (ConPyro) fire behaviour modelling
#' system. See Perrakis et al. (2023) for details.
#'
#' @inheritParams t_pCFO
#' @inheritParams conpyro
#' @param CBD A numeric value between `0.01` and `0.8` (inclusive). Crown bulk
#'   density in kg/m^3.
#'
#' @returns A single character string, one of either `S`, `PC`, or `AC`,
#'   corresponding to surface fire, passive crown fire, or active crown fire.
#' @export
#'
#' @examples
#' # TODO
#'
#' @importFrom checkmate assert_number
#'
t_FT <- function(
    mcsa      = 10,
    mcF       = NULL,
    FSG       = 6,
    SFC       = 2,
    CBD       = 0.16,
    ws        = 12,
    CF_thresh = 0.5
) {
  # Check input
  assert_number(mcsa, lower = 3, upper = 20, null.ok = TRUE)
  assert_number(mcF, lower = 3, upper = 20, null.ok = TRUE)
  assert_number(FSG, lower = 0.5, upper = 20)
  assert_number(SFC, lower = 0.1, upper = 6)
  assert_numeric(CBD, lower = 0.01, upper = 0.8)
  assert_number(ws, lower = 0, upper = 60)
  assert_number(CF_thresh, lower = 0, upper = 1)
  # Calculate
  model  <- if (is.null(mcF)) 11 else 10
  mc     <- if (is.null(mcF)) mcsa else mcF
  pCFO   <- fn_pCFO(model, ws, FSG, SFC, mc)
  CROS_A <- fn_CROS_A(1, mc, ws, CBD)
  CAC    <- fn_CAC(CROS_A, CBD)
  FT     <- if (pCFO < CF_thresh) {
    "S"
  } else if (pCFO >= CF_thresh & CAC < 1) {
    "PC"
  } else {
    "AC"
  }
  return(FT)
}

#' Calculate rate of spread (ROS) for a single set of conditions
#'
#' `t_ROS()` calculates rate of spread for a single set of fuel and fire weather
#' conditions using the Conifer Pyrometrics (ConPyro) fire behaviour modelling
#' system. See Perrakis et al. (2023) for details. Provides a more convenient
#' single-scenario calculation than using the main [conpyro()] function. If you
#' want want to run multiple scenarios or plot output, use [conpyro()] instead.
#'
#' @inheritParams t_FT
#' @inheritParams t_pCFO
#' @inheritParams conpyro
#'
#' @returns A list of length 2 consisting of:
#'   * A single numeric value named `Predicted rate of spread (m/min)`
#'   * A single character string named `Type of fire`
#' @export
#'
#' @examples
#' # TODO
#'
#' @importFrom checkmate assert_number
#'
t_ROS <- function(
    mcsa      = 10,
    mcF       = NULL,
    FSG       = 6,
    SFC       = 2,
    CBD       = 0.16,
    ws        = 12,
    CF_thresh = 0.5
) {
  # Check input
  assert_number(mcsa, lower = 3, upper = 20, null.ok = TRUE)
  assert_number(mcF, lower = 3, upper = 20, null.ok = TRUE)
  assert_number(FSG, lower = 0.5, upper = 20)
  assert_number(SFC, lower = 0.1, upper = 6)
  assert_numeric(CBD, lower = 0.01, upper = 0.8)
  assert_number(ws, lower = 0, upper = 60)
  assert_number(CF_thresh, lower = 0, upper = 1)
  # Calculate
  model  <- if (is.null(mcF)) 11 else 10
  mc     <- if (is.null(mcF)) mcsa else mcF
  pCFO   <- fn_pCFO(model, ws, FSG, SFC, mc)
  SROS   <- fn_SROS(1, ws, mc, NULL, NULL)
  CROS_A <- fn_CROS_A(1, mc, ws, CBD)
  CAC    <- fn_CAC(CROS_A, CBD)
  CROS_P <- fn_CROS_P(CROS_A, CAC)
  if (pCFO < CF_thresh) {
    ROS <- SROS
    FT  <- "S"
  } else if (pCFO >= CF_thresh & CAC < 1) {
    ROS <- CROS_P
    FT  <- "PC"
  } else {
    ROS <- CROS_A
    FT  <- "AC"
  }
  # Output
  out <- list(
    "Predicted rate of spread (m/min)" = round(ROS, 2),
    "Type of fire" = FT
  )
  return(out)
}

#' Estimate foliar moisture content (FMC)
#'
#' `t_FMC()` estimates foliar moisture content as per the Canadian Forest
#' Fire Behavior Prediction System (FBPS) equations. A simple wrapper for
#' `cffdrs:::foliar_moisture_content`.
#'
#' @param LAT A numeric value between `42` and `70` (inclusive). Latitude in
#'   decimal degrees.
#' @param LONG A numeric value between `53` and `141` (inclusive). Longitude in
#'   decimal degrees.
#' @param ELV A numeric value between `0` and `2500` (inclusive).Elevation in
#'   metres about sea level.
#' @param Dj A numeric value between `0` and `365` (inclusive). Julian day.
#'
#' @returns A list of length `1` consisting of a numeric value named `FMC`.
#' @export
#'
#' @examples
#' t_FMC(LAT = 54.13, LONG = 116.89, ELV = 1000, Dj = 178)
#'
#' @importFrom checkmate assert_number
t_FMC <- function(LAT = 48, LONG = 83.3, ELV = 100, Dj = 200) {
  assert_number(LAT, lower = 42, upper = 70)
  assert_number(LONG, lower = 53, upper = 141)
  assert_number(ELV, lower = 0, upper = 2500)
  assert_number(Dj, lower = 0, upper = 365)
  FMC <- cffdrs:::foliar_moisture_content(LAT, LONG, ELV, Dj, 0)
  out <- list("FMC" = round(FMC, 2))
  return(out)
}

#' Estimate surface fuel consumption (SFC) using Canadian Forest Fire Behavior
#' Prediction System (FBPS) equations
#'
#' `t_SFC_FBP()` estimates SFC for all major fuel types as per the FBPS
#' equations. A simple wrapper for `cffdrs:::surface_fuel_consumption`. An
#' alternative SFC calculation method is offered by [t_SFC_deGroot()].
#'
#' @inheritParams conpyro
#' @param BUI A numeric value between `80` and `200` (inclusive). The Buildup
#'   Index (BUI) as per the Canadian Forest Fire Weather Index (FWI) System.
#' @param PC A numeric value between `0` and `100` (inclusive). Percent conifer
#'   for M1/M2 fuel types.
#'
#' @returns A list of length `10` consisting of named numeric values
#'   corresponding to the major FBPS fuel types.
#' @export
#'
#' @examples
#' t_SFC_FBP(BUI = 81, FFMC = 92, PC = 55)
#'
#' @importFrom checkmate assert_number
t_SFC_FBP <- function(BUI = 85, FFMC = 91, PC = 40) {
  assert_number(BUI, lower = 0, upper = 200)
  assert_number(FFMC, lower = 80, upper = 99)
  assert_number(PC, lower = 0, upper = 100)
  fueltypes <- c("C1", "C2", "C3", "C5", "C7", "D1", "M1", "S1", "S2", "S3")
  out <- lapply(
    fueltypes,
    cffdrs:::surface_fuel_consumption,
    BUI  = BUI,
    FFMC = FFMC,
    PC   = PC
  )
  out[] <- lapply(out, round, 2)
  names(out) <- c(
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
  return(out)
}

#' Estimate surface fuel consumption (SFC) using the forest floor equation from
#' De Groot et al. (2009)
#'
#' `t_SFC_deGroot()` estimates SFC based on fuel load and Buildup Index (BUI)
#' in experimental burns (R2 = 0.787). This equation may give nonsensical
#' results (e.g., SFC > SFL) for cases of low surface fuel load (SFL) and high
#' BUI. See De Groot et al. (2009) for details.
#'
#' @inheritParams t_SFC_FBP
#' @param FFL A numeric value between `1` and `5` (inclusive). Forest floor load
#'   (litter + duff) in kg/m^2.
#' @param FWFL A numeric value between `0` and `2` (inclusive). Fine woody fuel
#'   load (<7 cm diam.) in kg/m^2.
#'
#' @returns A list of length `2` consisting of one numeric value named
#'   `Forest Floor Fuel Consumption` and one numeric value named `SFC`.
#' @export
#'
#' @examples
#' t_SFC_deGroot(BUI = 78, FFL = 2.8, FWFL = 0.4)
#'
#' @importFrom checkmate assert_number
t_SFC_deGroot <- function(BUI = 85, FFL = 3.5, FWFL = 0.3) {
  assert_number(BUI, lower = 0, upper = 200)
  assert_number(FFL, lower = 1, upper = 5)
  assert_number(FWFL, lower = 0, upper = 2)
  FFFC  <- -0.176 + 0.156 * FFL + 0.015 * BUI
  SFC   <- FWFL + FFFC
  out   <- list("Forest Floor Fuel Consumption" = FFFC, "SFC" = SFC)
  out[] <- lapply(out, round, 2)
  return(out)
}

#' Estimate standing dead ladder fuels for midstory small snags <5 cm DBH
#'
#' `ladder_standing_dead()` estimates standing dead ladder fuels for midstory
#' small snags <5 cm DBH, e.g., Jack pine overstory with dense standing dead
#' pine ladder fuels. Caution: Experimental! These equations are under
#' development and may contain errors.
#'
#' @inheritParams t_pCFO
#' @param consumption A numeric value between `0.1` and `10` (inclusive).
#'   Consumption of fine dead elevated wood in kg/m^2. Assumes continuity with
#'   surface fuels.
#' @param cl A numeric value between `0.5` and `15` (inclusive). Mean centroid
#'   height of ladder fuels in metres.
#'
#' @returns A list of length `2` consisting of one numeric value named
#'   `LFSG [m]` and one numeric value named
#'   `Scaled SFC contribution, small snags [kg/m^2]`
#' @export
#'
#' @examples
#' ladder_standing_dead(consumption = 1.1, cl = 5.1, FSG = 6.3)
#'
#' @importFrom checkmate assert_number
ladder_standing_dead <- function(consumption = 0.2, cl = 4, FSG = 6) {
  assert_number(consumption, lower = 0.1, upper = 10)
  assert_number(cl, lower = 0.5, upper = 15)
  assert_number(FSG, lower = 0.5, upper = 20)
  snag_centroid <- if((cl / 2) >= FSG) FSG - 0.5 else cl / 2
  zl            <- FSG - snag_centroid
  scaled_SFC    <- (FSG / zl)^1.5 * consumption * 3.1
  out           <- list(
    "LFSG [m]" = round(zl, 2),
    "Scaled SFC contribution, small snags [kg/m^2]" = round(scaled_SFC, 2)
  )
  return(out)
}

#' Estimate surface fuel consumption (SFC) contribution from a midstory sapling
#' cohort
#'
#' `ladder_midstory_saplings()` estimates the "false-SFC" contribution of a
#' midstory sapling cohort (>1.5 m height), e.g., Jack pine overstory with
#' midstory live black spruce saplings. These equations assume that the fuel
#' strata gap (FSG) represents the distance from the sapling crown centroid to
#' the overstory lower crown base height (LCBH). Calculate lower crowning
#' probability to the sapling cohort separately. Caution: Experimental! These
#' equations are under development and may contain errors.
#'
#' @param hs Sapling height in metres.
#' @param zs Sapling LCBH in metres.
#' @param zp Overstory LCBH in metres.
#' @param lnfl Live needle fuel load in kg/m^2.
#' @param sapling_FMC Sapling foliar moisture content in percent.
#' @param actual_SFC Actual SFC in kg/m^2.
#'
#' @returns A list of length `5` consisting of numeric values named
#'   `Sapling crown centroid [m]`, `FSG [m]`, `Sapling false-SFC`,
#'   `SFC scaled to crown centroid`, and `Total false-SFC`, respectively.
#' @export
#'
#' @examples
#' ladder_midstory_saplings(hs = 4.5, zs = 1.3, zp = 7, lnfl = 0.4, sapling_FMC
#' = 120, actual_SFC  = 2.8)
#'
#' @importFrom checkmate assert_number
ladder_midstory_saplings <- function(
    hs          = 5,
    zs          = 1,
    zp          = 6,
    lnfl        = 0.5,
    sapling_FMC = 120,
    actual_SFC  = 2.7
) {
  assert_number(hs)
  assert_number(zs)
  assert_number(zp)
  assert_number(lnfl)
  assert_number(sapling_FMC)
  assert_number(actual_SFC)
  cs           <- zs + (hs - zs) / 2
  FSG          <- if((zp - cs) < 0.5) 0.5 else zp - cs
  deltah       <- (16.52 - 0.057 * sapling_FMC) / 16
  sapling_SFCF <- deltah * lnfl * 1.5 * 3.1
  SFC_cs       <- (FSG / zp)^1.5 * actual_SFC
  total_SFCF   <- sapling_SFCF + SFC_cs
  out          <- list(
    "Sapling crown centroid [m]" = cs,
    "FSG [m]" = FSG,
    "Sapling false-SFC" = sapling_SFCF,
    "SFC scaled to crown centroid" = SFC_cs,
    "Total false-SFC" = total_SFCF
  )
  out[]        <- lapply(out, round, 2)
  return(out)
}
