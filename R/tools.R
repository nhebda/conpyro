#' Calculate probability of crown fire occurrence (pCFO) for a single set of
#' conditions
#'
#' `tool_pcfo()` calculates crown fire occurrence probability for a single set
#' of fuel, wind speed, and stand structure observations using the Canadian
#' Conifer Pyrometrics (ConPyro) model system. See Perrakis et al. (2023) for
#' details.
#'
#' @param mc A numeric value between `5` and `20` (inclusive). Fine fuel
#'   moisture content, either FFMC-based (mcFFMC) or stand-adjusted (mcSA). May
#'   be estimated using [tool_mc()].
#' @param ws A numeric value between `0` and `60` (inclusive). Wind speed in
#'   km/h.
#' @param fsg A numeric value between `0.5` and `20` (inclusive). Fuel strata
#'   gap in metres. The vertical distance between the top of the surface fuels
#'   and the lower limit of the canopy fuels. Analogous to crown base height
#'   (CBH) in the absence of mid-story ladder fuels.
#' @param sfc A numeric value between `0.1` and `6` (inclusive). Surface fuel
#'   consumption in kg/m^2. May be estimated using [tool_sfc_fbp()] or
#'   [tool_sfc_degroot()].
#' @param model Choose one of `7`, `8`, `10`, or `11`. The ConPyro model form
#'   used for calculations (see Perrakis et al., 2023). Models 7 and 10 assume
#'   that `mc` is FFMC-based (mcFFMC) while models 8 and 11 assume `mc` is
#'   stand-adjusted (mcSA).
#'
#' @returns A list of length `1` consisting of a numeric value named `pCFO`.
#' @export
#'
#' @examples {
#' ## TODO
#' }
#'
#' @importFrom checkmate assert_choice
#' @md
tool_pcfo <- function(mc = 7.8, ws = 20, fsg = 9.5, sfc = 1.8, model = 11) {
  # Check input validity
  assert_number(mc, lower = 5, upper = 20)
  assert_number(ws, lower = 0, upper = 60)
  assert_number(fsg, lower = 0.5, upper = 20)
  assert_number(sfc, lower = 0.1, upper = 6)
  assert_choice(model, c(7, 8, 10, 11))
  # Call pCFO function
  pcfo <- fn_pcfo(model, ws, fsg, sfc, mc)
  # Output
  out  <- list("pCFO" = round(pcfo, 2))
  return(out)
}

#' Estimate fine dead litter moisture content (MC)
#'
#' `tool_mc()` estimates fine dead surface litter moisture content (MC), both
#' from the FFMC (mcFFMC) and using the stand-adjusted model (mcSA).
#'
#' @param ffmc A numeric value between `80` and `99` (inclusive). The Fine Fuel
#'   Moisture Code (FFMC) as per the Canadian Forest Fire Weather Index System.
#' @param dmc A numeric value between `5` and `200` (inclusive). The Duff
#'   Moisture Code (DMC) as per the Canadian Forest Fire Weather Index System.
#' @param season One of `spring`, `sp-su`, `summer`, or `fall`.
#' @param density One of `light`, `moderate`, or `dense`.
#' @param stand One of `pine`, `spruce`, `Douglas-fir`, `deciduous`, or
#'   `mixedwood`.
#'
#' @returns A list of length `2` consisting of one numeric value named `mcFFMC`
#'   and one numeric value named `mcSA`.
#' @export
#'
#' @examples {
#' ## TODO
#' }
#'
#' @importFrom checkmate assert_number assert_choice
#' @md
tool_mc <- function(
    ffmc    = 91.2,
    dmc     = 75,
    season  = "summer",
    density = "moderate",
    stand   = "pine"
) {
  assert_number(ffmc, lower = 80, upper = 99)
  assert_number(dmc, lower = 5, upper = 200)
  assert_choice(
    tolower(season),
    c("spring","sp-su", "summer", "fall"),
    .var.name = "season"
  )
  assert_choice(
    tolower(density),
    c("light", "moderate", "dense"),
    .var.name = "density"
  )
  assert_choice(
    tolower(stand),
    c("deciduous", "douglas-fir", "mixedwood", "pine", "spruce"),
    .var.name = "stand"
  )
  mcffmc     <- fn_mcffmc(ffmc)
  mcdmc      <- fn_mcdmc(dmc)
  idx        <- fn_mcsa_idx(tolower(season), tolower(density), tolower(stand))
  mcsa       <- fn_mcsa(idx, mcffmc, mcdmc)
  out        <- list(round(mcffmc, 2), round(mcsa, 2))
  names(out) <- c("mcFFMC", "mcSA")
  return(out)
}

#' Estimate foliar moisture content (FMC)
#'
#' `tool_fmc()` estimates foliar moisture content as per the Canadian Forest
#' Fire Behavior Prediction System equations. A simple wrapper for
#' `cffdrs:::foliar_moisture_content`.
#'
#' @param lat A numeric value between `42` and `70` (inclusive). Latitude in
#'   decimal degrees.
#' @param long A numeric value between `53` and `141` (inclusive). Longitude in
#'   decimal degrees.
#' @param elv A numeric value between `0` and `2500` (inclusive).Elevation in
#'   metres about sea level.
#' @param dj A numeric value between `0` and `365` (inclusive). Julian day.
#'
#' @returns A list of length `1` consisting of a numeric value named `FMC`.
#' @export
#'
#' @examples {
#' ## TODO
#' }
#'
#' @importFrom checkmate assert_number
#' @md
tool_fmc <- function(lat = 48, long = 83.3, elv = 100, dj = 200) {
  assert_number(lat, lower = 42, upper = 70)
  assert_number(long, lower = 53, upper = 141)
  assert_number(elv, lower = 0, upper = 2500)
  assert_number(dj, lower = 0, upper = 365)
  fmc <- cffdrs:::foliar_moisture_content(lat, long, elv, dj, 0)
  out <- list("FMC" = round(fmc, 2))
  return(out)
}

#' Estimate surface fuel consumption (SFC) using Canadian Forest Fire Behavior
#' Prediction System (FBPS) equations
#'
#' `tool_sfc_fbp()` estimates SFC for all major fuel types as per the FBPS
#' equations. A simple wrapper for `cffdrs:::surface_fuel_consumption`. An
#' alternative SFC calculation method is offered by [tool_sfc_degroot()].
#'
#' @inheritParams tool_mc
#' @param bui A numeric value between `80` and `200` (inclusive). The Buildup
#'   Index (BUI) as per the Canadian Forest Fire Weather Index System.
#' @param pc A numeric value between `0` and `100` (inclusive). Percent conifer
#'   for M1/M2 fuel types.
#'
#' @returns A list of length `10` consisting of named numeric values
#'   corresponding to the major FBPS fuel types.
#' @export
#'
#' @examples {
#' ## TODO
#' }
#'
#' @importFrom checkmate assert_number
#' @md
tool_sfc_fbp <- function(
    bui  = 85,
    ffmc = 91,
    pc   = 40
) {
  assert_number(bui, lower = 0, upper = 200)
  assert_number(ffmc, lower = 80, upper = 99)
  assert_number(pc, lower = 0, upper = 100)
  fueltypes <- c("C1", "C2", "C3", "C5", "C7", "D1", "M1", "S1", "S2", "S3")
  out <- lapply(
    fueltypes,
    cffdrs:::surface_fuel_consumption,
    BUI  = bui,
    FFMC = ffmc,
    PC   = pc
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
#' `tool_sfc_degroot()` estimates SFC based on fuel load and Buildup Index (BUI)
#' in experimental burns (R2 = 0.787). This equation may give nonsensical
#' results (e.g., SFC > SFL) for cases of low surface fuel load (SFL) and high
#' BUI. See De Groot et al. (2009) for details.
#'
#' @inheritParams tool_sfc_fbp
#' @param ffl A numeric value between `1` and `5` (inclusive). Forest floor load
#'   (litter + duff) in kg/m^2.
#' @param fwfl A numeric value between `0` and `2` (inclusive). Fine woody fuel
#'   load (<7 cm diam.) in kg/m^2.
#'
#' @returns A list of length `2` consisting of one numeric value named
#'   `Forest Floor Fuel Consumption` and one numeric value named `SFC`.
#' @export
#'
#' @examples {
#' ## TODO
#' }
#'
#' @importFrom checkmate assert_number
#' @md
tool_sfc_degroot <- function(bui = 85, ffl = 3.5, fwfl = 0.3) {
  assert_number(bui, lower = 0, upper = 200)
  assert_number(ffl, lower = 1, upper = 5)
  assert_number(fwfl, lower = 0, upper = 2)
  fffc  <- -0.176 + 0.156 * ffl + 0.015 * bui
  sfc   <- fwfl + fffc
  out   <- list("Forest Floor Fuel Consumption" = fffc, "SFC" = sfc)
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
#' @inheritParams tool_pcfo
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
#' @examples {
#' ## TODO
#' }
#'
#' @importFrom checkmate assert_number
#' @md
ladder_standing_dead <- function(consumption = 0.2, cl = 4, fsg = 6) {
  assert_number(consumption, lower = 0.1, upper = 10)
  assert_number(cl, lower = 0.5, upper = 15)
  assert_number(fsg, lower = 0.5, upper = 20)
  snag_centroid <- if((cl / 2) >= fsg) fsg - 0.5 else cl / 2
  zl            <- fsg - snag_centroid
  scaled_sfc    <- (fsg / zl)^1.5 * consumption * 3.1
  out           <- list(
    "LFSG [m]" = round(zl, 2),
    "Scaled SFC contribution, small snags [kg/m^2]" = round(scaled_sfc, 2)
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
#' @param sapling_fmc Sapling foliar moisture content in percent.
#' @param actual_sfc Actual SFC in kg/m^2.
#'
#' @returns A list of length `5` consisting of numeric values named
#'   `Sapling crown centroid [m]`, `FSG [m]`, `Sapling false-SFC`,
#'   `SFC scaled to crown centroid`, and `Total false-SFC`, respectively.
#' @export
#'
#' @examples {
#' ## TODO
#' }
#' @importFrom checkmate assert_number
#' @md
ladder_midstory_saplings <- function(
    hs          = 5,
    zs          = 1,
    zp          = 6,
    lnfl        = 0.5,
    sapling_fmc = 120,
    actual_sfc  = 2.7
) {
  assert_number(hs)
  assert_number(zs)
  assert_number(zp)
  assert_number(lnfl)
  assert_number(sapling_fmc)
  assert_number(actual_sfc)
  cs           <- zs + (hs - zs) / 2
  fsg          <- if((zp - cs) < 0.5) 0.5 else zp - cs
  deltah       <- (16.52 - 0.057 * sapling_fmc) / 16
  sapling_sfcf <- deltah * lnfl * 1.5 * 3.1
  sfc_cs       <- (fsg / zp)^1.5 * actual_sfc
  total_sfcf   <- sapling_sfcf + sfc_cs
  out          <- list(
    "Sapling crown centroid [m]" = cs,
    "FSG [m]" = fsg,
    "Sapling false-SFC" = sapling_sfcf,
    "SFC scaled to crown centroid" = sfc_cs,
    "Total false-SFC" = total_sfcf
  )
  out[]        <- lapply(out, round, 2)
  return(out)
}
