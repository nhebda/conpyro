#' Calculate crown fire occurrence probability (pCFO) for a single set of
#' observations
#'
#' `tool_pcfo()` calculates crown fire occurrence probability for a single set
#' of fuel, wind speed, and stand structure observations using the Canadian
#' Conifer Pyrometrics (ConPyro) model system. See Perrakis et al. (2023) for
#' details.
#'
#' @param mc A numeric value between 5 and 20 (inclusive). Fine fuel moisture
#'   content, either FFMC-based (mcFFMC) or stand-adjusted (mcSA). Calculate
#'   using [tool_mc()].
#' @param ws A numeric value between 0 and 60 (inclusive). Wind speed in km/h.
#' @param fsg A numeric value between 0.5 and 20 (inclusive). Fuel strata gap in
#'   metres. The vertical distance between the top of the surface fuels and the
#'   lower limit of the canopy fuels. Analogous to crown base height (CBH) in
#'   the absence of mid-story ladder fuels.
#' @param sfc A numeric value between 0.1 and 6 (inclusive). Surface fuel
#'   consumption. May be estimated using [tool_sfc_fbp()] or
#'   [tool_sfc_degroot()].
#' @param model Choose one of `7`, `8`, `10`, or `11`. The ConPyro model form to
#'   use (see Perrakis et al., 2023). Models 7 and 10 assume that `mc` is
#'   FFMC-based (mcFFMC) while models 8 and 11 assume `mc` is stand-adjusted
#'   (mcSA).
#' @md
#'
#' @returns A list of length 1 consisting of a numeric value named "pCFO".
#' @export
#'
#' @examples {
#' ## TODO
#' }
#' @importFrom checkmate assert_choice
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
  out  <- list("pCFO" = pcfo)
  return(out)
}

#' Estimate fine dead litter moisture content (MC)
#'
#' `tool_mc` estimates fine dead surface litter moisture content (MC), both from
#' the FFMC (mcFFMC) and using the stand-adjusted model (mcSA).
#'
#' @param ffmc A numeric value between 80 and 99 (inclusive). The Fine Fuel
#'   Moisture Code (FFMC) as per the Canadian Forest Fire Weather Index System.
#' @param dmc A numeric value between 5 and 200 (inclusive). The Duff Moisture
#'   Code (DMC) as per the Canadian Forest Fire Weather Index System.
#' @param season One of "spring", "sp-su", "summer, or "fall".
#' @param density TODO: description.
#' @param stand TODO: description.
#'
#' @md
#' @returns
#' @export
#'
#' @examples
#'
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
  mcffmc <- fn_mcffmc(ffmc)
  mcdmc  <- fn_mcdmc(dmc)
  idx    <- fn_mcsa_idx(tolower(season), tolower(density), tolower(stand))
  mcsa   <- fn_mcsa(idx, mcffmc, mcdmc)
  out    <- list("mcffmc" = mcffmc, "mcsa" = mcsa)
  out
}

#' FMC calculator
#'
#' @param lat TODO: description.
#' @param long TODO: description.
#' @param elv TODO: description.
#' @param dj TODO: description.
#'
#' @returns
#' @export
#'
#' @examples
tool_fmc <- function(lat = 48, long = 83.3, elv = 100, dj = 200) {
  assert_number(lat, lower = 42, upper = 70)
  assert_number(long, lower = 53, upper = 141)
  assert_number(elv, lower = 0, upper = 2500)
  assert_number(dj, lower = 0, upper = 365)
  fmc <- cffdrs:::foliar_moisture_content(lat, long, elv, dj, 0)
  out <- list("fmc" = fmc)
  out
}

#' FBP SFC calculator
#'
#' @param bui TODO: description.
#' @param ffmc TODO: description.
#' @param pc TODO: description.
#'
#' @returns
#' @export
#'
#' @examples
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
  out
}

#' Degroot SFC calculator
#'
#' @param bui TODO: description.
#' @param ffl TODO: description.
#' @param fwfl TODO: description.
#'
#' @returns
#' @export
#'
#' @examples
tool_sfc_degroot <- function(bui = 85, ffl = 3.5, fwfl = 0.3) {
  assert_number(bui, lower = 0, upper = 200)
  assert_number(ffl, lower = 1, upper = 5)
  assert_number(fwfl, lower = 0, upper = 2)
  fffc <- -0.176 + 0.156 * ffl + 0.015 * bui
  sfc  <- fwfl + fffc
  out  <- list("Forest Floor Fuel Consumption" = fffc, "SFC" = sfc)
  out
}

#' Standing dead ladder fuels calculator
#'
#' @param consumption TODO: description.
#' @param cl TODO: description.
#' @param fsg TODO: description.
#'
#' @returns
#' @export
#'
#' @examples
ladder_standing_dead <- function(consumption = 0.2, cl = 4, fsg = 6) {
  assert_number(consumption, lower = 0.1, upper = 10)
  assert_number(cl, lower = 0.5, upper = 15)
  assert_number(fsg, lower = 0.5, upper = 20)
  snag_centroid <- if((cl / 2) >= fsg) fsg - 0.5 else cl / 2
  zl            <- fsg - snag_centroid
  scaled_sfc    <- (fsg / zl)^1.5 * consumption * 3.1
  out           <- list(
    "LFSG [m]" = zl,
    "Scaled SFC contribution, small snags [kg/m^2]" = scaled_sfc
  )
  out
}

#' Midstory saplings ladder fuel calculator
#'
#' @param hs TODO: description.
#' @param zs TODO: description.
#' @param zp TODO: description.
#' @param lnfl TODO: description.
#' @param sapling_fmc TODO: description.
#' @param actual_sfc TODO: description.
#'
#' @returns
#' @export
#'
#' @examples
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
  out
}
