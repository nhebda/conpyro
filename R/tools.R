#' Determine `season` input for [t_mcsa()]
#'
#' `t_mcSeason()` takes a month and day as input and calculates the `season`
#' argument to be used in [t_mcsa()].
#'  * Jan 1 to May 31  = `1 ("spring")`
#'  * Jun 1 to Jun 15  = `1.5 ("sp-su")`
#'  * Jun 16 to Aug 31 = `2 ("summer")`
#'  * Sep 1 to Dec 31  = `3 ("fall")`
#' A leap year (2024) is used internally so that February 29 is treated as a
#' valid date.
#'
#' @param month An integer in `{1, 2, 3, ..., 12}`.
#' @param day An integer in `{1, 2, 3, ..., 31}`.
#'
#' @returns A number in `{1, 1.5, 2, 3}`, corresponding to `{"spring", "sp-su",
#'   "summer", "fall"}`.
#' @export
#'
#' @examples
#' # Spring
#' t_mcSeason(5, 11)
#' # Spring-summer transition
#' t_mcSeason(6, 7)
#' # Summer
#' t_mcSeason(8, 16)
#' # Fall
#' t_mcSeason(9, 24)
#'
t_mcSeason <- function(month, day) {
  # Validate inputs
  validate_input(month = month, day = day)
  check_length(n = 1L, month = month, day = day)

  # Validate calendar date using a leap year (allows Feb 29)
  year <- 2024
  date <- as.Date(paste(year, month, day, sep = "-"), optional = TRUE)
  if (is.na(date)) {
    stop("Invalid month/day combination.", call. = FALSE)
  }

  # Calculate
  md <- month * 100L + day
  season <- if (md < 601L) {
    1
  } else if (md < 616L) {
    1.5
  } else if (md < 901L) {
    2
  } else {
    3
  }

  return(season)
}

#' Determine `density` input for [t_mcsa()]
#'
#' `t_mcDensity()` takes a canopy closure value as input and outputs a
#' categorical `density` value to be used in [t_mcsa()].
#'
#' @param canopy_closure An integer in `[20, 100]`. Canopy closure in percent.
#'
#' @returns A number in `{1, 2, 3}`, corresponding to `{"light", "moderate",
#'   "dense"}`
#' @export
#'
#' @examples
#' # Light
#' t_mcDensity(30)
#' # Moderate
#' t_mcDensity(50)
#' # Dense
#' t_mcDensity(70)
#'
t_mcDensity <- function(canopy_closure) {
  # Validate input
  validate_input(canopy_closure = canopy_closure)
  check_length(n = 1L, canopy_closure = canopy_closure)

  # Calculate
  dens <- if (canopy_closure <= 45) {
    1
  } else if (canopy_closure <= 60) {
    2
  } else {
    3
  }

  return(dens)
}

#' Estimate FFMC-based fine dead litter moisture content
#'
#' `t_mcF()` estimates fine dead surface litter moisture content based on the
#' Fine Fuel Moisture Code (FFMC) of the Canadian Fire Weather Index (FWI)
#' system.
#'
#' @inheritParams conpyro
#'
#' @returns A numeric value representing fine dead litter moisture content in
#'   percent.
#' @export
#'
#' @examples
#' # Moist
#' t_mcF(80)
#' # Moderate
#' t_mcF(84)
#' # Dry
#' t_mcF(89)
#' # Very dry
#' t_mcF(92)
#'
t_mcF <- function(FFMC) {
  # Validate inputs
  validate_input(FFMC = FFMC)
  check_length(n = 1L, FFMC = FFMC)

  # Calculate
  mcF <- mcFFMC(FFMC)
  out <- round(mcF, 2)
  return(out)
}

#' Estimate stand-adjusted fine dead litter moisture content
#'
#' `t_mcsa()` estimates fine dead surface litter moisture content using the
#' stand-adjusted model (mcsa).
#'
#' @inheritParams conpyro
#' @param season One of `{"spring", "sp-su", "summer", "fall"}` or numeric
#'   equivalents `{1, 1.5, 2, 3}`. See [t_mcSeason()].
#' @param density One of `{"light", "moderate", "dense"}` or numeric equivalents
#'   `{1, 2, 3}`.
#' @param stand One of `{"pine", "spruce", "Douglas-fir", "deciduous",
#'   "mixedwood"}` or abbreviated equivalents `{"p", "s", "df", "d", "m"}`.
#'
#' @returns A numeric value representing fine dead litter moisture content in
#'   percent.
#' @export
#'
#' @examples
#' # Moist
#' t_mcsa(FFMC = 80, DMC = 20, season = 1, density = 3, stand = "s")
#' # Moderate
#' t_mcsa(FFMC = 84, DMC = 30, season = 1, density = 3, stand = "p")
#' # Dry
#' t_mcsa(FFMC = 89, DMC = 40, season = 2, density = 2, stand = "p")
#' # Very dry
#' t_mcsa(FFMC = 92, DMC = 60, season = 2, density = 1, stand = "p")
#'
t_mcsa <- function(
    FFMC,
    DMC,
    season,
    density,
    stand,
    ...
) {
  # Validate required inputs
  validate_input(
    FFMC = FFMC,
    DMC = DMC,
    season = season,
    density = density,
    stand = stand
  )
  check_length(
    n = 1L,
    FFMC = FFMC,
    DMC = DMC,
    season = season,
    density = density,
    stand = stand
  )

  # Resolve and validate optional inputs
  extra_args <- list(...)
  if ("model_mcsa" %in% names(extra_args)) {
    model_mcsa <- extra_args[["model_mcsa"]]
    validate_input(model_mcsa = model_mcsa)
    check_length(n = 1L, model_mcsa = model_mcsa)
  } else {
    model_mcsa <- "corrected"
  }

  # Calculate
  coefs_mcsa <- sysdata$coefs_MCSA
  idx <- mcsa_idx(
    FFMC = FFMC,
    season = season,
    density = density,
    stand = stand,
    model_mcsa = model_mcsa
  )
  mcF <- mcFFMC(FFMC)
  mcDMC <- mcDMC(DMC)
  mcsa <- mcsa(
    idx = idx,
    mcF = mcF,
    mcDMC = mcDMC,
    coefs = coefs_mcsa
  )
  out <- round(mcsa, 2)

  return(out)
}

#' Calculate probability of crown fire occurrence (pCFO) for a single set of
#' conditions
#'
#' `t_pCFO()` calculates crown fire occurrence probability for a single set of
#' fuel and fire weather conditions using the Conifer Pyrometrics (ConPyro) fire
#' behaviour modelling system. See Perrakis et al. (2023) for details.
#'
#' @param WS10 A numeric value in `[0, 60]`. Wind speed in km/h.
#' @param mcsa A numeric value in `[3, 20]`. Fine dead surface litter moisture
#'   content calculated using the stand-adjusted model (mcsa). May be estimated
#'   using [t_mcsa()]. If `mcF` is anything other than `NULL`, it will override
#'   `mcsa`.
#' @param mcF A numeric value in `[3, 20]`. Fine dead surface litter moisture
#'   content calculated using the Fine Fuel Moisture Code (FFMC). May be
#'   estimated using [t_mcF()]. If `NULL`, `mcsa` will be used instead.
#' @param FSG A numeric value in `[0.5, 20]`. Fuel strata gap in metres. The
#'   vertical distance between the top of the surface fuels and the lower limit
#'   of the canopy fuels. Analogous to crown base height in the absence of
#'   mid-story ladder fuels.
#' @param SFC A numeric value in `[0.1, 6]`. Surface fuel consumption in kg/m^2.
#'   May be estimated using [t_SFC_FBP()] or [t_SFC_deGroot()].
#' @param ... Additional arguments to be passed to nested functions.
#'
#' @returns A numeric value giving probability of crown fire occurrence in the
#'   range of `[0, 1]`.
#' @export
#'
#' @examples
#' # Low probability
#' t_pCFO(ws = 11, mcsa = 10, FSG = 6, SFC = 2)
#' # Moderate probability
#' t_pCFO(ws = 12, mcsa = 9, FSG = 6, SFC = 2)
#' # High probability
#' t_pCFO(ws = 13, mcsa = 8, FSG = 6, SFC = 2)
#' # Using t_McF()
#' t_pCFO(ws = 13, mcF = t_mcF(89), FSG = 6, SFC = 2)
#' # Using t_mcsa()
#' t_pCFO(
#'   ws = 13,
#'   mcsa = t_mcsa(
#'     FFMC = 89,
#'     DMC = 40,
#'     season = 2,
#'     density = 2,
#'     stand = "p"
#'   ),
#'   FSG = 6,
#'   SFC = 2
#' )
#'
#' @importFrom checkmate assert_number
#'
t_pCFO <- function(
    WS10,
    mcsa = NULL,
    mcF = NULL,
    FSG,
    SFC,
    ...
) {
  # Resolve and validate required inputs
  if (is.null(mcsa) && is.null(mcF)) {
    stop("Either `mcsa` or `mcF` must be non-NULL.")
  }
  if (is.null(mcF)) {
    mc <- mcsa
    mc_type <- "mcsa"
  } else {
    mc <- mcF
    mc_type <- "mcF"
  }
  validate_input(
    WS10 = WS10,
    mc = mc,
    FSG = FSG,
    SFC = SFC
  )
  check_length(
    n = 1L,
    WS10 = WS10,
    mc = mc,
    FSG = FSG,
    SFC = SFC
  )

  # Resolve and validate optional inputs
  extra_args <- list(...)
  if ("model_pCFO" %in% names(extra_args)) {
    model_pCFO <- extra_args[["model_pCFO"]]
    validate_input(model_pCFO = model_pCFO)
    check_length(n = 1L, model_pCFO = model_pCFO)
  } else {
    model_pCFO <- if (mc_type == "mcF") 10L else if (mc_type == "mcsa") 11L
  }

  # Calculate
  coefs_pCFO <- sysdata$coefs_pCFO
  pCFO <- pCFO(
    WS10 = WS10,
    mc = mc,
    FSG = FSG,
    SFC = SFC,
    model_pCFO = model_pCFO,
    coefs = coefs_pCFO
  )
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
#' @param CBD A numeric value in `[0.01, 0.8]`. Crown bulk density in kg/m^3.
#'
#' @returns A single character vector giving the fire type. One of `{"S", "PC",
#'   "AC"}`, corresponding to surface fire, passive crown fire, or active crown
#'   fire, respectively.
#' @export
#'
#' @examples
#' # Surface fire
#' t_FT(mcsa = 9, FSG = 6, SFC = 2, CBD = 0.1, ws = 11)
#' # Passive crown fire
#' t_FT(mcsa = 8, FSG = 6, SFC = 2, CBD = 0.1, ws = 11)
#' # Active crown fire
#' t_FT(mcsa = 8, FSG = 6, SFC = 2, CBD = 0.2, ws = 11)
#' # Using t_mcF()
#' t_FT(mcF = t_mcF(91), FSG = 6, SFC = 2, CBD = 0.2, ws = 13)
#' # Using t_mcsa()
#' t_FT(mcsa = t_mcsa(91, 60, 2, 2, "p"), FSG = 6, SFC = 2, CBD = 0.2, ws = 13)
#'
#' @importFrom checkmate assert_number
#'
t_FT <- function(
    WS10,
    mcsa = NULL,
    mcF = NULL,
    FSG,
    SFC,
    CBD,
    ...
) {
  # Resolve and validate required inputs
  if (is.null(mcsa) & is.null(mcF)) {
    stop("Either `mcsa` or `mcF` must be non-NULL.")
  }
  if (is.null(mcF)) {
    mc <- mcsa
    mc_type <- "mcsa"
  } else {
    mc <- mcF
    mc_type <- "mcF"
  }
  validate_input(
    WS10 = WS10,
    mc = mc,
    FSG = FSG,
    SFC = SFC,
    CBD = CBD
  )
  check_length(
    n = 1L,
    WS10 = WS10,
    mc = mc,
    FSG = FSG,
    SFC = SFC,
    CBD = CBD
  )

  # Resolve and validate optional inputs
  extra_args <- list(...)
  if ("model_pCFO" %in% names(extra_args)) {
    model_pCFO <- extra_args[["model_pCFO"]]
    validate_input(model_pCFO = model_pCFO)
    check_length(n = 1L, model_pCFO = model_pCFO)
  } else {
    model_pCFO <- if (mc_type == "mcF") 10L else if (mc_type == "mcsa") 11L
  }
  if ("model_cROS" %in% names(extra_args)) {
    model_cROS <- extra_args[["model_cROS"]]
    validate_input(model_cROS = model_cROS)
    check_length(n = 1L, model_cROS = model_cROS)
  } else {
    model_cROS <- 1L
  }
  if ("CF_thresh" %in% names(extra_args)) {
    CF_thresh <- extra_args[["CF_thresh"]]
    validate_input(CF_thresh = CF_thresh)
    check_length(n = 1L, CF_thresh = CF_thresh)
  } else {
    CF_thresh <- 0.5
  }

  # Calculate
  coefs_pCFO <- sysdata$coefs_pCFO
  pCFO_val <- pCFO(
    WS10 = WS10,
    mc = mc,
    FSG = FSG,
    SFC = SFC,
    model_pCFO = model_pCFO,
    coefs = coefs_pCFO
  )
  cROS_A_val <- cROS_A(
    WS10 = WS10,
    mc = mc,
    CBD = CBD,
    model_cROS = model_cROS
  )
  CAC_val <- CAC(cROS_A = cROS_A_val, CBD = CBD)
  out <- if (pCFO_val < CF_thresh) {
    "S"
  } else if (pCFO_val >= CF_thresh & CAC_val < 1) {
    "PC"
  } else {
    "AC"
  }

  return(out)
}

#' Calculate rate of spread (ROS) for a single set of conditions
#'
#' `t_ROS()` calculates rate of spread for a single set of fuel and fire weather
#' conditions using the Conifer Pyrometrics (ConPyro) fire behaviour modelling
#' system. See Perrakis et al. (2023) for details. Provides a more convenient
#' single-scenario calculation than the main [conpyro()] function. To run
#' multiple scenarios or plot output, use [conpyro()] instead.
#'
#' @inheritParams t_FT
#' @inheritParams t_pCFO
#' @inheritParams conpyro
#'
#' @returns A list of length 2 consisting of:
#'   * A single numeric value named `Predicted rate of spread (m/min)`
#'   * A single character vector named `Type of fire`
#' @export
#'
#' @examples
#' # Surface fire
#' t_ROS(mcsa = 9, FSG = 6, SFC = 2, CBD = 0.1, ws = 11)
#' # Passive crown fire
#' t_ROS(mcsa = 8, FSG = 6, SFC = 2, CBD = 0.1, ws = 11)
#' # Active crown fire
#' t_ROS(mcsa = 8, FSG = 6, SFC = 2, CBD = 0.2, ws = 11)
#' # Active crown fire with smooth initiation
#' t_ROS(mcsa = 8, FSG = 6, SFC = 2, CBD = 0.2, ws = 11, smooth_CFO = TRUE)
#' # Using t_mcF()
#' t_ROS(mcF = t_mcF(91), FSG = 6, SFC = 2, CBD = 0.2, ws = 13)
#' # Using t_mcsa()
#' t_ROS(mcsa = t_mcsa(91, 60, 2, 2, "p"), FSG = 6, SFC = 2, CBD = 0.2, ws = 13)
#'
#' @importFrom checkmate assert_number assert_logical
#'
t_ROS <- function(
    WS10,
    mcsa = NULL,
    mcF = NULL,
    FSG,
    SFC,
    CBD,
    smooth_CFO = FALSE,
    ...
) {
  # Resolve and validate required inputs
  if (is.null(mcsa) & is.null(mcF)) {
    stop("Either `mcsa` or `mcF` must be non-NULL.")
  }
  if (is.null(mcF)) {
    mc <- mcsa
    mc_type <- "mcsa"
  } else {
    mc <- mcF
    mc_type <- "mcF"
  }
  validate_input(
    WS10 = WS10,
    mc = mc,
    FSG = FSG,
    SFC = SFC,
    CBD = CBD,
    smooth_CFO = smooth_CFO
  )
  check_length(
    n = 1L,
    WS10 = WS10,
    mc = mc,
    FSG = FSG,
    SFC = SFC,
    CBD = CBD,
    smooth_CFO = smooth_CFO
  )

  # Resolve and validate optional inputs
  extra_args <- list(...)
  if ("model_pCFO" %in% names(extra_args)) {
    model_pCFO <- extra_args[["model_pCFO"]]
    validate_input(model_pCFO = model_pCFO)
    check_length(n = 1L, model_pCFO = model_pCFO)
  } else {
    model_pCFO <- if (mc_type == "mcF") 10L else if (mc_type == "mcsa") 11L
  }
  if ("model_sROS" %in% names(extra_args)) {
    model_sROS <- extra_args[["model_sROS"]]
    validate_input(model_sROS = model_sROS)
    check_length(n = 1L, model_sROS = model_sROS)
  } else {
    model_sROS <- if (mc_type == "mcF") 12L else if (mc_type == "mcsa") 13L
  }
  if ("model_cROS" %in% names(extra_args)) {
    model_cROS <- extra_args[["model_cROS"]]
    validate_input(model_cROS = model_cROS)
    check_length(n = 1L, model_cROS = model_cROS)
  } else {
    model_cROS <- 1L
  }
  if ("CF_thresh" %in% names(extra_args)) {
    CF_thresh <- extra_args[["CF_thresh"]]
    validate_input(CF_thresh = CF_thresh)
    check_length(n = 1L, CF_thresh = CF_thresh)
  } else {
    CF_thresh <- 0.5
  }

  # Calculate
  coefs_pCFO <- sysdata$coefs_pCFO
  pCFO_val <- pCFO(
    WS10 = WS10,
    mc = mc,
    FSG = FSG,
    SFC = SFC,
    model_pCFO = model_pCFO,
    coefs = coefs_pCFO
  )
  sROS_val <- sROS(
    WS10 = WS10,
    mc = mc,
    SFC = SFC,
    model_sROS = model_sROS
  )
  cROS_A_val <- cROS_A(
    WS10 = WS10,
    mc = mc,
    CBD = CBD,
    model_cROS = model_cROS
  )
  CAC_val <- CAC(cROS_A = cROS_A_val, CBD = CBD)
  passive_crowning <- pCFO_val >= CF_thresh & CAC_val < 1
  cROS_P_val <- cROS_P(cROS_A = cROS_A_val, CAC = CAC_val)
  if (isTRUE(smooth_CFO)) {
    ROS_smooth_val <- ROS_smooth(
      pCFO = pCFO_val,
      passive_crowning = passive_crowning,
      sROS = sROS_val,
      cROS_P = cROS_P_val,
      cROS_A = cROS_A_val
    )
  }
  if (pCFO_val < CF_thresh) {
    ROS <- if (isTRUE(smooth_CFO)) ROS_smooth_val else sROS_val
    FT <- "S"
  } else if (pCFO_val >= CF_thresh & CAC_val < 1) {
    ROS <- if (isTRUE(smooth_CFO)) ROS_smooth_val else cROS_P_val
    FT  <- "PC"
  } else {
    ROS <- if (isTRUE(smooth_CFO)) ROS_smooth_val else cROS_A_val
    FT  <- "AC"
  }

  out <- list(
    "Type of fire" = FT,
    "Rate of spread (m/min)" = round(ROS, 1)
  )
  return(out)
}

#' Estimate foliar moisture content (FMC)
#'
#' `t_FMC()` estimates foliar moisture content as per the Canadian Forest
#' Fire Behavior Prediction System (FBPS) equations. A simple wrapper for
#' `cffdrs:::foliar_moisture_content`.
#'
#' @param LAT A numeric value in `[41, 70]`. Latitude in decimal degrees.
#' @param LONG A numeric value in `[52, 141]`. Longitude in decimal degrees.
#' @param ELV A numeric value in `[0, 2500]` or `NA`. Elevation in metres above
#'   sea level. If `NA`, elevation will not be used in the calculation.
#' @param Dj A numeric value in `[1, 366]`. Julian day.
#'
#' @returns A numeric value giving an estimate of foliar moisture content in
#'   percent.
#' @export
#'
#' @examples
#' # Excluding elevation
#' t_FMC(LAT = 54.13, LONG = 116.89, ELV = NA, Dj = 178)
#' # Including elevation
#' t_FMC(LAT = 54.13, LONG = 116.89, ELV = 1000, Dj = 178)
#'
#' @importFrom checkmate assert_number
#'
t_FMC <- function(LAT, LONG, ELV = NA, Dj) {
  # Validate inputs
  validate_input(LAT = LAT, LONG = LONG, ELV = ELV, Dj = Dj)

  # Calculate
  ELV <- if (is.na(ELV)) 0 else ELV
  FMC <- cffdrs:::foliar_moisture_content(LAT, LONG, ELV, Dj, 0)
  out <- round(FMC, 2)

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
#' @param BUI A numeric value in `[80, 200]`. The Buildup Index (BUI) as per the
#'   Canadian Forest Fire Weather Index (FWI) System.
#' @param PC A numeric value in `[0, 100]`. Percent conifer for M1/M2 fuel
#'   types.
#'
#' @returns A list of length `10` consisting of named numeric values
#'   corresponding to the major FBPS fuel types.
#' @export
#'
#' @examples
#' t_SFC_FBP(BUI = 81, FFMC = 92, PC = 55)
#'
#' @importFrom checkmate assert_number
#'
t_SFC_FBP <- function(BUI = 85, FFMC = 91, PC = 40) {
  # Validate inputs
  validate_input(BUI = BUI, FFMC = FFMC, PC = PC)

  # Calculate
  fueltypes <- c("C1", "C2", "C3", "C5", "C7", "D1", "M1", "S1", "S2", "S3")
  out <- lapply(
    fueltypes,
    cffdrs:::surface_fuel_consumption,
    BUI = BUI,
    FFMC = FFMC,
    PC = PC
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
#' @param FFL A numeric value in `[1, 5]`. Forest floor load (litter + duff) in
#'   kg/m^2.
#' @param FWFL A numeric value in `[0, 2]`. Fine woody fuel load (<7 cm diam.)
#'   in kg/m^2.
#'
#' @returns A list of length `2` consisting of one numeric value named `Forest
#'   Floor Fuel Consumption` and one numeric value named `SFC`, both in kg/m^2.
#' @export
#'
#' @examples
#' t_SFC_deGroot(BUI = 78, FFL = 2.8, FWFL = 0.4)
#'
#' @importFrom checkmate assert_number
#'
t_SFC_deGroot <- function(BUI, FFL, FWFL) {
  # Validate inputs
  validate_input(BUI = BUI, FFL = FFL, FWFL = FWFL)

  # Calculate
  FFFC <- -0.176 + 0.156 * FFL + 0.015 * BUI
  SFC <- FWFL + FFFC
  out <- list("Forest Floor Fuel Consumption" = FFFC, "SFC" = SFC)
  out[] <- lapply(out, round, 2)

  return(out)
}

#' Estimate standing dead ladder fuels for midstory small snags <5 cm DBH
#'
#' `ladder_standing_dead()` estimates standing dead ladder fuels for midstory
#' small snags <5 cm DBH, e.g., Jack pine overstory with dense standing dead
#' pine ladder fuels. CAUTION: Experimental! These equations are under
#' development and may contain errors.
#'
#' @inheritParams t_pCFO
#' @param cons A numeric value in `[0.1, 10]`. Consumption of fine dead elevated
#'   wood in kg/m^2. Assumes continuity with surface fuels.
#' @param cl A numeric value in `[0.5, 15]`. Mean centroid height of ladder
#'   fuels in metres.
#'
#' @returns A list of length `2` consisting of one numeric value named
#'   `LFSG [m]` and one numeric value named
#'   `Scaled SFC contribution, small snags [kg/m^2]`
#' @export
#'
#' @examples
#' ladder_standing_dead(cons = 1.1, cl = 5.1, FSG = 6.3)
#'
#' @importFrom checkmate assert_number
#'
ladder_standing_dead <- function(cons, cl, FSG) {
  # Validate inputs
  validate_input(cons = cons, cl = cl, FSG = FSG)

  # Calculate
  snag_centroid <- if((cl / 2) >= FSG) FSG - 0.5 else cl / 2
  zl <- FSG - snag_centroid
  scaled_SFC <- (FSG / zl)^1.5 * cons * 3.1
  out <- list(
    "LFSG (m)" = round(zl, 2),
    "Scaled SFC contribution, small snags (kg/m^2)" = round(scaled_SFC, 2)
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
#' probability to the sapling cohort separately. CAUTION: Experimental! These
#' equations are under development and may contain errors.
#'
#' @param hs Sapling height in metres.
#' @param zs Sapling lower crown base height (LCBH) in metres.
#' @param zp Overstory lower crown base height (LCBH) in metres.
#' @param lnfl Live needle fuel load in kg/m^2.
#' @param sapling_FMC Sapling foliar moisture content (FMC) in percent.
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
#'
ladder_midstory_saplings <- function(
    hs,
    zs,
    zp,
    lnfl,
    sapling_FMC,
    actual_SFC
) {
  # Validate inputs
  assert_number(hs)
  assert_number(zs)
  assert_number(zp)
  assert_number(lnfl)
  assert_number(sapling_FMC)
  assert_number(actual_SFC)

  # Calculate
  cs <- zs + (hs - zs) / 2
  FSG <- if((zp - cs) < 0.5) 0.5 else zp - cs
  deltah <- (16.52 - 0.057 * sapling_FMC) / 16
  sapling_SFCF <- deltah * lnfl * 1.5 * 3.1
  SFC_cs <- (FSG / zp)^1.5 * actual_SFC
  total_SFCF <- sapling_SFCF + SFC_cs
  out <- list(
    "Sapling crown centroid (m)" = cs,
    "FSG (m)" = FSG,
    "Sapling false-SFC" = sapling_SFCF,
    "SFC scaled to crown centroid" = SFC_cs,
    "Total false-SFC" = total_SFCF
  )
  out[] <- lapply(out, round, 2)

  return(out)
}
