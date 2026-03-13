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
#' @param month Required. An integer in `{1, 2, 3, ..., 12}`.
#' @param day Required. An integer in `{1, 2, 3, ..., 31}`.
#'
#' @returns A number in `{1, 1.5, 2, 3}`, corresponding to `{"spring", "sp-su",
#'   "summer", "fall"}`. [t_mcsa()] accepts either numeric codes or the
#'   corresponding labels.
#' @export
#'
#' @examples
#' # Basic usage
#' t_mcSeason(5, 11)   # Spring -> 1
#' t_mcSeason(6, 7)    # Spring-summer transition -> 1.5
#' t_mcSeason(8, 16)   # Summer -> 2
#' t_mcSeason(9, 24)   # Fall -> 3
#'
#' # Boundary dates
#' t_mcSeason(5, 31)   # last day of spring -> 1
#' t_mcSeason(6, 1)    # first day of sp-su -> 1.5
#' t_mcSeason(6, 15)   # last day of sp-su -> 1.5
#' t_mcSeason(6, 16)   # first day of summer -> 2
#' t_mcSeason(8, 31)   # last day of summer -> 2
#' t_mcSeason(9, 1)    # first day of fall -> 3
#'
#' # Leap-day behavior
#' t_mcSeason(2, 29)   # valid (leap year used internally) -> 1
#'
t_mcSeason <- function(month, day) {
  # Validate & normalize input
  validate_input(month = month, day = day)
  month <- normalize_input(month, "month")
  day <- normalize_input(day, "day")

  # Validate calendar date using a leap year (allows Feb 29)
  year <- 2024
  date <- as.Date(paste(year, month, day, sep = "-"), optional = TRUE)
  if (is.na(date)) {
    stop("Invalid month/day combination.", call. = FALSE)
  }

  # Calculate
  md <- month * 100L + day
  out <- if (md < 601L) {
    1
  } else if (md < 616L) {
    1.5
  } else if (md < 901L) {
    2
  } else {
    3
  }

  return(out)
}

#' Determine `density` input for [t_mcsa()]
#'
#' `t_mcDensity()` takes a canopy closure value as input and outputs a
#' categorical `density` value to be used in [t_mcsa()]. Light ≤ 45; Moderate
#' 46–60; Dense ≥ 61.
#'
#' @param canopy_closure Required. An number in `[20, 100]`. Canopy closure in
#'   percent.
#'
#' @returns An integer in `{1, 2, 3}`, corresponding to `{"light", "moderate",
#'   "dense"}`. [t_mcsa()] accepts either numeric codes or the corresponding
#'   labels.
#' @export
#'
#' @examples
#' # Basic usage
#' t_mcDensity(30)  # Light -> 1
#' t_mcDensity(50)  # Moderate -> 2
#' t_mcDensity(70)  # Dense -> 3
#'
#' # Boundary conditions
#' t_mcDensity(45)  # Light upper bound -> 1
#' t_mcDensity(46)  # Moderate lower bound -> 2
#' t_mcDensity(60)  # Moderate upper bound -> 2
#' t_mcDensity(61)  # Dense lower bound -> 3
#'
t_mcDensity <- function(canopy_closure) {
  # Validate & normalize input
  validate_input(canopy_closure = canopy_closure)
  canopy_closure <- normalize_input(canopy_closure, "canopy_closure")

  # Calculate
  out <- if (canopy_closure <= 45) {
    1L
  } else if (canopy_closure <= 60) {
    2L
  } else {
    3L
  }

  return(out)
}

#' Estimate FFMC-based fine dead litter moisture content
#'
#' `t_mcF()` estimates fine dead surface litter moisture content based on the
#' Fine Fuel Moisture Code (FFMC) of the Canadian Fire Weather Index System.
#'
#' @param FFMC Required. A single numeric value in `[80, 99]`. The Fine Fuel
#'   Moisture Code as per the Canadian Fire Weather Index System.
#'
#' @returns A numeric value representing fine dead litter moisture content in
#'   percent.
#' @export
#'
#' @examples
#' t_mcF(80)  # Moist
#' t_mcF(84)  # Moderate
#' t_mcF(89)  # Dry
#' t_mcF(92)  # Very dry
#'
t_mcF <- function(FFMC) {
  # Validate inputs
  validate_input(FFMC = FFMC)
  check_length(n = 1L, FFMC = FFMC)
  FFMC <- normalize_input(FFMC, "FFMC")

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
#' @inheritParams t_mcF
#' @param DMC Required. A single numeric value in `[5, 250]`. The Duff Moisture
#'   Code as per the Canadian Fire Weather Index System.
#' @param season Required. One of `{"spring", "sp-su", "summer", "fall"}` or
#'   numeric equivalents `{1, 1.5, 2, 3}`. See [t_mcSeason()].
#' @param density Required. One of `{"light", "moderate", "dense"}` or numeric
#'   equivalents `{1, 2, 3}`. See [t_mcDensity()].
#' @param stand Required. One of `{"pine", "spruce", "Douglas-fir", "deciduous",
#'   "mixedwood"}` or abbreviated equivalents `{"p", "s", "df", "d", "m"}`.
#' @param ... Optional. Allows additional arguments to be passed to nested
#'   functions by advanced users. In [t_mcsa()], the following advanced
#'   parameters are supported:
#'   * `model_mcsa`: One of either `original` or `corrected`. Default is
#'   `corrected`. See Perrakis et al. (2023) supplementary material for details.
#'
#' @returns A numeric value representing fine dead litter moisture content in
#'   percent.
#' @export
#'
#' @examples
#' t_mcsa(FFMC = 80, DMC = 20, season = 1, density = 3, stand = "s") # Moist
#' t_mcsa(FFMC = 84, DMC = 30, season = 1, density = 3, stand = "p") # Moderate
#' t_mcsa(FFMC = 89, DMC = 40, season = 2, density = 2, stand = "p") # Dry
#' t_mcsa(FFMC = 92, DMC = 60, season = 2, density = 1, stand = "p") # Very dry
#'
t_mcsa <- function(
    FFMC,
    DMC,
    season,
    density,
    stand,
    ...
) {
  # Validate & normalize required inputs
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
  FFMC <- normalize_input(FFMC, "FFMC")
  DMC <- normalize_input(DMC, "DMC")
  season <- normalize_input(season, "season")
  density <- normalize_input(density, "density")
  stand <- normalize_input(stand, "stand")

  # Resolve, validate, & normalize optional inputs
  extra_args <- list(...)
  allowed <- "model_mcsa"
  unknown <- setdiff(names(extra_args), allowed)
  if (length(unknown) > 0) {
    stop(
      "Unknown argument(s) in ...: ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }

  if ("model_mcsa" %in% names(extra_args)) {
    model_mcsa <- extra_args[["model_mcsa"]]
    validate_input(model_mcsa = model_mcsa)
    check_length(n = 1L, model_mcsa = model_mcsa)
    model_mcsa <- normalize_input(model_mcsa, "model_mcsa")
  } else {
    model_mcsa <- "corrected"
  }

  # Calculate
  coefs_mcsa <- sysdata$coefs_MCSA
  idx_val <- mcsa_idx(
    FFMC = FFMC,
    season = season,
    density = density,
    stand = stand,
    model_mcsa = model_mcsa
  )
  mcF_val <- mcFFMC(FFMC)
  mcDMC_val <- mcDMC(DMC)
  mcsa_val <- mcsa(
    idx = idx_val,
    mcFFMC = mcF_val,
    mcDMC = mcDMC_val,
    coefs = coefs_mcsa
  )
  out <- round(mcsa_val, 2)

  return(out)
}

#' Calculate probability of crown fire occurrence (pCFO) for a single set of
#' conditions
#'
#' `t_pCFO()` calculates crown fire occurrence probability for a single set of
#' fuel and fire weather conditions using the Conifer Pyrometrics (ConPyro) fire
#' behaviour modelling system. See Perrakis et al. (2023) for details.
#'
#' @param WS10 Required. A single numeric value in `[0, 60]`. Standard 10-m open
#'   wind speed in km/h.
#' @param mcF Optional. A single numeric value in `[1, 30]`. Fine dead surface
#'   litter moisture content in percent, calculated using the Fine Fuel Moisture
#'   Code (FFMC). May be estimated using [t_mcF()]. If `NULL`, `mcsa` will be
#'   used instead.
#' @param mcsa Optional. A single numeric value in `[1, 30]`. Fine dead surface
#'   litter moisture content in percent, calculated using the stand-adjusted
#'   model. May be estimated using [t_mcsa()]. If `mcF` is anything other than
#'   `NULL`, it will override `mcsa`.
#' @param FSG Required. A single numeric value in `[0.5, 20]`. Fuel strata gap
#'   in metres, representing the vertical distance between the top of the
#'   surface fuels and the lower limit of the canopy fuels. Analogous to crown
#'   base height in the absence of mid-story ladder fuels.
#' @param SFC Required. A single numeric value in `[0.1, 6]`. Surface fuel
#'   consumption in kg/m^2. May be estimated using [t_SFC_FBP()] or
#'   [t_SFC_deGroot()].
#' @param ... Optional. Allows additional arguments to be passed to nested
#'   functions by advanced users. In [t_pCFO()], the following advanced
#'   parameters are supported:
#'   * `model_pCFO`: An integer in `{7, 8, 10, 11}`, corresponding to the
#'   numbered crown fire occurrence models presented in Perrakis et al. (2023),
#'   Table 2. Default when using `mcF` is `10`; default when using `mcsa` is
#'   `11`.
#'
#' @returns A numeric value giving probability of crown fire occurrence in the
#'   range of `[0, 1]`.
#' @export
#'
#' @examples
#' # Basic usage
#' t_pCFO(WS10 = 11, mcsa = 10, FSG = 6, SFC = 2)  # Low probability
#' t_pCFO(WS10 = 12, mcsa = 9, FSG = 6, SFC = 2)   # Moderate probability
#' t_pCFO(WS10 = 13, mcsa = 8, FSG = 6, SFC = 2)   # High probability
#'
#' # Using t_mcF()
#' t_pCFO(WS10 = 13, mcF = t_mcF(89), FSG = 6, SFC = 2)
#'
#' # Using t_mcsa()
#' t_pCFO(
#'   WS10 = 13,
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
t_pCFO <- function(
    WS10,
    mcF = NULL,
    mcsa = NULL,
    FSG,
    SFC,
    ...
) {
  # Resolve, validate, & normalize required inputs
  if (is.null(mcsa) && is.null(mcF)) {
    stop("Either `mcsa` or `mcF` must be non-NULL.", call. = FALSE)
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
  WS10 <- normalize_input(WS10, "WS10")
  mc <- normalize_input(mc, "mc")
  FSG <- normalize_input(FSG, "FSG")
  SFC <- normalize_input(SFC, "SFC")

  # Resolve, validate, & normalize optional inputs
  extra_args <- list(...)
  allowed <- "model_pCFO"
  unknown <- setdiff(names(extra_args), allowed)
  if (length(unknown) > 0) {
    stop(
      "Unknown argument(s) in ...: ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }

  if ("model_pCFO" %in% names(extra_args)) {
    model_pCFO <- extra_args[["model_pCFO"]]
    validate_input(model_pCFO = model_pCFO)
    check_length(n = 1L, model_pCFO = model_pCFO)
    model_pCFO <- normalize_input(model_pCFO, "model_pCFO")
  } else {
    model_pCFO <- if (mc_type == "mcF") 10L else if (mc_type == "mcsa") 11L
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
  out <- round(pCFO_val, 2)

  return(out)
}

#' Calculate fire type
#'
#' `t_FT()` calculates fire type from a single set of fuel and fire weather
#' conditions using the Conifer Pyrometrics (ConPyro) fire behaviour modelling
#' system. See Perrakis et al. (2023) for details.
#'
#' @inheritParams t_pCFO
#' @param CBD Required. A single numeric value in `[0.01, 0.8]`. Crown bulk
#'   density in kg/m^3.
#' @param ... Optional. Allows additional arguments to be passed to nested
#'   functions by advanced users. In [t_FT()], the following advanced
#'   parameters are supported:
#'   * `model_pCFO`: An integer in `{7, 8, 10, 11}`, corresponding to the
#'   numbered crown fire occurrence models presented in Perrakis et al. (2023),
#'   Table 2. Default when using `mcF` is `10`; default when using `mcsa` is
#'   `11`.
#'   * `model_cROS`: An integer in `{1, 2, 3}`. Default is `1`. Numbers
#'   correspond to the following active cROS models:
#'     * `1`: WS10, CBD, mc
#'     * `2`: (0.084)WS10
#'     * `3`: (0.1)WS10
#'   * `CF_thresh`: A single numeric value in `[0, 1]`. Crown fire occurrence
#'   threshold. Default is 0.5.
#'
#' @returns A single character vector giving the fire type. One of `{"S", "PC",
#'   "AC"}`, corresponding to surface fire, passive crown fire, or active crown
#'   fire, respectively.
#' @export
#'
#' @examples
#' # Basic usage
#' t_FT(WS10 = 11, mcsa = 9, FSG = 6, SFC = 2, CBD = 0.1)  # Surface fire
#' t_FT(WS10 = 11, mcsa = 8, FSG = 6, SFC = 2, CBD = 0.1)  # Passive crown fire
#' t_FT(WS10 = 11, mcsa = 8, FSG = 6, SFC = 2, CBD = 0.2)  # Active crown fire
#'
#' # Using t_mcF()
#' t_FT(WS10 = 11, mcF = t_mcF(91), FSG = 6, SFC = 2, CBD = 0.2)
#'
#' # Using t_mcsa()
#' t_FT(
#'   WS10 = 13,
#'   mcsa = t_mcsa(
#'     FFMC = 91,
#'     DMC = 60,
#'     season = 2,
#'     density = 2,
#'     stand = "p"
#'   ),
#'   FSG = 6,
#'   SFC = 2,
#'   CBD = 0.2
#' )
#'
t_FT <- function(
    WS10,
    mcF = NULL,
    mcsa = NULL,
    FSG,
    SFC,
    CBD,
    ...
) {
  # Resolve, validate, & normalize required inputs
  if (is.null(mcsa) && is.null(mcF)) {
    stop("Either `mcsa` or `mcF` must be non-NULL.", call. = FALSE)
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
  WS10 <- normalize_input(WS10, "WS10")
  mc <- normalize_input(mc, "mc")
  FSG <- normalize_input(FSG, "FSG")
  SFC <- normalize_input(SFC, "SFC")
  CBD <- normalize_input(CBD, "CBD")

  # Resolve, validate, & normalize optional inputs
  extra_args <- list(...)
  allowed <- c("model_pCFO", "model_cROS", "CF_thresh")
  unknown <- setdiff(names(extra_args), allowed)
  if (length(unknown) > 0) {
    stop(
      "Unknown argument(s) in ...: ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }

  if ("model_pCFO" %in% names(extra_args)) {
    model_pCFO <- extra_args[["model_pCFO"]]
    validate_input(model_pCFO = model_pCFO)
    check_length(n = 1L, model_pCFO = model_pCFO)
    model_pCFO <- normalize_input(model_pCFO, "model_pCFO")
  } else {
    model_pCFO <- if (mc_type == "mcF") 10L else if (mc_type == "mcsa") 11L
  }
  if ("model_cROS" %in% names(extra_args)) {
    model_cROS <- extra_args[["model_cROS"]]
    validate_input(model_cROS = model_cROS)
    check_length(n = 1L, model_cROS = model_cROS)
    model_cROS <- normalize_input(model_cROS, "model_cROS")
  } else {
    model_cROS <- 1L
  }
  if ("CF_thresh" %in% names(extra_args)) {
    CF_thresh <- extra_args[["CF_thresh"]]
    validate_input(CF_thresh = CF_thresh)
    check_length(n = 1L, CF_thresh = CF_thresh)
    CF_thresh <- normalize_input(CF_thresh, "CF_thresh")
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
  } else if (pCFO_val >= CF_thresh && CAC_val < 1) {
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
#' @param smooth_CFO A single logical value that defines whether crown fire
#'   occurrence is modeled as a smooth transition (`TRUE`) or an instantaneous
#'   event (`FALSE`). Default is `FALSE`. If passive crowning occurs at any wind
#'   speed, smoothing is applied between sROS and cROS_P across the transition
#'   region; any subsequent transition to active crowning remains abrupt.
#' @param ... Optional. Allows additional arguments to be passed to nested
#'   functions by advanced users. In [t_ROS()], the following advanced
#'   parameters are supported:
#'   * `model_pCFO`: An integer in `{7, 8, 10, 11}`, corresponding to the
#'   numbered crown fire occurrence models presented in Perrakis et al. (2023),
#'   Table 2. Default when using `mcF` is `10`; default when using `mcsa` is
#'   `11`.
#'   * `model_sROS`: An integer in `{1, 2, 3, 4, 12, 13}`. Default when using
#'   `mcF` is `12`; default when using `mcsa` is `13`. Numbers correspond to the
#'   following sROS models:
#'     * `1`: FBPS aggregated surf. V4
#'     * `2`: FBPS D-1 (no BE)
#'     * `3`: FBPS C-6 (surface only, no BE)
#'     * `4`: ISI2SFC
#'     * `12`: m12 sl.con.ISI (Perrakis et al., 2026)
#'     * `13`: m13 sl.con.isim (Perrakis et al., 2026)
#'   * `model_cROS`: An integer in `{1, 2, 3}`. Default is `1`. Numbers
#'   correspond to the following active cROS models:
#'     * `1`: WS10, CBD, mc
#'     * `2`: (0.084)WS10
#'     * `3`: (0.1)WS10
#'   * `CF_thresh`: A single numeric value in `[0, 1]`. Crown fire occurrence
#'   threshold. Default is 0.5.
#'   * `ROS_output`: A vector comprising
#'   `{"sROS", "cROS_P", "cROS_A", "composite"}`. Determines whether ROS output
#'   is only sROS, only cROS_P, only cROS_A, or a piecewise composite of all
#'   three (default). Note that when `ROS_output` is anything other than
#'   `"composite"`, `smooth_CFO = TRUE` will be ignored.
#'
#' @returns A named list of length `2` with the following elements:
#'   * `Type of Fire`: A single character vector giving the fire type. One of
#'   `{"S", "PC", "AC"}`, corresponding to surface fire, passive crown fire, or
#'   active crown fire, respectively.
#'   * Depending of the value of `ROS_output`:
#'     * `Composite Rate of Spread (m/min)`
#'     * `Surface Rate of Spread (m/min)`
#'     * `Passive Crowning Rate of Spread (m/min)`
#'     * `Active Crowning Rate of Spread (m/min)`
#'
#' @export
#'
#' @examples
#' # Basic usage
#' t_ROS(WS10 = 11, mcsa = 9, FSG = 6, SFC = 2, CBD = 0.1)  # Surface fire
#' t_ROS(WS10 = 11, mcsa = 8, FSG = 6, SFC = 2, CBD = 0.1)  # Passive crown fire
#' t_ROS(WS10 = 11, mcsa = 8, FSG = 6, SFC = 2, CBD = 0.2)  # Active crown fire
#'
#' # Active crown fire with smooth occurrence
#' t_ROS(
#'   WS10 = 11,
#'   mcsa = 8,
#'   FSG = 6,
#'   SFC = 2,
#'   CBD = 0.2,
#'   smooth_CFO = TRUE
#' )
#'
#' # Using t_mcF()
#' t_ROS(WS10 = 13, mcF = t_mcF(91), FSG = 6, SFC = 2, CBD = 0.2)
#'
#' # Using t_mcsa()
#' t_ROS(
#'   WS10 = 13,
#'   mcsa = t_mcsa(
#'     FFMC = 91,
#'     DMC = 60,
#'     season = 2,
#'     density = 2,
#'     stand = "p"
#'   ),
#'   FSG = 6,
#'   SFC = 2,
#'   CBD = 0.2
#' )
#'
t_ROS <- function(
    WS10,
    mcF = NULL,
    mcsa = NULL,
    FSG,
    SFC,
    CBD,
    smooth_CFO = FALSE,
    ...
) {
  # Resolve, validate, & normalize required inputs
  if (is.null(mcsa) && is.null(mcF)) {
    stop("Either `mcsa` or `mcF` must be non-NULL.", call. = FALSE)
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
  WS10 <- normalize_input(WS10, "WS10")
  mc <- normalize_input(mc, "mc")
  FSG <- normalize_input(FSG, "FSG")
  SFC <- normalize_input(SFC, "SFC")
  CBD <- normalize_input(CBD, "CBD")
  smooth_CFO <- normalize_input(smooth_CFO, "smooth_CFO")

  # Resolve, validate, & normalize optional inputs
  extra_args <- list(...)
  allowed <- c(
    "model_pCFO",
    "model_sROS",
    "model_cROS",
    "CF_thresh",
    "ROS_output"
  )
  unknown <- setdiff(names(extra_args), allowed)
  if (length(unknown) > 0) {
    stop(
      "Unknown argument(s) in ...: ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  if ("model_pCFO" %in% names(extra_args)) {
    model_pCFO <- extra_args[["model_pCFO"]]
    validate_input(model_pCFO = model_pCFO)
    check_length(n = 1L, model_pCFO = model_pCFO)
    model_pCFO <- normalize_input(model_pCFO, "model_pCFO")
  } else {
    model_pCFO <- if (mc_type == "mcF") 10L else if (mc_type == "mcsa") 11L
  }
  if ("model_sROS" %in% names(extra_args)) {
    model_sROS <- extra_args[["model_sROS"]]
    validate_input(model_sROS = model_sROS)
    check_length(n = 1L, model_sROS = model_sROS)
    model_sROS <- normalize_input(model_sROS, "model_sROS")
  } else {
    model_sROS <- if (mc_type == "mcF") 12L else if (mc_type == "mcsa") 13L
  }
  if ("model_cROS" %in% names(extra_args)) {
    model_cROS <- extra_args[["model_cROS"]]
    validate_input(model_cROS = model_cROS)
    check_length(n = 1L, model_cROS = model_cROS)
    model_cROS <- normalize_input(model_cROS, "model_cROS")
  } else {
    model_cROS <- 1L
  }
  if ("CF_thresh" %in% names(extra_args)) {
    CF_thresh <- extra_args[["CF_thresh"]]
    validate_input(CF_thresh = CF_thresh)
    check_length(n = 1L, CF_thresh = CF_thresh)
    CF_thresh <- normalize_input(CF_thresh, "CF_thresh")
  } else {
    CF_thresh <- 0.5
  }
  if ("ROS_output" %in% names(extra_args)) {
    ROS_output <- extra_args[["ROS_output"]]
    validate_input(ROS_output = ROS_output)
    check_length(n = 1L, ROS_output = ROS_output)
    ROS_output <- normalize_input(ROS_output, "ROS_output")
  } else {
    ROS_output <- "composite"
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
  passive_crowning <- pCFO_val >= CF_thresh && CAC_val < 1
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
    iROS <- if (isTRUE(smooth_CFO)) ROS_smooth_val else sROS_val
    FT <- "S"
  } else if (pCFO_val >= CF_thresh && CAC_val < 1) {
    iROS <- if (isTRUE(smooth_CFO)) ROS_smooth_val else cROS_P_val
    FT  <- "PC"
  } else {
    iROS <- if (isTRUE(smooth_CFO)) ROS_smooth_val else cROS_A_val
    FT  <- "AC"
  }

  out <- list("Type of Fire" = FT)

  # Conditional ROS output
  if (ROS_output == "composite") {
    out[["Composite Rate of Spread (m/min)"]] <- round(iROS, 1)
  } else if (ROS_output == tolower("sROS")) {
    out[["Surface Rate of Spread (m/min)"]] <- round(sROS_val, 1)
  } else if (ROS_output == tolower("cROS_P")) {
    out[["Passive Crowning Rate of Spread (m/min)"]] <- round(cROS_P_val, 1)
  } else if (ROS_output == tolower("cROS_A")) {
    out[["Active Crowning Rate of Spread (m/min)"]] <- round(cROS_A_val, 1)
  }

  return(out)
}

#' Estimate foliar moisture content (FMC)
#'
#' `t_FMC()` estimates foliar moisture content by calling
#' `cffdrs:::foliar_moisture_content` (a non-exported helper in the `cffdrs`
#' package). Therefore, `cffdrs` must be installed for this function to work.
#'
#' @param LAT Required. A numeric value in `[41, 70]`. Latitude in decimal
#'   degrees.
#' @param LONG Required. A numeric value in `[52, 141]`. Longitude in decimal
#'   degrees west (positive; e.g., 123.1 for 123.1°W).
#' @param Dj Required. A single integer in `[1, 366]`. Julian day.
#' @param ELV Optional. A numeric value in `[0, 2500]` or `NA`. Elevation in
#'   metres above sea level. If `NA`, elevation is treated as `0` m (i.e.,
#'   sea-level).
#'
#' @returns A numeric value giving an estimate of foliar moisture content in
#'   percent.
#' @export
#'
#' @examples
#' if (requireNamespace("cffdrs", quietly = TRUE)) {
#'   # Excluding elevation
#'   t_FMC(LAT = 54.13, LONG = 116.89, Dj = 178, ELV = NA)
#'   # Including elevation
#'   t_FMC(LAT = 54.13, LONG = 116.89, Dj = 178, ELV = 1000)
#' }
#'
t_FMC <- function(LAT, LONG, Dj, ELV = NA) {
  # Dependency check (cffdrs is in Suggests)
  if (!requireNamespace("cffdrs", quietly = TRUE)) {
    stop(
      "Package 'cffdrs' must be installed to use t_FMC().",
      call. = FALSE
    )
  }

  # Validate and normalize inputs
  validate_input(LAT = LAT, LONG = LONG, Dj = Dj, ELV = ELV)
  check_length(n = 1L, LAT = LAT, LONG = LONG, Dj = Dj, ELV = ELV)
  LAT <- normalize_input(LAT, "LAT")
  LONG <- normalize_input(LONG, "LONG")
  Dj <- normalize_input(Dj, "Dj")
  ELV <- normalize_input(ELV, "ELV")

  # Calculate
  ELV <- if (is.na(ELV)) 0 else ELV
  FMC <- cffdrs:::foliar_moisture_content(
    LAT = LAT,
    LONG = LONG,
    DJ = Dj,
    ELV = ELV,
    D0 = 0
  )

  # Defensive checks: ensures cffdrs:::foliar_moisture_content() output is a
  # finite numeric scalar
  if (!is.numeric(FMC) || length(FMC) != 1L || !is.finite(FMC)) {
    stop(
      "Unexpected output from cffdrs:::foliar_moisture_content(). ",
      "Expected a finite numeric scalar.",
      call. = FALSE
    )
  }
  out <- round(as.numeric(FMC), 2)

  return(out)
}

#' Estimate surface fuel consumption (SFC) using Canadian Forest Fire Behavior
#' Prediction System (FBPS) equations
#'
#' `t_SFC_FBP()` estimates SFC for major FBPS fuel types by calling
#' `cffdrs:::surface_fuel_consumption` (a non-exported helper in the `cffdrs`
#' package). Therefore, `cffdrs` must be installed for this function to work.
#' An alternative SFC calculation method is offered by [t_SFC_deGroot()].
#'
#' @inheritParams t_mcF
#' @param BUI Required. A numeric value in `[5, 200]`. The Buildup Index (BUI)
#'   as per the Canadian Forest Fire Weather Index (FWI) System.
#' @param PC Required. A numeric value in `[0, 100]`. Percent conifer for M1/M2
#'   fuel types.
#'
#' @returns A list of length `10` consisting of named numeric values
#'   corresponding to the major FBPS fuel types.
#' @export
#'
#' @examples
#' if (requireNamespace("cffdrs", quietly = TRUE)) {
#'   t_SFC_FBP(BUI = 81, FFMC = 92, PC = 55)
#' }
#'
t_SFC_FBP <- function(BUI, FFMC, PC) {
  # Dependency check (cffdrs is in Suggests)
  if (!requireNamespace("cffdrs", quietly = TRUE)) {
    stop(
      "Package 'cffdrs' must be installed to use t_SFC_FBP().",
      call. = FALSE
    )
  }

  # Validate and normalize inputs
  validate_input(BUI = BUI, FFMC = FFMC, PC = PC)
  check_length(n = 1L, FFMC = FFMC)
  BUI <- normalize_input(BUI, "BUI")
  FFMC <- normalize_input(FFMC, "FFMC")
  PC <- normalize_input(PC, "PC")

  # Calculate
  fueltypes <- c("C1", "C2", "C3", "C5", "C7", "D1", "M1", "S1", "S2", "S3")
  out <- lapply(
    fueltypes,
    cffdrs:::surface_fuel_consumption,
    BUI = BUI,
    FFMC = FFMC,
    PC = PC
  )

  # Defensive checks: ensures each element is a finite numeric scalar
  out <- lapply(out, function(x) {
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x)) {
      stop(
        "Unexpected output from cffdrs:::surface_fuel_consumption(). ",
        "Expected a finite numeric scalar.",
        call. = FALSE
      )
    }
    round(as.numeric(x), 2)
  })

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
#' @param FFL Required. A numeric value in `[1, 5]`. Forest floor load (litter +
#'   duff) in kg/m^2.
#' @param FWFL Required. A numeric value in `[0, 2]`. Fine woody fuel load (<7
#'   cm diam.) in kg/m^2.
#'
#' @returns A list of length `2` consisting of one numeric value named `Forest
#'   Floor Fuel Consumption` and one numeric value named `SFC`, both in kg/m^2.
#'   SFC = Fine Woody Fuel Load + Forest Floor Fuel Consumption.
#' @export
#'
#' @examples
#' t_SFC_deGroot(BUI = 78, FFL = 2.8, FWFL = 0.4)
#'
t_SFC_deGroot <- function(BUI, FFL, FWFL) {
  # Validate and normalize inputs
  validate_input(BUI = BUI, FFL = FFL, FWFL = FWFL)
  BUI <- normalize_input(BUI, "BUI")
  FFL <- normalize_input(FFL, "FFL")
  FWFL <- normalize_input(FWFL, "FWFL")

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
#' @param cons Required. A numeric value in `[0.1, 10]`. Consumption of fine
#'   dead elevated wood in kg/m^2. Assumes continuity with surface fuels.
#' @param cl Required. A numeric value in `[0.5, 15]`. Mean centroid height of
#'   ladder fuels in metres.
#'
#' @returns A list of length `2` consisting of one numeric value named
#'   `LFSG (m)` and one numeric value named
#'   `Scaled SFC contribution, small snags (kg/m^2)`
#' @export
#'
#' @examples
#' ladder_standing_dead(cons = 1.1, cl = 5.1, FSG = 6.3)
#'
ladder_standing_dead <- function(cons, cl, FSG) {
  # Validate and normalize inputs
  validate_input(cons = cons, cl = cl, FSG = FSG)
  check_length(n = 1L, cons = cons, cl = cl, FSG = FSG)
  cons <- normalize_input(cons, "cons")
  cl <- normalize_input(cl, "cl")
  FSG <- normalize_input(FSG, "FSG")

  # Calculate
  snag_centroid <- if ((cl / 2) >= FSG) FSG - 0.5 else cl / 2
  zl <- FSG - snag_centroid
  scaled_SFC <- (FSG / zl)^1.5 * cons * 3.1 # zl > 0
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
#' @param hs Required. A single numeric value in `[0.5, 10]`. Sapling height in
#'   metres.
#' @param zs Required. A single numeric value in `[0.5, 9.5]`. Sapling lower
#'   crown base height (LCBH) in metres. `zs` must always be less than `hs`.
#' @param zp Required. A single numeric value in `[0.5, 20]`. Overstory lower
#'   crown base height (LCBH) in metres.
#' @param lnfl Required. A single numeric value in `[0.1, 2]`. Live needle fuel
#'   load in kg/m^2.
#' @param sapling_FMC Required. A single numeric value in `[80, 120]`. Sapling
#'   foliar moisture content (FMC) in percent.
#' @param actual_SFC Required. A single numeric value in `[0.1, 6]`. Actual SFC
#'   in kg/m^2.
#'
#' @returns A list of length `5` consisting of numeric values named `Sapling
#'   crown centroid (m)`, `FSG (m)`, `Sapling false-SFC (kg/m^2)`, `SFC scaled
#'   to crown centroid (kg/m^2)`, and `Total false-SFC (kg/m^2)`, respectively.
#' @export
#'
#' @examples
#' ladder_midstory_saplings(
#'   hs = 4.5,
#'   zs = 1.3,
#'   zp = 7,
#'   lnfl = 0.4,
#'   sapling_FMC = 120,
#'   actual_SFC = 2.8
#' )
#'
ladder_midstory_saplings <- function(
    hs,
    zs,
    zp,
    lnfl,
    sapling_FMC,
    actual_SFC
) {
  # Validate and normalize inputs
  validate_input(
    hs = hs,
    zs = zs,
    zp = zp,
    lnfl = lnfl,
    sapling_FMC = sapling_FMC,
    actual_SFC = actual_SFC
  )
  check_length(
    n = 1L,
    hs = hs,
    zs = zs,
    zp = zp,
    lnfl = lnfl,
    sapling_FMC = sapling_FMC,
    actual_SFC = actual_SFC
  )
  hs <- normalize_input(hs, "hs")
  zs <- normalize_input(zs, "zs")
  zp <- normalize_input(zp, "zp")
  lnfl <- normalize_input(lnfl, "lnfl")
  sapling_FMC <- normalize_input(sapling_FMC, "sapling_FMC")
  actual_SFC <- normalize_input(actual_SFC, "actual_SFC")

  # Check that hs > zs
  if (zs >= hs) {
    stop(
      "Sapling total height (hs) must be greater than sapling LCBH (zs).",
      call. = FALSE
    )
  }

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
    "Sapling false-SFC (kg/m^2)" = sapling_SFCF,
    "SFC scaled to crown centroid (kg/m^2)" = SFC_cs,
    "Total false-SFC (kg/m^2)" = total_SFCF
  )
  out[] <- lapply(out, round, 2)

  return(out)
}
