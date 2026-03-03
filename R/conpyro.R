#' Calculate probability of crown fire occurrence (pCFO), crowning thresholds,
#' and rate of spread (ROS) for one or more scenarios
#'
#' `conpyro()` calculates pCFO, crowning thresholds, and ROS for any number of
#' fuel and fire weather configurations using the Conifer Pyrometrics (ConPyro)
#' fire behaviour modelling system. See Perrakis et al. (2023) for details.
#'
#' Input can be supplied in two ways: either via data frame or via arguments.
#' Any given input variable may only be supplied by one of these avenues, but
#' different variables may be supplied by different methods in any combination.
#' For example, attempting to supply `FFMC` via both data frame and argument
#' will yield an error, but supplying `FFMC` via data frame and `FSG` via
#' argument is supported. The sole exception is `WS10`, which can only be
#' supplied via argument.
#'
#' When supplying input via data frame, the data frame must have at least `1`
#' row and `1` column. Each row defines a ConPyro prediction for a single set of
#' fuel and fire weather conditions (a *scenario*), and each column represents a
#' single input parameter. Columns must be named according to the parameter they
#' supply. All inputs are case-insensitive and columns may be in any order.
#'
#' When using arguments, inputs must either be of length `1` or of the common
#' length among all inputs. Length `1` inputs will be automatically recycled to
#' the common length, thus applying to all scenarios. Function parameters are
#' case-sensitive but arguments themselves are case-insensitive.
#'
#' @param data Optional. A data frame of at least `1` row and `1` column.
#'   Columns must be named according to the parameter they supply.
#' @param WS10 Required. A numeric vector in `[0, 60]`. Standard 10-m open wind
#'   speed in km/h.
#' @param FFMC Required. A numeric vector in `[80, 99]`. The Fine Fuel Moisture
#'   Code as per the Canadian Fire Weather Index System.
#' @param FSG Required. A numeric vector in `[0.5, 20]`. Fuel strata gap in
#'   metres, representing the vertical distance between the top of the surface
#'   fuels and the lower limit of the canopy fuels. Analogous to crown base
#'   height in the absence of mid-story ladder fuels. Used in calculating crown
#'   fire probability (pCFO).
#' @param SFC Required. A numeric vector in `[0.1, 6]`. Surface fuel consumption
#'   in kg/m^2. Used in calculating crown fire probability (pCFO) and surface
#'   fire rate of spread (sROS). May be estimated using [t_SFC_FBP()] or
#'   [t_SFC_deGroot()].
#' @param CBD Required. A numeric vector in `[0.01, 0.8]`. Crown bulk density in
#'   kg/m^3. Used in calculating crown fire rate of spread (cROS).
#' @param DMC Optional. A numeric vector in `[5, 250]`. The Duff Moisture Code
#'   as per the Canadian Fire Weather Index System. Used in calculating
#'   stand-adjusted fine dead surface litter moisture content (`mcsa`). If `DMC`
#'   is not supplied, fine fuel moisture calculations will fall back on the
#'   FFMC-based model (`mcF`).
#' @param season Optional. A vector comprising `{"spring", "sp-su", "summer",
#'   "fall"}` or numeric equivalents `{1, 1.5, 2, 3}`. See [t_mcSeason()]. Used
#'   in calculating stand-adjusted fine dead surface litter moisture content
#'   (`mcsa`). If `season` is not supplied, fine fuel moisture calculations will
#'   fall back on the FFMC-based model.
#' @param density Optional. A vector comprising `{"light", "moderate", "dense"}`
#'   or numeric equivalents `{1, 2, 3}`. See [t_mcDensity]. Used in calculating
#'   stand-adjusted fine dead surface litter moisture content (`mcsa`). If
#'   `density` is not supplied, fine fuel moisture calculations will fall back
#'   on the FFMC-based model.
#' @param stand Optional. A vector comprising `{"pine", "spruce", "Douglas-fir",
#'   "deciduous", "mixedwood"}` or abbreviated equivalents `{"p", "s", "df",
#'   "d", "m"}`. Used in calculating stand-adjusted fine dead surface litter
#'   moisture content (`mcsa`). If `stand` is not supplied, fine fuel moisture
#'   calculations will fall back on the FFMC-based model.
#' @param smooth_CFO Optional. A logical vector that defines whether crown fire
#'   occurrence is modeled as a smooth transition (`TRUE`) or an instantaneous
#'   event (`FALSE`). Default is `FALSE`.
#' @param ID Optional. A vector of unique scenario identifiers.
#' @param plot Optional. A character vector to control plotting of output.
#'   Choose any of the following:
#'   * `"pCFO"`: Crown fire occurrence probability.
#'   * `"CAC"`: Criterion for active crowning.
#'   * `"ROS"`: Composite rate of spread plot with crowning thresholds.
#' @param ... Optional. Additional advanced arguments to be passed to nested
#'   functions.
#'
#' @returns A list of lists, with each sub-list containing outputs for a single
#'   ConPyro scenario. Optionally plots output.
#' @export
#'
#' @examples
#' # Basic usage
#' conpyro(
#'   data = default_input,
#'   WS10 = 0:40,
#'   FFMC = 91,
#'   DMC = 85
#' )
#' # Smooth crown fire occurrence for all scenarios
#' conpyro(
#'   data = default_input,
#'   WS10 = 0:40,
#'   FFMC = 91,
#'   DMC = 85,
#'   smooth_CFO = TRUE
#' )
#' # Per-scenario FFMC
#' conpyro(
#'   data = default_input,
#'   WS10 = 0:40,
#'   FFMC = c(90, 91, 92),
#'   DMC = 85,
#' )
#' # Per-scenario smooth crown fire occurrence, with plot
#' conpyro(
#'   data = default_input,
#'   WS10 = 0:40,
#'   FFMC = c(90, 91, 92),
#'   DMC = 85,
#'   smooth_CFO = c(TRUE, FALSE, TRUE),
#'   plot = "ROS"
#' )
#'
conpyro <- function(
    data,
    WS10,
    FFMC,
    FSG,
    SFC,
    CBD,
    DMC,
    season,
    density,
    stand,
    smooth_CFO,
    ID,
    plot = NULL,
    ...
) {
  # Get arguments
  args_list <- introspect_args()

  # Resolve required inputs
  FFMC <- resolve_input(
    args_list = args_list,
    data = data,
    name = "FFMC"
  )
  FSG <- resolve_input(
    args_list = args_list,
    data = data,
    name = "FSG"
  )
  SFC <- resolve_input(
    args_list = args_list,
    data = data,
    name = "SFC"
  )
  CBD <- resolve_input(
    args_list = args_list,
    data = data,
    name = "CBD"
  )

  # Resolve optional inputs without defaults
  DMC <- resolve_input(
    args_list = args_list,
    data = data,
    name = "DMC",
    required = FALSE
  )
  season <- resolve_input(
    args_list = args_list,
    data = data,
    name = "season",
    required = FALSE
  )
  density <- resolve_input(
    args_list = args_list,
    data = data,
    name = "density",
    required = FALSE
  )
  stand <- resolve_input(
    args_list = args_list,
    data = data,
    name = "stand",
    required = FALSE
  )

  # Resolve optional inputs with defaults
  smooth_CFO <- resolve_input(
    args_list = args_list,
    data = data,
    name = "smooth_CFO",
    required = FALSE
  )
  ID <- resolve_input(
    args_list = args_list,
    data = data,
    name = "ID",
    required = FALSE
  )
  CF_thresh <- resolve_input(
    args_list = args_list,
    data = data,
    name = "CF_thresh",
    required = FALSE
  )
  model_mcsa <- resolve_input(
    args_list = args_list,
    data = data,
    name = "model_mcsa",
    required = FALSE
  )
  model_pCFO <- resolve_input(
    args_list = args_list,
    data = data,
    name = "model_pCFO",
    required = FALSE
  )
  model_sROS <- resolve_input(
    args_list = args_list,
    data = data,
    name = "model_sROS",
    required = FALSE
  )
  model_cROS <- resolve_input(
    args_list = args_list,
    data = data,
    name = "model_cROS",
    required = FALSE
  )

  # Enforce strict common length among inputs
  n <- check_common_length(
    FFMC = FFMC,
    FSG = FSG,
    SFC = SFC,
    CBD = CBD,
    DMC = DMC,
    season = season,
    density = density,
    stand = stand,
    smooth_CFO = smooth_CFO,
    ID = ID,
    CF_thresh = CF_thresh,
    model_mcsa = model_mcsa,
    model_pCFO = model_pCFO,
    model_sROS = model_sROS,
    model_cROS = model_cROS
  )
  if (length(FFMC) != n) FFMC <- rep(FFMC, length.out = n)
  if (length(FSG) != n) FSG <- rep(FSG, length.out = n)
  if (length(SFC) != n) SFC <- rep(SFC, length.out = n)
  if (length(CBD) != n) CBD <- rep(CBD, length.out = n)
  if (length(DMC) != n) DMC <- rep(DMC, length.out = n)
  if (length(season) != n) season <- rep(season, length.out = n)
  if (length(density) != n) density <- rep(density, length.out = n)
  if (length(stand) != n) stand <- rep(stand, length.out = n)
  if (length(smooth_CFO) != n) smooth_CFO <- rep(smooth_CFO, length.out = n)
  if (length(ID) != n) {
    if (!is.na(ID)) {
      ID <- paste0(ID, "_", 1:n)
    } else {
      ID <- 1:n
    }
  }
  if (length(CF_thresh) != n) CF_thresh <- rep(CF_thresh, length.out = n)
  if (length(model_mcsa) != n) model_mcsa <- rep(model_mcsa, length.out = n)
  if (length(model_pCFO) != n) model_pCFO <- rep(model_pCFO, length.out = n)
  if (length(model_sROS) != n) model_sROS <- rep(model_sROS, length.out = n)
  if (length(model_cROS) != n) model_cROS <- rep(model_cROS, length.out = n)

  # Validate inputs
  validate_input(
    WS10 = WS10,
    FFMC = FFMC,
    FSG = FSG,
    SFC = SFC,
    CBD = CBD,
    DMC = DMC,
    season = season,
    density = density,
    stand = stand,
    smooth_CFO = smooth_CFO,
    ID = ID,
    CF_thresh = CF_thresh,
    model_mcsa = model_mcsa,
    model_pCFO = model_pCFO,
    model_sROS = model_sROS,
    model_cROS = model_cROS
  )

  # Load coefficients
  coefs_mcsa <- sysdata$coefs_MCSA
  coefs_pCFO <- sysdata$coefs_pCFO

  # Initialize outputs list
  out <- list()
  ggdata <- data.frame()

  # Loop through scenarios and do calculations
  for (i in 1:n) {
    # Assign scenario values, including defaults
    FFMC_val <- FFMC[[i]]
    FSG_val <- FSG[[i]]
    SFC_val <- SFC[[i]]
    CBD_val <- CBD[[i]]
    DMC_val <- DMC[[i]]
    season_val <- season[[i]]
    density_val <- density[[i]]
    stand_val <- stand[[i]]
    mc_type <- if (
      is.na(DMC_val) ||
      is.na(season_val) ||
      is.na(density_val) ||
      is.na(stand_val)
    ) {
      "mcF"
    } else {
      "mcsa"
    }
    smooth_CFO_val <- if (is.na(smooth_CFO[[i]])) FALSE else smooth_CFO[[i]]
    ID_val <- ID[[i]]
    CF_thresh_val <- if (is.na(CF_thresh[[i]])) 0.5 else CF_thresh[[i]]
    model_mcsa_val <- if (is.na(model_mcsa[[i]])) {
      "corrected"
    } else {
      model_mcsa[[i]]
    }
    model_pCFO_val <- if (is.na(model_pCFO[[i]])) {
      if (mc_type == "mcF") 10L else if (mc_type == "mcsa") 11L
    } else {
      model_pCFO[[i]]
    }
    model_sROS_val <- if (is.na(model_sROS[[i]])) {
      if (mc_type == "mcF") 12L else if (mc_type == "mcsa") 13L
    } else {
      model_sROS[[i]]
    }
    model_cROS_val <- if (is.na(model_cROS[[i]])) 1L else model_cROS[[i]]

    # Do calculations
    mcFFMC_val <- mcFFMC(FFMC = FFMC_val)
    if (mc_type == "mcF") {
      mcDMC_val <- NA
      mcsa_idx_val <- NA
      mcsa_val <- NA
      mc_val <- mcFFMC_val
    } else if (mc_type == "mcsa") {
      mcDMC_val <- mcDMC(DMC = DMC_val)
      mcsa_idx_val <- mcsa_idx(
        FFMC = FFMC_val,
        season = season_val,
        density = density_val,
        stand = stand_val,
        model_mcsa = model_mcsa_val
      )
      mcsa_val <- mcsa(
        idx = mcsa_idx_val,
        mcFFMC = mcFFMC_val,
        mcDMC = mcDMC_val,
        coefs = coefs_mcsa
      )
      mc_val <- mcsa_val
    }
    pCFO_val <- pCFO(
      WS10 = WS10,
      mc = mc_val,
      FSG = FSG_val,
      SFC = SFC_val,
      model_pCFO = model_pCFO_val,
      coefs = coefs_pCFO
    )
    surface_fire <- pCFO_val < CF_thresh_val
    sROS_val <- sROS(
      WS10 = WS10,
      mc = mc_val,
      SFC = SFC_val,
      model_sROS_val
    )
    crown_fire <- pCFO_val >= CF_thresh_val
    cROS_A_val <- cROS_A(
      WS10 = WS10,
      mc = mc_val,
      CBD = CBD_val,
      model_cROS = model_cROS_val
    )
    CAC_val <- CAC(
      cROS_A = cROS_A_val,
      CBD = CBD_val
    )
    passive_crowning <- pCFO_val >= CF_thresh_val & CAC_val < 1
    active_crowning <- pCFO_val >= CF_thresh_val & CAC_val >= 1
    cROS_P_val <- if (any(passive_crowning)) {
      cROS_P(
        cROS_A = cROS_A_val,
        CAC = CAC_val
      )
    } else {
      NA
    }
    cROS_P_thresh <- if (any(passive_crowning)) {
      WS10[match(TRUE, passive_crowning)]
    } else {
      NA
    }
    cROS_A_thresh <- if (any(active_crowning)) {
      WS10[match(TRUE, active_crowning)]
    } else {
      NA
    }
    ROS_smooth_val <- if (isTRUE(smooth_CFO_val)) {
      ROS_smooth(
        pCFO = pCFO_val,
        passive_crowning = passive_crowning,
        sROS = sROS_val,
        cROS_P = cROS_P_val,
        cROS_A = cROS_A_val
      )
    } else {
      NA
    }
    if (isTRUE(smooth_CFO_val)) {
      sROS_out <- ROS_smooth_val[surface_fire]
      if (any(passive_crowning)) {
        cROS_P_out <- ROS_smooth_val[passive_crowning]
        cROS_A_out <- cROS_A_val[active_crowning]
      } else {
        cROS_P_out <- NULL
        cROS_A_out <- ROS_smooth_val[active_crowning]
      }
    } else {
      sROS_out <- sROS_val[surface_fire]
      cROS_P_out <- cROS_P_val[passive_crowning]
      cROS_A_out <- cROS_A_val[active_crowning]
    }
    iROS_out <- c(sROS_out, cROS_P_out, cROS_A_out)

    # Add list of results to output list
    out[[as.character(ID_val)]] <- list(
      "mcFFMC (%)" = round(mcFFMC_val, 1),
      "mcsa (%)" = round(mcsa_val, 1),
      "WS10 (km/h)" = WS10,
      "Crown Fire Occurrence Probability" = round(pCFO_val, 2),
      "Passive Crown Fire WS10 Threshold (km/h)" = cROS_P_thresh,
      "Active Crown Fire WS10 Threshold (km/h)" = cROS_A_thresh,
      "Composite Rate of Spread (m/min)" = round(iROS_out, 1)
    )

    # Prepare plotting data
    if (!is.null(plot)) {
      if ("pCFO" %in% plot) {
        ggdata_pCFO <- prep_ggdata(
          ID = ID_val,
          name = "pCFO",
          WS10 = WS10,
          data = pCFO_val
        )

        ggdata <- rbind(ggdata, ggdata_pCFO)
      }

      if ("CAC" %in% plot) {
        ggdata_CAC <- prep_ggdata(
          ID = ID_val,
          name = "CAC",
          WS10 = WS10,
          data = CAC_val
        )

        ggdata <- rbind(ggdata, ggdata_CAC)
      }

      if ("ROS" %in% plot) {
        ggdata_sROS <- prep_ggdata(
          ID = ID_val,
          name = "sROS",
          WS10 = WS10[surface_fire],
          data = sROS_out
        )
        ggdata_cROS_P <- prep_ggdata(
          ID = ID_val,
          name = "cROS_P",
          WS10 = WS10[passive_crowning],
          data = cROS_P_out
        )
        ggdata_cROS_A <- prep_ggdata(
          ID = ID_val,
          name = "cROS_A",
          WS10 = WS10[active_crowning],
          data = cROS_A_out
        )
        # sROS to cROS_P transition
        if (any(surface_fire) && any(passive_crowning)) {
          ggdata_sROS_cROS_P <- data.frame(
            ID = c(ID_val, ID_val),
            name = rep("sROS_cROS_P", times = 2),
            WS10 = WS10[c(
              max(which(surface_fire)),
              min(which(passive_crowning))
            )],
            val = c(max(sROS_out), min(cROS_P_out))
          )
          ggdata_cf_pt_scp <- data.frame(
            ID = ID_val,
            name = "cf_pt_scp",
            WS10 = WS10[min(which(passive_crowning))],
            val = cROS_P_out[[1]]
          )
        } else {
          ggdata_sROS_cROS_P <- NULL
          ggdata_cf_pt_scp <- NULL
        }
        # sROS to cROS_A transition
        if (
          any(surface_fire) &&
          any(active_crowning) &&
          !any(passive_crowning)
        ) {
          ggdata_sROS_cROS_A <- data.frame(
            ID = c(ID_val, ID_val),
            name = rep("sROS_cROS_A", times = 2),
            WS10 = WS10[c(
              max(which(surface_fire)),
              min(which(active_crowning))
            )],
            val = c(max(sROS_out), min(cROS_A_out))
          )
          ggdata_cf_pt_sca <- data.frame(
            ID = ID_val,
            name = "cf_pt_sca",
            WS10 = WS10[min(which(active_crowning))],
            val = cROS_A_out[[1]]
          )
        } else {
          ggdata_sROS_cROS_A <- NULL
          ggdata_cf_pt_sca <- NULL
        }
        # cROS_P to cROS_A transition
        if (any(passive_crowning) && any(active_crowning)) {
          ggdata_cROS_P_cROS_A <- data.frame(
            ID = c(ID_val, ID_val),
            name = rep("cROS_P_cROS_A", times = 2),
            WS10 = WS10[c(
              max(which(passive_crowning)),
              min(which(active_crowning))
            )],
            val = c(max(cROS_P_out), min(cROS_A_out))
          )
          ggdata_cf_pt_cpca <- data.frame(
            ID = ID_val,
            name = "cf_pt_cpca",
            WS10 = WS10[min(which(active_crowning))],
            val = cROS_A_out[[1]]
          )
        } else {
          ggdata_cROS_P_cROS_A <- NULL
          ggdata_cf_pt_cpca <- NULL
        }

        ggdata <- rbind(
          ggdata,
          ggdata_sROS,
          ggdata_cROS_P,
          ggdata_cROS_A,
          ggdata_sROS_cROS_P,
          ggdata_cf_pt_scp,
          ggdata_sROS_cROS_A,
          ggdata_cf_pt_sca,
          ggdata_cROS_P_cROS_A,
          ggdata_cf_pt_cpca
        )

      }
    }
  }

  # Plotting
  if (!is.null(plot)) {
    if ("pCFO" %in% plot) {
      fig_pCFO <- plot_ggdata(
        data = ggdata,
        var = "pCFO",
        xlab = "Wind Speed (km/h)",
        ylab = "Crown Fire Occurrence Probability"
      )

      print(fig_pCFO)
    }

    if ("CAC" %in% plot) {
      fig_CAC <- plot_ggdata(
        data = ggdata,
        var = "CAC",
        xlab = "Wind Speed (km/h)",
        ylab = "Criterion for Active Crowning"
      )

      print(fig_CAC)
    }

    if ("ROS" %in% plot) {
      fig_ROS <- ggplot() +
        # sROS
        geom_line(
          data = subset(ggdata, name == "sROS"),
          mapping = aes(WS10, val, color = ID),
          linewidth = 1.5
        ) +
        # cROS_P
        geom_line(
          data = subset(ggdata, name == "cROS_P"),
          mapping = aes(WS10, val, color = ID),
          linewidth = 1.5
        ) +
        # cROS_A
        geom_line(
          data = subset(ggdata, name == "cROS_A"),
          mapping = aes(WS10, val, color = ID),
          linewidth = 1.5
        ) +
        # Transition lines
        geom_line(
          data = subset(ggdata, name == "sROS_cROS_P"),
          mapping = aes(WS10, val, color = ID),
          linewidth = 1.5,
          linetype = "dotted"
        ) +
        geom_line(
          data = subset(ggdata, name == "sROS_cROS_A"),
          mapping = aes(WS10, val, color = ID),
          linewidth = 1.5,
          linetype = "dotted"
        ) +
        geom_line(
          data = subset(ggdata, name == "cROS_P_cROS_A"),
          mapping = aes(WS10, val, color = ID),
          linewidth = 1.5,
          linetype = "dotted"
        ) +
        # Transition points
        geom_point(
          data = subset(ggdata, name == "cf_pt_scp"),
          mapping = aes(WS10, val, color = ID),
          shape = 16,
          size = 4
        ) +
        geom_point(
          data = subset(ggdata, name == "cf_pt_sca"),
          mapping = aes(WS10, val, color = ID),
          shape = 15,
          size = 4
        ) +
        geom_point(
          data = subset(ggdata, name == "cf_pt_cpca"),
          mapping = aes(WS10, val, color = ID),
          shape = 15,
          size = 4
        ) +
        labs(color = "Scenario") +
        xlab("Wind Speed (km/h)") +
        ylab("Equilibrium Rate of Spread (m/min)") +
        scale_color_viridis_d()

      print(fig_ROS)
    }
  }

  return(out)
}
