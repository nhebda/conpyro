#' Calculate probability of crown fire occurrence (pCFO), crowning thresholds,
#' and rate of spread (ROS) for one or more scenarios
#'
#' `conpyro()` calculates pCFO, crowning thresholds, and ROS for any number of
#' fuel and fire weather configurations using the Conifer Pyrometrics (ConPyro)
#' fire behaviour modelling system. See Perrakis et al. (2023) for details.
#'
#' @param input A data frame of least `7` columns and `1` row. Each row defines
#'   a ConPyro prediction for a single set of fuel and fire weather conditions
#'   (a *scenario*). Inputs are case-insensitive and columns may be in any
#'   order. Missing values are not permitted and will produce an error. Required
#'   columns are:
#'   * `ID`: Unique scenario identifier.
#'   * `Season`: One of `{"spring", "sp-su", "summer", "fall"}` or numeric
#'   equivalents `{1, 1.5, 2, 3}`. See [t_mcSeason()].
#'   * `Density`: One of `{"light", "moderate", "dense"}` or numeric equivalents
#'   `{1, 2, 3}`.
#'   * `Stand`: One of `{"pine", "spruce", "Douglas-fir", "deciduous",
#'   "mixedwood"}` or abbreviated equivalents `{"p", "s", "df", "d", "m"}`.
#'   * `FSG`: A numeric value in `[0.5, 20]`. Fuel strata gap in metres. The
#'   vertical distance between the top of the surface fuels and the lower limit
#'   of the canopy fuels. Analogous to crown base height in the absence of
#'   mid-story ladder fuels.
#'   * `SFC`: A numeric value in `[0.1, 6]`. Surface fuel consumption in kg/m^2.
#'   May be estimated using [t_SFC_FBP()] or [t_SFC_deGroot()].
#'   * `CBD`: A numeric value in `[0.01, 0.8]`. Crown bulk density in kg/m^3.
#'
#'   Additionally, there are several optional columns:
#'   * `smooth_CFO`: Defines whether crown fire initiation is modeled as a
#'   smooth transition (`TRUE`) or an instantaneous occurrence (`FALSE`). Allows
#'   this behaviour to be specified on a per-scenario basis. If this column is
#'   present, it will override the `smooth_CFI` argument.
#'   * `ws_min`: A numeric value in `[0, 60]`. Minimum wind speed in km/h.
#'   Allows wind speed to be specified on a per-scenario basis. If this column
#'   *and* the `ws_max` column are present, they will override the `ws`
#'   argument.
#'   * `ws_max`: A numeric value in `[0, 60]`. Maximum wind speed in km/h.
#'   Allows wind speed to be specified on a per-scenario basis. If this column
#'   *and* the `ws_min` column are present, they will override the `ws`
#'   argument.
#'   * `FFMC`: A numeric value in `[80, 99]`. The Fine Fuel Moisture Code (FFMC)
#'   as per the Canadian Forest Fire Weather Index (FWI) System. Allows FFMC to
#'   be specified on a per-scenario basis. If this column is present, it will
#'   override the `FFMC` argument.
#'   * `DMC`: A numeric value in `[5, 250]`. The Duff Moisture Code (DMC) as per
#'   the Canadian Forest Fire Weather Index (FWI) System. Allows DMC to be
#'   specified on a per-scenario basis. If this column is present, it will
#'   override the `DMC` argument.
#'   * `model_conpyro`: One of `{7, 8, 10, 11}`. The ConPyro model form used for
#'   calculations (see Perrakis et al., 2023). Models `7` and `10` use
#'   FFMC-based fine fuel moisture content (mcFFMC) while models `8` and `11`
#'   use stand-adjusted moisture content (mcsa). Default is `11`.
#'   * `model_SROS`: One of `{1, 2, 3, 4}`. The surface fire ROS model used for
#'   calculations.
#'      * `1`: Aggregated FBPS surf. V4 (default)
#'      * `2`: D-1 FBPS
#'      * `3`: C-6 (surface only) FBPS
#'      * `4`: IsaSFC
#'   * `model_CROS`: One of `{1, 2, 3}`. The crown fire ROS model used for
#'   calculations.
#'      * `1`: Adapted Cruz, Alexander, & Wakimoto (2005): ws, MC, CBD (default)
#'      * `2`: Adapted Cruz & Alexander (2019): ws only
#'      * `3`: Adapted Cruz & Alexander (2019): ws only, 10%
#' @param ws An ordered integer vector of length `2`, with each element being in
#'   `[0, 60]`. The first value defines the minimum wind speed and the second
#'   defines the maximum, in km/h. Calculations are carried out on the sequence
#'   of integer values from the minimum to the maximum, inclusive.
#' @param FFMC A numeric value in `[80, 99]`. The Fine Fuel Moisture Code (FFMC)
#'   as per the Canadian Forest Fire Weather Index (FWI) System.
#' @param DMC A numeric value in `[5, 250]`. The Duff Moisture Code (DMC) as per
#'   the Canadian Forest Fire Weather Index (FWI) System.
#' @param model_mcsa One of `{"original", "corrected"}`. Defines whether the
#'   mcsa calculation uses the original version as per Wotton & Beverly (2007)
#'   or the updated version from Perrakis et al. (2023), which corrects certain
#'   illogical behaviours at high FFMC levels. Specifically, when `model_mcsa =
#'   "corrected"`, if `FFMC > 92.93` and `density = "dense"`, the density class
#'   will be adjusted to `"moderate"`. Similarly, if `FFMC > 96.15` and `density
#'   = "light"`, the density class will be adjusted to `"moderate"`. See
#'   Perrakis et al. (2023) Supplementary Material for details.
#' @param smooth_CFO Defines whether crown fire initiation is modeled as a
#'   smooth transition (`TRUE`) or an instantaneous occurrence (`FALSE`).
#' @param CF_thresh A numeric value in `[0, 1]`. Defines the pCFO threshold at
#'   which crown fire occurs.
#' @param ROS_output A character vector to control ROS output. Choose any of the
#'   following:
#'   * `SROS`: Surface fire rate of spread.
#'   * `CROS_P`: Passive crown fire rate of spread.
#'   * `CROS_A`: Active crown fire rate of spread.
#'   * `integrated`: Complete composite rate of spread.
#' @param plot A character vector to control plotting of output. Choose any of
#'   the following:
#'   * `pCFO`: Crown fire occurrence probability.
#'   * `SROS`: Surface fire rate of spread.
#'   * `CROS_P`: Passive crown fire rate of spread.
#'   * `CROS_A`: Active crown fire rate of spread.
#'   * `CAC`: Criterion for active crowning.
#'   * `ROS_integrated`: Complete composite rate of spread plot with crowning
#'   thresholds.
#' @param ... Additional arguments to be passed to nested functions.
#'
#' @returns A list of lists, with each sub-list containing outputs for a single
#'   scenario (input row). Optionally plots output.
#' @export
#'
#' @examples
#' # Basic usage
#' data(default_input)
#' conpyro(default_input)
#' # Smooth CFO for all scenarios
#' data(default_input)
#' conpyro(default_input, smooth_CFO = TRUE)
#' # Per-scenario smooth CFO with plotting
#' data(default_input)
#' new_input <- cbind(default_input, smooth_CFO = c(TRUE, FALSE, TRUE))
#' conpyro(new_input, plot = "ROS_integrated")
#' # Per-scenario wind speed
#' data(default_input)
#' new_input <- cbind(
#'   default_input,
#'   ws_min = c(0, 10, 15),
#'   ws_max = c(30, 40, 50)
#' )
#' conpyro(new_input)
#' # Per-scenario FFMC
#' data(default_input)
#' new_input <- cbind(default_input, FFMC = c(89, 95, 91.4))
#' conpyro(new_input)
#' # Per-scenario ConPyro, SROS, and CROS models
#' data(default_input)
#' new_input <- cbind(
#'   default_input,
#'   model_conpyro = c(11, 10, 8),
#'   model_SROS = c(1, 2, 4),
#'   model_CROS = c(1, 1, 2)
#' )
#' conpyro(new_input)
#'
#' @importFrom checkmate assert_data_frame assert_subset assert_true
#'   assert_numeric assert_logical assert_integerish test_subset assert_number
#'   assert_choice assert_int makeAssertCollection reportAssertions
#' @importFrom utils read.csv
#'
conpyro <- function(
    data,
    WS10 = NULL,
    FFMC = NULL,
    DMC = NULL,
    model_mcsa = "corrected",
    smooth_CFO = FALSE,
    CF_thresh = 0.5,
    ROS_output = "integrated",
    plot = NULL,
    ...
) {
  # Prepare input ----
  # __ Validate input structure ----
  assert_data_frame(
    input,
    col.names = "named"
  )
  # Normalize case
  input_col_names <- tolower(names(input))
  # Required columns
  required_col_names <- c("fsg", "sfc", "cbd")
  # Identify missing columns
  missing_cols <- setdiff(required_col_names, input_col_names)
  if (length(missing_cols) > 0) {
    stop(
      paste(
        "Input data frame is missing the following required column(s):",
        paste(missing_cols, collapse = ", ")
      )
    )
  }
  # Normalize input character columns to lowercase, except ID, if present
  names(input) <- tolower(names(input))
  cols_to_modify <- if ("id" %in% names(input)) {
    setdiff(names(input), "id")
  } else {
    names(input)
  }
  input[cols_to_modify] <- lapply(input[cols_to_modify], function(x) {
    if (is.character(x)) tolower(x) else x
  })
  # If ID column exists, ensure that IDs are unique
  if ("id" %in% names(input)) {
    assert_true(
      length(input[["id"]]) == length(unique(input[["id"]])),
      .var.name = "Input IDs must be unique"
    )
  }
  # __ Resolve inputs ----
  WS10 <- resolve_input(
    arg = WS10,
    data = data,
    name = "WS10"
  )
  FFMC <- resolve_input(
    arg = FFMC,
    data = data,
    name = "FFMC"
  )
  DMC <- resolve_input(
    arg = DMC,
    data = data,
    name = "DMC"
  )

  # __ Validate inputs ----
  validate_input(
    WS10 = WS10
  )




  cols <- names(input) # this could happen earlier
  fn_validate_input(
    WS10 = if ("ws10" %in% cols) input[[ws10]] else WS10,
    FFMC = if ("ffmc" %in% cols) input[[ffmc]] else FFMC,
    DMC = if ("dmc" %in% cols) input[[dmc]] else DMC,
  )

  # Required columns
  fn_validate_input(
    FSG_vec = input[["fsg"]],
    SFC_vec = input[["sfc"]],
    CBD_vec = input[["cbd"]]
  )
  # Optional columns
  if ("smooth_cfo" %in% names(input)) {
    assert_logical(input$smooth_cfo, .var.name = "smooth_CFO")
  }



  if ("ws_min" %in% names(input)) {
    assert_integerish(input$ws_min, lower = 0, upper = 59, .var.name = "ws_min")
  }
  if ("ws_max" %in% names(input)) {
    assert_integerish(input$ws_max, lower = 1, upper = 60, .var.name = "ws_max")
  }
  if ("ffmc" %in% names(input)) {
    assert_numeric(input$ffmc, lower = 80, upper = 99, .var.name = "FFMC")
  }
  if ("dmc" %in% names(input)) {
    assert_numeric(input$ffmc, lower = 5, upper = 250, .var.name = "DMC")
  }
  if ("model_conpyro" %in% names(input)) {
    assert_subset(input$model_conpyro, choices = c(7, 8, 10, 11))
  }
  if ("model_sros" %in% names(input)) {
    assert_subset(input$model_sros, choices = c(1, 2, 3, 4, 12, 13))
  }
  if ("model_cros" %in% names(input)) {
    assert_subset(input$model_cros, choices = c(1, 2, 3))
  }
  if (length(ws) == 1) {
    assert_integerish(ws, lower = 0, upper = 60)
  } else if (length(ws) > 1) {
    assert_integerish(
      ws,
      lower  = 0,
      upper  = 60,
      unique = TRUE,
      len    = 2,
      sorted = TRUE
    )
  }
  assert_logical(smooth_CFO, .var.name = "smooth_CFO")
  assert_number(CF_thresh, lower = 0, upper = 1)
  assert_subset(ROS_output, c("SROS", "CROS_P", "CROS_A", "integrated"))
  assert_subset(
    plot,
    c(
      "pCFO",
      "SROS",
      "CROS_P",
      "CROS_A",
      "CAC",
      # "SROS_smooth",
      # "CROS_P_smooth",
      # "CROS_A_smooth",
      # "ROS_AIO",
      "ROS_integrated"
    )
  )
  # Initialize data structures
  out <- list()
  ggdata <- data.frame()
  # Loop through input rows (scenarios)
  for (i in 1:nrow(input)) {
    # Assign inputs to vars ----
    extra_args <- list(...)
    ID <- if("id" %in% names(input)) input$id[[i]] else i
    season <- if("season" %in% names(input)) input$season[[i]] else NA
    density <- if("density" %in% names(input)) input$density[[i]] else NA
    stand <- if("stand" %in% names(input)) input$stand[[i]] else NA
    FSG <- input$fsg[i]
    SFC <- input$sfc[i]
    CBD <- input$cbd[i]
    smooth_CFO <- if ("smooth_cfo" %in% names(input)) {
      input$smooth_cfo[i]
    } else {
      smooth_CFO
    }
    ws_seq <- if ("ws" %in% names(input)) {
      input$ws[i]
    } else if ("ws_min" %in% names(input) && "ws_max" %in% names(input)) {
      seq(input$ws_min[i], input$ws_max[i], 1)
    } else {
      if (length(ws) == 1) {
        ws
      } else {
        seq(ws[1], ws[2], 1)
      }
    }
    FFMC          <- if ("ffmc" %in% names(input)) {
      input$ffmc[i]
    } else {
      FFMC
    }
    DMC           <- if ("dmc" %in% names(input)) {
      input$dmc[i]
    } else {
      DMC
    }
    model_conpyro <- if ("model_conpyro" %in% names(input)) {
      input$model_conpyro[i]
    } else {
      11
    }
    model_SROS    <- if ("model_sros" %in% names(input)) {
      input$model_sros[i]
    } else if ("model_SROS" %in% names(extra_args)) {
      extra_args$model_SROS
    } else {
      if (NA %in% c(season, density, stand)) 12 else 13
    }
    model_CROS    <- if ("model_cros" %in% names(input)) {
      input$model_cros[i]
    } else {
      1
    }
    # Do calculations
    mcFFMC        <- fn_mcFFMC(FFMC)
    mcDMC         <- fn_mcDMC(DMC)
    if (!NA %in% c(season, density, stand)) {
      idx         <- fn_mcsa_idx(FFMC, season, density, stand, model_mcsa)
      mcsa        <- fn_mcsa(idx, mcFFMC, mcDMC)
      MC          <- switch(
        as.character(model_conpyro),
        "7"  = mcFFMC,
        "8"  = mcsa,
        "10" = mcFFMC,
        "11" = mcsa
      )
    } else {
      mcsa = NA
      MC   = mcFFMC
      if (!model_conpyro %in% c(7, 10)) model_conpyro <- 10
      message(
        "Missing value(s) detected in ",
        ID,
        ", using mcFFMC with ConPyro Model ",
        model_conpyro,
        "."
      )
    }
    pCFO          <- fn_pCFO(model_conpyro, ws_seq, FSG, SFC, MC)
    CF_CI         <- fn_CF_CI(ws_seq, pCFO)
    SROS          <- fn_SROS(
      model  = model_SROS,
      ws_seq = ws_seq,
      mc     = MC,
      SFC    = SFC
    )
    CROS_A        <- fn_CROS_A(model_CROS, MC, ws_seq, CBD)
    CAC           <- fn_CAC(CROS_A, CBD)
    CROS_P        <- fn_CROS_P(CROS_A, CAC)
    SROS_smooth   <- fn_SROS_smooth(CROS_P, pCFO, CF_thresh, CAC, SROS, CROS_A)
    CROS_P_smooth <- fn_CROS_P_smooth(SROS, pCFO, CROS_P)
    CROS_A_smooth <- fn_CROS_A_smooth(SROS, pCFO, CROS_A)
    SROS_out      <- fn_SROS_out(pCFO, smooth_CFO, CF_thresh, SROS_smooth, SROS)
    CROS_P_out    <- fn_CROS_P_out(
      pCFO,
      CAC,
      smooth_CFO,
      CF_thresh,
      CROS_P_smooth,
      CROS_P
    )
    CROS_A_out    <- fn_CROS_A_out(
      pCFO,
      CAC,
      smooth_CFO,
      CF_thresh,
      CROS_P,
      CROS_A_smooth,
      CROS_A
    )
    ROS_AIO       <- c(SROS_out, CROS_P_out, CROS_A_out)
    # Prepare output data
    results <- list(
      "mcFFMC" = round(mcFFMC, 2),
      "mcsa" = round(mcsa, 2),
      "Wind Speed (km/h)" = ws_seq,
      "Crown Fire Occurrence Probability (pCFO)" = round(pCFO, 2)
    )
    if ("SROS" %in% ROS_output) {
      results[["Surface Fire Rate of Spread (m/min)"]] = round(SROS, 2)
    }
    if ("CROS_P" %in% ROS_output) {
      results[["Crown Fire (Passive) Rate of Spread (m/min)"]] =
        round(CROS_P, 2)
    }
    if ("CROS_A" %in% ROS_output) {
      results[["Crown Fire (Active) Rate of Spread (m/min)"]] =
        round(CROS_A, 2)
    }
    if ("integrated" %in% ROS_output) {
      results[["Integrated Rate of Spread (m/min)"]] = round(ROS_AIO, 2)
    }
    if (length(SROS_out) > 0 & length(CROS_P_out > 0)) {
      results[["Crown Fire (Passive) Wind Speed Threshold (km/h)"]] =
        ws_seq[length(SROS_out) + 1]
    }
    if (
      (length(SROS_out) > 0 | length(CROS_P_out > 0)) & length(CROS_A_out) > 0
    ) {
      results[["Crown Fire (Active) Wind Speed Threshold (km/h)"]] =
        ws_seq[length(ws_seq) - length(CROS_A_out) + 1]
    }
    if (!is.na(CF_CI[1]) & length(ws_seq) > 1) {
      results[["Crown Fire Wind Speed Threshold Lower Bound (km/h)"]] = CF_CI[1]
    }
    if (!is.na(CF_CI[2]) & length(ws_seq) > 1) {
      results[["Crown Fire Wind Speed Threshold Upper Bound (km/h)"]] = CF_CI[2]
    }
    out[[i]] <- results
    # Prepare plotting data
    if (!is.null(plot)) {
      ggdata_pCFO          <- fn_prep_ggdata(ID, ws_seq, pCFO)
      ggdata_SROS          <- fn_prep_ggdata(ID, ws_seq, SROS)
      ggdata_CROS_A        <- fn_prep_ggdata(ID, ws_seq, CROS_A)
      ggdata_CAC           <- fn_prep_ggdata(ID, ws_seq, CAC)
      ggdata_CROS_P        <- fn_prep_ggdata(ID, ws_seq, CROS_P)
      ggdata_SROS_smooth   <- fn_prep_ggdata(ID, ws_seq, SROS_smooth)
      ggdata_CROS_P_smooth <- fn_prep_ggdata(ID, ws_seq, CROS_P_smooth)
      ggdata_CROS_A_smooth <- fn_prep_ggdata(ID, ws_seq, CROS_A_smooth)
      ggdata_SROS_out      <- fn_prep_ggdata(
        ID,
        ws_seq[which(pCFO < CF_thresh)],
        SROS_out
      )
      ggdata_CROS_P_out    <- fn_prep_ggdata(
        ID,
        ws_seq[which(pCFO >= CF_thresh & CAC < 1)],
        CROS_P_out
      )
      ggdata_CROS_A_out    <- fn_prep_ggdata(
        ID,
        ws_seq[which(pCFO >= CF_thresh & CAC > 1)],
        CROS_A_out
      )
      ggdata_CF_CI         <- if (!NA %in% CF_CI) {
        data.frame(
          ID  = rep(ID, times = 2),
          var = rep("CF_CI", times = 2),
          ws  = CF_CI,
          val = if (length(CROS_P_out) > 0) {
            rep(CROS_P_out[1], times = 2)
          } else if (length(CROS_A_out) > 0) {
            rep(CROS_A_out[1], times = 2)
          } else NULL
        )
      }
      ggdata_ROS_AIO       <- fn_prep_ggdata(ID, ws_seq, ROS_AIO)
      ggdata_SROS_CROS_P   <- if (
        length(SROS_out) > 0 & length(CROS_P_out) > 0
      ) {
        data.frame(
          ID  = rep(ID, times = 2),
          var = rep("SROS_CROS_P", times = 2),
          ws  = ws_seq[c(length(SROS_out), length(SROS_out) + 1)],
          val = c(max(SROS_out), min(CROS_P_out))
        )
      } else {
        NULL
      }
      ggdata_SROS_CROS_A   <- if (
        length(SROS_out) > 0 & length(CROS_A_out) > 0 & length(CROS_P_out) == 0
      ) {
        data.frame(
          ID  = rep(ID, times = 2),
          var = rep("SROS_CROS_A", times = 2),
          ws  = ws_seq[c(length(SROS_out), length(SROS_out) + 1)],
          val = c(max(SROS_out), min(CROS_A_out))
        )
      } else {
        NULL
      }
      ggdata_CROS_P_CROS_A <- if (
        length(CROS_P_out) > 0 & length(CROS_A_out) > 0
      ) {
        data.frame(
          ID  = rep(ID, times = 2),
          var = rep("CROS_P_CROS_A", times = 2),
          ws  = ws_seq[c(
            length(ws_seq) - length(CROS_A_out),
            length(ws_seq) - length(CROS_A_out) + 1
          )],
          val = c(max(CROS_P_out), min(CROS_A_out))
        )
      } else {
        NULL
      }
      ggdata_cf_pt_scp  <- if (
        length(SROS_out) > 0 & length(CROS_P_out) > 0
      ) {
        data.frame(
          ID  = ID,
          var = "cf_pt_scp",
          ws  = ggdata_SROS_CROS_P[2, 3],
          val = ggdata_SROS_CROS_P[2, 4]
        )
      } else {
        NULL
      }
      ggdata_cf_pt_sca  <- if (
        length(SROS_out) > 0 & length(CROS_A_out) > 0 & length(CROS_P_out) == 0
      ) {
        data.frame(
          ID  = ID,
          var = "cf_pt_sca",
          ws  = ggdata_SROS_CROS_A[2, 3],
          val = ggdata_SROS_CROS_A[2, 4]
        )
      } else {
        NULL
      }
      ggdata_cf_pt_cpca <- if (
        length(CROS_P_out) > 0 & length(CROS_A_out) > 0
      ) {
        data.frame(
          ID  = ID,
          var = "cf_pt_cpca",
          ws  = ggdata_CROS_P_CROS_A[2, 3],
          val = ggdata_CROS_P_CROS_A[2, 4]
        )
      } else {
        NULL
      }
      ggdata <- rbind(
        ggdata,
        ggdata_pCFO,
        ggdata_SROS,
        ggdata_CROS_A,
        ggdata_CAC,
        ggdata_CROS_P,
        ggdata_SROS_smooth,
        ggdata_CROS_P_smooth,
        ggdata_CROS_A_smooth,
        ggdata_SROS_out,
        ggdata_CROS_P_out,
        ggdata_CROS_A_out,
        ggdata_CF_CI,
        ggdata_ROS_AIO,
        ggdata_SROS_CROS_P,
        ggdata_SROS_CROS_A,
        ggdata_CROS_P_CROS_A,
        ggdata_cf_pt_scp,
        ggdata_cf_pt_sca,
        ggdata_cf_pt_cpca
      )
    }
  }
  # Plotting
  if ("pCFO" %in% plot) {
    fig_pCFO <- fn_plot(
      ggdata,
      "pCFO",
      "Wind Speed [km/h]",
      "Probability of Crown Fire Occurrence [0-1]"
    )
    print(fig_pCFO)
  }
  if ("SROS" %in% plot) {
    fig_SROS <- fn_plot(
      ggdata,
      "SROS",
      "Wind Speed [km/h]",
      "Equilibrium Surface Fire Rate of Spread [m/min]"
    )
    print(fig_SROS)
  }
  if ("CROS_A" %in% plot) {
    fig_CROS_A <- fn_plot(
      ggdata,
      "CROS_A",
      "Wind Speed [km/h]",
      "Equilibrium Active Crown Fire Rate of Spread [m/min]"
    )
    print(fig_CROS_A)
  }
  if ("CAC" %in% plot) {
    fig_CAC <- fn_plot(
      ggdata,
      "CAC",
      "Wind Speed [km/h]",
      "Criterion for Active Crowning [0-1]"
    )
    print(fig_CAC)
  }
  if ("CROS_P" %in% plot) {
    fig_CROS_P <- fn_plot(
      ggdata,
      "CROS_P",
      "Wind Speed [km/h]",
      "Equilibrium Passive Crown Fire Rate of Spread [m/min]"
    )
    print(fig_CROS_P)
  }
  if ("SROS_smooth" %in% plot) {
    fig_SROS_smooth <- fn_plot(
      ggdata,
      "SROS_smooth",
      "Wind Speed [km/h]",
      "SROS_smooth"
    )
    print(fig_SROS_smooth)
  }
  if ("CROS_P_smooth" %in% plot) {
    fig_CROS_P_smooth <- fn_plot(
      ggdata,
      "CROS_P_smooth",
      "Wind Speed [km/h]",
      "CROS_P_smooth"
    )
    print(fig_CROS_P_smooth)
  }
  if ("CROS_A_smooth" %in% plot) {
    fig_CROS_A_smooth <- fn_plot(
      ggdata,
      "CROS_A_smooth",
      "Wind Speed [km/h]",
      "CROS_A_smooth"
    )
    print(fig_CROS_A_smooth)
  }
  if ("ROS_AIO" %in% plot) {
    fig_ROS_AIO <- fn_plot(
      ggdata,
      "ROS_AIO",
      "Wind Speed [km/h]",
      "Equilibrium Rate of Spread [m/min]"
    )
    print(fig_ROS_AIO)
  }
  # Main ROS plot
  if ("ROS_integrated" %in% plot) {
    fig_ROS_integrated <- ggplot() +
      # SROS
      geom_line(
        data = subset(ggdata, var == "SROS_out"),
        mapping = aes(ws, val, color = ID),
        linewidth = 1.5
      ) +
      # CROSp
      geom_line(
        data = subset(ggdata, var == "CROS_P_out"),
        mapping = aes(ws, val, color = ID),
        linewidth = 1.5
      ) +
      # CROSa
      geom_line(
        data = subset(ggdata, var == "CROS_A_out"),
        mapping = aes(ws, val, color = ID),
        linewidth = 1.5
      ) +
      # Transition lines
      geom_line(
        data = subset(ggdata, var == "SROS_CROS_P"),
        mapping = aes(ws, val, color = ID),
        linewidth = 1.5,
        linetype = "dotted"
      ) +
      geom_line(
        data = subset(ggdata, var == "SROS_CROS_A"),
        mapping = aes(ws, val, color = ID),
        linewidth = 1.5,
        linetype = "dotted"
      ) +
      geom_line(
        data = subset(ggdata, var == "CROS_P_CROS_A"),
        mapping = aes(ws, val, color = ID),
        linewidth = 1.5,
        linetype = "dotted"
      ) +
      # CF confidence intervals
      geom_line(
        data = subset(ggdata, var == "CF_CI"),
        mapping = aes(ws, val, color = ID),
        linewidth = 1,
        alpha = 0.5
      ) +
      # Transition points
      # Crown fire point SROS CROS_P
      geom_point(
        data = subset(ggdata, var == "cf_pt_scp"),
        mapping = aes(ws, val, color = ID),
        shape = 16,
        size = 4
      ) +
      # Crown fire point SROS CROS_A
      geom_point(
        data = subset(ggdata, var == "cf_pt_sca"),
        mapping = aes(ws, val, color = ID),
        shape = 15,
        size = 4
      ) +
      # Crown fire point CROS_P CROS_A
      geom_point(
        data = subset(ggdata, var == "cf_pt_cpca"),
        mapping = aes(ws, val, color = ID),
        shape = 15,
        size = 4
      ) +
      # scale_x_continuous(breaks = seq(0, 60, 5), limits = c(0, 60)) +
      # scale_y_continuous(breaks = seq(0, 120, 10), limits = c(0, 110)) +
      labs(color = "Scenario") +
      xlab("Wind Speed [km/h]") +
      ylab("Equilibrium Rate of Spread [m/min]") +
      scale_color_viridis_d()
    print(fig_ROS_integrated)
  }
  # Output
  names(out) <- input$id
  return(out)
}

# Internal functions ----

# __Confidence intervals
fn_CF_CI <- function(ws_seq, pCFO, CI = 90) {
  lower <- if (any(pCFO > (0.5 - (CI / 200)))) {
    ws_seq[min(which(pCFO > (0.5 - (CI / 200))))]
  } else {
    NA
  }
  upper <- if (any(pCFO > (0.5 + (CI / 200)))) {
    ws_seq[min(which(pCFO > (0.5 + (CI / 200))))]
  } else {
    NA
  }
  out   <- c(lower, upper)
  return(out)
}

# __Surface fire rate of spread (sROS) ----
fn_SROS <- function(model, ws_seq, mc, SFC) {
  ISI  <- fn_ISI(ws_seq = ws_seq, mc = mc)
  sROS <- switch(
    as.character(model),
    # FBPS aggregated surf. V4
    "1"  = 25 * (1 - exp(-0.035177 * ISI))^1.9875, # ST-X-3 Eq. 26
    # FBPS D-1 (no BE)
    "2"  = 30 * (1 - exp(-0.0232 * ISI))^1.6, # ST-X-3 Eq. 26 & Tbl. 6
    # FBPS C-6 (surface only, no BE)
    "3"  = 30 * (1 - exp(-0.08 * ISI))^3, # ST-X-3 Eq. 62
    # ISI2SFC
    "4"  = 0.015822 * ISI^2 + 0.344379 * SFC,
    # m12 sl.con.ISI (Perrakis et al., 2026): default for mcFFMC
    "12" = (0.15 * ISI + 13) * (1-exp(-0.13498 * ISI))^5.773107, # Tbls. 1 & A2
    # m13 sl.con.isim (Perrakis et al., 2026): default for mcsa
    "13" = (0.15 * ISI + 13) * (1-exp(-0.101379 * ISI))^4.164469 # Tbls. 1 & A2
  )
  return(sROS)
}

# __Crown fire rate of spread (CROS) ----
# Active crown fire rate of spread (CROS_A)
fn_CROS_A <- function(model, MC, ws_seq, CBD) {
  if (model == 1) {
    effm_mod <- -0.4812 + 3.8842 * log(mc) # Coefficients updated 2026/02/13
    CROS_A <- 11.02 * (ws_seq^0.9) * CBD^0.19 * exp(-0.17 * effm_mod)
  } else if (model == 2) {
    CROS_A <- 0.084 * ws_seq * 1000 / 60
  } else if (model == 3) {
    CROS_A <- 0.1 * ws_seq * 1000 / 60
  }
  return(CROS_A)
}
# Criteria for active crowning (CAC)
fn_CAC <- function(CROS_A, CBD) {CROS_A / (3 / CBD)}
# Passive crown fire rate of spread (CROS_P)
fn_CROS_P <- function(CROS_A, CAC) {CROS_A * exp(-CAC)}

# __Smooth crown fire initiation ----
fn_SROS_smooth <- function(CROS_P, pCFO, CF_thresh, CAC, SROS, CROS_A) {
  if (sum(CROS_P[which(pCFO >= CF_thresh & CAC < 1)]) > 0) {
    (SROS * (1 - pCFO)) + (CROS_P * pCFO)
  } else {
    (SROS * (1 - pCFO)) + (CROS_A * pCFO)
  }
}
fn_CROS_P_smooth <- function(SROS, pCFO, CROS_P) {
  (SROS * (1 - pCFO)) + (CROS_P * pCFO)
}
fn_CROS_A_smooth <- function(SROS, pCFO, CROS_A) {
  (SROS * (1 - pCFO)) + (CROS_A * pCFO)
}

# __Final outputs ----
# Output SROS when p(CFO) < CF_thresh
fn_SROS_out <- function(pCFO, smooth_CFO, CF_thresh, SROS_smooth, SROS) {
  ROS <- if (isTRUE(smooth_CFO)) {
    SROS_smooth[which(pCFO < CF_thresh)]
  } else {
    SROS[which(pCFO < CF_thresh)]
  }
  return(ROS)
}
# Output CROS_P when p(CFO) >= CF_thresh and CAC < 1
fn_CROS_P_out <- function(
    pCFO,
    CAC,
    smooth_CFO,
    CF_thresh,
    CROS_P_smooth,
    CROS_P
) {
  ROS <- if (isTRUE(smooth_CFO)) {
    CROS_P_smooth[which(pCFO >= CF_thresh & CAC < 1)]
  } else {
    CROS_P[which(pCFO >= CF_thresh & CAC < 1)]
  }
  return(ROS)
}
# Output CROS_A if p(CFO) >= CF_thresh and CAC > 1
fn_CROS_A_out <- function(
    pCFO,
    CAC,
    smooth_CFO,
    CF_thresh,
    CROS_P,
    CROS_A_smooth,
    CROS_A
) {
  ROS <- if (
    isTRUE(smooth_CFO) & sum(CROS_P[which(pCFO >= CF_thresh & CAC < 1)]) == 0
  ) {
    CROS_A_smooth[which(pCFO >= CF_thresh & CAC > 1)]
  } else {
    CROS_A[which(pCFO >= CF_thresh & CAC > 1)]
  }
  return(ROS)
}
