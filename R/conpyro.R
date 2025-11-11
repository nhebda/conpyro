#' Calculate probability of crown fire occurrence (pCFO), crowning thresholds,
#' and rate of spread (ROS) for one or more scenarios
#'
#' `conpyro()` calculates pCFO, crowning thresholds, and ROS for any number of
#' fuel and fire weather configurations using the Canadian Conifer Pyrometrics
#' (ConPyro) model system. See Perrakis et al. (2023) for details.
#'
#' @param input A data frame of least `7` columns and `1` row. Each row defines
#'   a ConPyro prediction for a single set of fuel and fire weather conditions
#'   (a *scenario*). Inputs are case-insensitive and columns may be in any
#'   order. Missing values are not permitted and will produce an error.
#'   Required columns are:
#'   * `ID`: Unique scenario identifier.
#'   * `Season`: One of `spring`, `sp-su`, `summer`, or `fall`.
#'   * `Density`: One of `light`, `moderate`, or `dense`.
#'   * `Stand`: One of `pine`, `spruce`, `Douglas-fir`, `deciduous`, or
#'   `mixedwood`.
#'   * `FSG`: A numeric value between `0.5` and `20` (inclusive). Fuel strata
#'   gap in metres. The vertical distance between the top of the surface fuels
#'   and the lower limit of the canopy fuels. Analogous to crown base height
#'   (CBH) in the absence of mid-story ladder fuels.
#'   * `SFC`: A numeric value between `0.1` and `6` (inclusive). Surface fuel
#'   consumption in kg/m^2. May be estimated using [tool_SFC_FBP()] or
#'   [tool_SFC_deGroot()].
#'   * `CBD`: A numeric value between `0.01` and `0.8` (inclusive). Crown bulk
#'   density in kg/m^3.
#'
#'   Additionally, there are several optional columns:
#'   * `smooth_CFO`: Defines whether crown fire initiation is modeled as a
#'   smooth transition (`TRUE`) or an instantaneous occurrence (`FALSE`). Allows
#'   this behaviour to be specified on a per-scenario basis. If this column is
#'   present, it will override the `smooth_CFI` argument.
#'   * `WS_min`: A numeric value between `0` and `60` (inclusive). Minimum wind
#'   speed in km/h. Allows wind speed to be specified on a per-scenario basis.
#'   If this column *and* the `WS_max` column are present, they will override
#'   the `WS` argument.
#'   * `WS_max`: A numeric value between `0` and `60` (inclusive). Maximum wind
#'   speed in km/h. Allows wind speed to be specified on a per-scenario basis.
#'   If this column *and* the `WS_min` column are present, they will override
#'   the `WS` argument.
#'   * `FFMC`: A numeric value between 80 and 99 (inclusive). The Fine Fuel
#'   Moisture Code (FFMC) as per the Canadian Forest Fire Weather Index System.
#'   Allows FFMC to be specified on a per-scenario basis. If this column is
#'   present, it will override the `FFMC` argument.
#'   * `DMC`: A numeric value between 80 and 99 (inclusive). The Duff Moisture
#'   Code (DMC) as per the Canadian Forest Fire Weather Index (FWI) System.
#'   Allows DMC to be specified on a per-scenario basis. If this column is
#'   present, it will override the `DMC` argument.
#'   * `model_conpyro`: Choose one of `7`, `8`, `10`, or `11`. The ConPyro
#'   model form used for calculations (see Perrakis et al., 2023). Models `7`
#'   and `10` use FFMC-based fine fuel moisture content (MCFFMC) while models
#'   `8` and `11` use stand-adjusted moisture content (MCSA). Default is `11`.
#'   * `model_SROS`: Choose one of `1`, `2`, `3`, or `4`. The surface fire ROS
#'   model used for calculations.
#'      * `1`: Aggregated FBPS surf. V4 (default)
#'      * `2`: D-1 FBPS
#'      * `3`: C-6 (surface only) FBPS
#'      * `4`: IsaSFC
#'   * `model_CROS`: Choose one of `1` or `2`. The crown fire ROS model used
#'   for calculations.
#'      * `1`: Adapted Cruz, Alexander, & Wakimoto (2005): WS, MC, CBD (default)
#'      * `2`: Adapted Cruz & Alexander (2019): WS only
#' @param WS An ordered integer vector of length `2`, with each element being
#'   between `0` and `60` (inclusive). The first value defines the minimum wind
#'   speed and the second defines the maximum, in km/h. Calculations are carried
#'   out on the sequence of integer values from the minimum to the maximum
#'   (inclusive).
#' @param FFMC A numeric value between 80 and 99 (inclusive). The Fine Fuel
#'   Moisture Code (FFMC) as per the Canadian Forest Fire Weather Index System.
#' @param DMC A numeric value between 5 and 200 (inclusive). The Duff Moisture
#'   Code (DMC) as per the Canadian Forest Fire Weather Index (FWI) System.
#' @param smooth_CFO Defines whether crown fire initiation is modeled as a
#'   smooth transition (`TRUE`) or an instantaneous occurrence (`FALSE`).
#' @param plot A character vector to control plotting of output. Choose any of
#'   the following:
#'   * `pCFO`: Crown fire occurrence probability.
#'   * `SROS`: Surface fire rate of spread.
#'   * `CROS_P`: Passive crown fire rate of spread.
#'   * `CROS_A`: Active crown fire rate of spread.
#'   * `CAC`: Criterion for active crowning.
#'   * `ROS_full`: Complete composite rate of spread plot with crowning
#'   thresholds.
#'
#' @returns A list of lists, with each sub-list containing outputs for a single
#'   scenario (input row). Optionally plots output.
#' @export
#'
#' @examples
#' # Basic usage
#' data(input)
#' conpyro(input)
#' # Smooth CFO for all scenarios
#' data(input)
#' conpyro(input, smooth_CFO = TRUE)
#' # Per-scenario smooth CFO with plotting
#' data(input)
#' input <- cbind(input, smooth_CFO = c(TRUE, FALSE, TRUE))
#' conpyro(input, plot = "ROS_full")
#' # Per-scenario wind speed
#' data(input)
#' input <- cbind(input, WS_min = c(0, 10, 15), WS_max = c(30, 40, 50))
#' conpyro(input)
#' # Per-scenario FFMC
#' data(input)
#' input <- cbind(input, FFMC = c(89, 95, 91.4))
#' conpyro(input)
#' # Per-scenario ConPyro, SROS, and CROS models
#' data(input)
#' input <- cbind(
#'   input,
#'   model_conpyro = c(11, 10, 8),
#'   model_SROS = c(1, 2, 4),
#'   model_CROS = c(1, 1, 2)
#' )
#' conpyro(input)
#'
#' @importFrom cffdrs fbp
#' @importFrom checkmate assert_data_frame assert_subset assert_true
#'   assert_numeric assert_logical assert_integerish
#' @importFrom utils read.csv
conpyro <- function(
    input,
    WS         = c(0, 40),
    FFMC       = 91,
    DMC        = 70,
    smooth_CFO = FALSE,
    plot       = NULL
) {
  # Coerce input data to all lowercase
  input[] <- lapply(input, function(col) {
    if (is.character(col)) tolower(col) else col
  })
  colnames(input) <- tolower(colnames(input))
  # Validate user input
  assert_data_frame(
    input,
    any.missing = FALSE,
    all.missing = FALSE,
    col.names   = "named"
  )
  assert_subset(
    c(
      "id",
      "season",
      "density",
      "stand",
      "fsg",
      "sfc",
      "cbd"
    ),
    colnames(input)
  )
  assert_true(
    length(input$id) == length(unique(input$id)),
    .var.name = "Input IDs must be unique"
  )
  assert_subset(
    input$season,
    choices = c("spring","sp-su", "summer", "fall"),
    empty.ok = FALSE,
    .var.name = "season"
  )
  assert_subset(
    input$density,
    choices = c("light", "moderate", "dense"),
    empty.ok = FALSE,
    .var.name = "density"
  )
  assert_subset(
    input$stand,
    choices = c("deciduous", "douglas-fir", "mixedwood", "pine", "spruce"),
    empty.ok = FALSE,
    .var.name = "stand"
  )
  assert_numeric(input$fsg, lower = 0.5, upper = 20, .var.name = "FSG")
  assert_numeric(input$sfc, lower = 0.1, upper = 6, .var.name = "SFC")
  assert_numeric(input$cbd, lower = 0.01, upper = 0.8, .var.name = "CBD")
  if ("smooth_cfo" %in% names(input)) {
    assert_logical(input$smooth_cfo, .var.name = "smooth_CFO")
  }
  if ("ws_min" %in% names(input)) {
    assert_integerish(input$ws_min, lower = 0, upper = 59, .var.name = "WS_min")
  }
  if ("ws_max" %in% names(input)) {
    assert_integerish(input$ws_max, lower = 1, upper = 60, .var.name = "WS_max")
  }
  if ("ffmc" %in% names(input)) {
    assert_numeric(input$ffmc, lower = 80, upper = 99, .var.name = "FFMC")
  }
  if ("dmc" %in% names(input)) {
    assert_numeric(input$ffmc, lower = 5, upper = 200, .var.name = "DMC")
  }
  assert_integerish(
    WS,
    lower  = 0,
    upper  = 60,
    len    = 2,
    unique = TRUE,
    sorted = TRUE
  )
  assert_number(FFMC, lower = 80, upper = 99)
  assert_number(DMC, lower = 5, upper = 200)
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
      "ROS_full"
    )
  )

  # Initialize data structures
  out       <- list()
  ggdata    <- data.frame()
  # Loop through input rows (scenarios)
  for (i in 1:nrow(input)) {
    # Assign inputs to vars
    ID            <- input$id[i]
    season        <- input$season[i]
    density       <- input$density[i]
    stand         <- input$stand[i]
    FSG           <- input$fsg[i]
    SFC           <- input$sfc[i]
    CBD           <- input$cbd[i]
    smooth_CFO    <- if ("smooth_cfo" %in% names(input)) {
      input$smooth_cfo[i]
    } else {
      smooth_CFO
    }
    WS_seq        <- if (
      "ws_min" %in% names(input) & "ws_max" %in% names(input)
    ) {
      seq(input$ws_min[i], input$ws_max[i], 1)
    } else {
      seq(WS[1], WS[2], 1)
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
    model_SROS    <- if ("model_SROS" %in% names(input)) {
      input$model_SROS[i]
    } else {
      1
    }
    model_CROS    <- if ("model_CROS" %in% names(input)) {
      input$model_CROS[i]
    } else {
      1
    }
    # Do calculations
    MCFFMC        <- fn_MCFFMC(FFMC)
    MCDMC         <- fn_MCDMC(DMC)
    idx           <- fn_MCSA_idx(season, density, stand)
    MCSA          <- fn_MCSA(idx, MCFFMC, MCDMC)
    MC            <- switch(
      as.character(model_conpyro),
      "7"  = MCFFMC,
      "8"  = MCSA,
      "10" = MCFFMC,
      "11" = MCSA
    )
    pCFO          <- fn_pCFO(model_conpyro, WS_seq, FSG, SFC, MC)
    CF_CI         <- fn_CF_CI(WS_seq, pCFO)
    SROS          <- fn_SROS(model_SROS, WS_seq, MC, FFMC, SFC)
    CROS_A        <- fn_CROS_A(model_CROS, MC, WS_seq, CBD)
    CAC           <- fn_CAC(CROS_A, CBD)
    CROS_P        <- fn_CROS_P(CROS_A, CAC)
    SROS_smooth   <- fn_SROS_smooth(CROS_P, pCFO, CAC, SROS, CROS_A)
    CROS_P_smooth <- fn_CROS_P_smooth(SROS, pCFO, CROS_P)
    CROS_A_smooth <- fn_CROS_A_smooth(SROS, pCFO, CROS_A)
    SROS_out      <- fn_SROS_out(pCFO, smooth_CFO, SROS_smooth, SROS)
    CROS_P_out    <- fn_CROS_P_out(
      pCFO,
      CAC,
      smooth_CFO,
      CROS_P_smooth,
      CROS_P
    )
    CROS_A_out    <- fn_CROS_A_out(
      pCFO,
      CAC,
      smooth_CFO,
      CROS_P,
      CROS_A_smooth,
      CROS_A
    )
    ROS_AIO       <- c(SROS_out, CROS_P_out, CROS_A_out)
    # Prepare output data
    results <- list(
      "MCFFMC"        = round(MCFFMC, 2),
      # "MCDMC"         = MCDMC,
      # "MCSA_idx"      = idx,
      "MCSA"          = round(MCSA, 2),
      "WS_seq"        = WS_seq,
      "pCFO"          = round(pCFO, 2),
      # "SROS"          = SROS,
      # "CROS_A"        = CROS_A,
      # "CAC"           = CAC,
      # "CROS_P"        = CROS_P,
      # "SROS_smooth"   = SROS_smooth,
      # "CROS_P_smooth" = CROS_P_smooth,
      # "CROS_A_smooth" = CROS_A_smooth,
      # "SROS_out"      = SROS_out,
      # "CROS_P_out"    = CROS_P_out,
      # "CROS_A_out"    = CROS_A_out,
      "ROS"           = round(ROS_AIO, 2),
      "CF_CI"         = CF_CI,
      "WS_CF_passive" = if (length(SROS_out) > 0 & length(CROS_P_out > 0)) {
        WS_seq[length(SROS_out) + 1]
      } else {
        NULL
      },
      "WS_CF_active"  = if (
        (length(SROS_out) > 0 | length(CROS_P_out)) > 0 & length(CROS_A_out) > 0
      ) {
        WS_seq[length(WS_seq) - length(CROS_A_out) + 1]
      } else {
        NULL
      }
    )
    out[[i]] <- results
    # Prepare plotting data
    ggdata_pCFO          <- fn_prep_ggdata(ID, WS_seq, pCFO)
    ggdata_SROS          <- fn_prep_ggdata(ID, WS_seq, SROS)
    ggdata_CROS_A        <- fn_prep_ggdata(ID, WS_seq, CROS_A)
    ggdata_CAC           <- fn_prep_ggdata(ID, WS_seq, CAC)
    ggdata_CROS_P        <- fn_prep_ggdata(ID, WS_seq, CROS_P)
    ggdata_SROS_smooth   <- fn_prep_ggdata(ID, WS_seq, SROS_smooth)
    ggdata_CROS_P_smooth <- fn_prep_ggdata(ID, WS_seq, CROS_P_smooth)
    ggdata_CROS_A_smooth <- fn_prep_ggdata(ID, WS_seq, CROS_A_smooth)
    ggdata_SROS_out      <- fn_prep_ggdata(
      ID,
      WS_seq[which(pCFO < 0.5)],
      SROS_out
    )
    ggdata_CROS_P_out    <- fn_prep_ggdata(
      ID,
      WS_seq[which(pCFO >= 0.5 & CAC < 1)],
      CROS_P_out
    )
    ggdata_CROS_A_out    <- fn_prep_ggdata(
      ID,
      WS_seq[which(pCFO >= 0.5 & CAC > 1)],
      CROS_A_out
    )
    ggdata_CF_CI         <- data.frame(
      ID  = rep(ID, times = 2),
      var = rep("CF_CI", times = 2),
      WS  = CF_CI,
      val = if (length(CROS_P_out) > 0) {
        rep(CROS_P_out[1], times = 2)
      } else if (length(CROS_A_out) > 0) {
        rep(CROS_A_out[1], times = 2)
      } else NULL
    )
    ggdata_ROS_AIO       <- fn_prep_ggdata(ID, WS_seq, ROS_AIO)
    ggdata_SROS_CROS_P   <- if (
      length(SROS_out) > 0 & length(CROS_P_out) > 0
    ) {
      data.frame(
        ID  = rep(ID, times = 2),
        var = rep("SROS_CROS_P", times = 2),
        WS  = WS_seq[c(length(SROS_out), length(SROS_out) + 1)],
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
        WS  = WS_seq[c(length(SROS_out), length(SROS_out) + 1)],
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
        WS  = WS_seq[c(
          length(WS_seq) - length(CROS_A_out),
          length(WS_seq) - length(CROS_A_out) + 1
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
        WS  = ggdata_SROS_CROS_P[2, 3],
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
        WS  = ggdata_SROS_CROS_A[2, 3],
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
        WS  = ggdata_CROS_P_CROS_A[2, 3],
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
  if ("ROS_full" %in% plot) {
    fig_ROS_full <- ggplot() +
      # SROS
      geom_line(
        data = subset(ggdata, var == "SROS_out"),
        mapping = aes(WS, val, color = ID),
        linewidth = 1.5
      ) +
      # CROSp
      geom_line(
        data = subset(ggdata, var == "CROS_P_out"),
        mapping = aes(WS, val, color = ID),
        linewidth = 1.5
      ) +
      # CROSa
      geom_line(
        data = subset(ggdata, var == "CROS_A_out"),
        mapping = aes(WS, val, color = ID),
        linewidth = 1.5
      ) +
      # Transition lines
      geom_line(
        data = subset(ggdata, var == "SROS_CROS_P"),
        mapping = aes(WS, val, color = ID),
        linewidth = 1.5,
        linetype = "dotted"
      ) +
      geom_line(
        data = subset(ggdata, var == "SROS_CROS_A"),
        mapping = aes(WS, val, color = ID),
        linewidth = 1.5,
        linetype = "dotted"
      ) +
      geom_line(
        data = subset(ggdata, var == "CROS_P_CROS_A"),
        mapping = aes(WS, val, color = ID),
        linewidth = 1.5,
        linetype = "dotted"
      ) +
      # CF confidence intervals
      geom_line(
        data = subset(ggdata, var == "CF_CI"),
        mapping = aes(WS, val, color = ID),
        linewidth = 1,
        alpha = 0.5
      ) +
      # Transition points
      # Crown fire point SROS CROS_P
      geom_point(
        data = subset(ggdata, var == "cf_pt_scp"),
        mapping = aes(WS, val, color = ID),
        shape = 16,
        size = 4
      ) +
      # Crown fire point SROS CROS_A
      geom_point(
        data = subset(ggdata, var == "cf_pt_sca"),
        mapping = aes(WS, val, color = ID),
        shape = 15,
        size = 4
      ) +
      # Crown fire point CROS_P CROS_A
      geom_point(
        data = subset(ggdata, var == "cf_pt_cpca"),
        mapping = aes(WS, val, color = ID),
        shape = 15,
        size = 4
      ) +
      # scale_x_continuous(breaks = seq(0, 60, 5), limits = c(0, 60)) +
      # scale_y_continuous(breaks = seq(0, 120, 10), limits = c(0, 110)) +
      labs(color = "Scenario") +
      xlab("Wind Speed [km/h]") +
      ylab("Equilibrium Rate of Spread [m/min]") +
      scale_color_viridis_d()
    print(fig_ROS_full)
  }
  # Output
  names(out) <- input$ID
  return(out)
}

# Internal functions ----
# __Fuel moisture content estimates ----

# Based on Eq. 2b in Van Wagner (1987). A more precise multiplier (e.g.,
# 147.2772277228) could be used to ensure a scale length closer to exactly 250,
# but the standard value of 147.2, as specified in NOR-X-424 (2015), is used
# here to ensure consistency with other implementations, giving a scale length
# of ~249.89
fn_MCFFMC   <- function(FFMC) {147.2 * (101 - FFMC) / (59.5 + FFMC)}
# Based on Eq. 16 in Van Wagner (1987)
fn_MCDMC    <- function(DMC) {20 + exp(-(DMC - 244.72) / 43.43)}
# Convert combinations of stand attributes to numeric codes
fn_MCSA_idx <- function(season, density, stand) {
  as.numeric(
    paste0(
      switch(
        season,
        "spring" = 1,
        "summer" = 2,
        "fall"   = 3,
        "sp-su"  = 4
      ),
      switch(
        density,
        "light"    = 1,
        "moderate" = 2,
        "dense"    = 3
      ),
      switch(
        stand,
        "deciduous"   = 1,
        "douglas-fir" = 2,
        "mixedwood"   = 3,
        "pine"        = 4,
        "spruce"      = 5
      )
    )
  )
}
# Calculate stand-adjusted moisture content
fn_MCSA <- function(idx, MCFFMC, MCDMC) {
  coefs <- sysdata$coefs_MCSA
  c     <- 0.002232
  calc_MCSA <- function(idx, MCFFMC, MCDMC) {
    a    <- coefs[which(coefs[1] == idx), 2]
    b    <- coefs[which(coefs[1] == idx), 3]
    MCSA <- exp(a + b * log(MCFFMC) + c * MCDMC)
  }
  if (idx < 400) {
    MCSA <- calc_MCSA(idx, MCFFMC, MCDMC)
  } else {
    idx_sp  <- as.numeric(paste0(1, substr(idx, 2, 3)))
    MCSA_sp <- calc_MCSA(idx_sp, MCFFMC, MCDMC)
    idx_su  <- as.numeric(paste0(2, substr(idx, 2, 3)))
    MCSA_su <- calc_MCSA(idx_su, MCFFMC, MCDMC)
    MCSA    <- mean(c(MCSA_sp, MCSA_su))
  }
  return(MCSA)
}

# __Probability of crown fire occurrence (pCFO) ----
fn_pCFO <- function(model, WS_seq, FSG, SFC, MC) {
  coefs <- sysdata$coefs_pCFO
  b0    <- coefs[which(coefs[1] == model), 2]
  b1    <- coefs[which(coefs[1] == model), 3]
  b2    <- coefs[which(coefs[1] == model), 4]
  b3    <- coefs[which(coefs[1] == model), 5]
  b4    <- coefs[which(coefs[1] == model), 6]
  gx    <- b0 + b1 * WS_seq + b2 * FSG^1.5 + b4 * log(SFC) + b3 * MC * WS_seq
  pCFO  <- exp(gx) / (1 + exp(gx))
  return(pCFO)
}

# __Confidence intervals
fn_CF_CI <- function(WS_seq, pCFO) {
  lower <- WS_seq[min(which(pCFO > (0.5 - (90 / 200))))]
  upper <- WS_seq[min(which(pCFO > (0.5 + (90 / 200))))]
  out   <- c(lower, upper)
  return(out)
}

# __Surface fire rate of spread (SROS) ----
fn_SROS <- function(model, WS_seq, MC, FFMC, SFC) {
  if (model == 1) {
    # Aggregated FBPS surf. V4 (default)
    f_w <- exp(0.05039 * WS_seq)
    f_f <- 91.9 * (exp(-0.1386 * MC) * (1 + (MC^5.31 / (4.93 * 10^7))))
    ISI_MC <- 0.208 * f_w * f_f
    SROS_under40 <- 25 * (1 - exp(-0.035177 * ISI_MC))^1.9875
    f_w <- 1 - exp(-0.0818 * (WS_seq - 28))
    ISI_MC <- 0.208 * 12 * f_w * f_f
    SROS_over40 <- 25 * (1 - exp(-0.035177 * ISI_MC))^1.9875
    SROS <- c(
      SROS_under40[which(WS_seq <= 40)],
      SROS_over40[which(WS_seq > 40)]
    )
    return(SROS)
  } else if (model == 2) {
    # D-1 FBPS
    cffdrs_in <- data.frame(
      id = seq(1, length(WS_seq), 1),
      fueltype = rep("D-1", times = length(WS_seq)),
      LAT = rep(55, times = length(WS_seq)),
      LONG = rep(-120, times = length(WS_seq)),
      FFMC = rep(FFMC, times = length(WS_seq)),
      BUI = rep(32, times = length(WS_seq)),
      WS = WS_seq,
      gs = rep(0, times = length(WS_seq)),
      Dj = rep(180, times = length(WS_seq)),
      aspect = rep(0, times = length(WS_seq))
    )
    cffdrs_out <- cffdrs::fbp(cffdrs_in,output = "Secondary")
    ISI <- cffdrs_out$ISI
    rsi_d1 <- 30 * (1 - exp(-0.0232 * ISI))^1.6
    SROS <- rsi_d1
    return(SROS)
  } else if (model == 3) {
    # C-6 (surface only) FBPS
    cffdrs_in <- data.frame(
      id = seq(1, length(WS_seq), 1),
      fueltype = rep("C-6", times = length(WS_seq)),
      LAT = rep(55, times = length(WS_seq)),
      LONG = rep(-120, times = length(WS_seq)),
      FFMC = rep(FFMC, times = length(WS_seq)),
      BUI = rep(62, times = length(WS_seq)),
      WS = WS_seq,
      gs = rep(0, times = length(WS_seq)),
      Dj = rep(180, times = length(WS_seq)),
      aspect = rep(0, times = length(WS_seq))
    )
    cffdrs_out <- cffdrs::fbp(cffdrs_in,output = "Secondary")
    ISI <- cffdrs_out$ISI
    rsi_c6 <- 30 * (1 - exp(-0.08 * ISI))^3
    SROS <- rsi_c6
    return(SROS)
  } else if (model == 4) {
    # IsaSFC
    b1 <- 0.015822
    b2 <- 0.344379
    f_w <- exp(0.05039 * WS_seq)
    f_f <- 91.9 * (exp(-0.1386 * MC) * (1 + (MC^5.31 / (4.93 * 10^7))))
    ISI_MC <- 0.208 * f_w * f_f
    SROS_under40 <- b1 * ISI_MC^2 + b2 * SFC
    f_w <- 1 - exp(-0.0818 * (WS_seq - 28))
    ISI_MC <- 0.208 * 12 * f_w * f_f
    SROS_over40 <- b1 * ISI_MC^2 + b2 * SFC
    SROS <- c(
      SROS_under40[which(WS_seq <= 40)],
      SROS_over40[which(WS_seq > 40)]
    )
    return(SROS)
  }
}

# __Crown fire rate of spread (CROS) ----
# Active crown fire rate of spread (CROS_A)
fn_CROS_A <- function(model, MC, WS_seq, CBD) {
  if (model == 1) {
    effm_mod <- 0.0079 + 3.6059 * log(MC)
    CROS_A <- 11.02 * (WS_seq^0.9) * CBD^0.19 * exp(-0.17 * effm_mod)
  } else if (model == 2) {
    CROS_A <- 0.084 * WS_seq * 1000 / 60
  }
  return(CROS_A)
}
# Criteria for active crowning (CAC)
fn_CAC <- function(CROS_A, CBD) {CROS_A / (3 / CBD)}
# Passive crown fire rate of spread (CROS_P)
fn_CROS_P <- function(CROS_A, CAC) {CROS_A * exp(-CAC)}

# __Smooth crown fire initiation ----
fn_SROS_smooth <- function(CROS_P, pCFO, CAC, SROS, CROS_A) {
  if (sum(CROS_P[which(pCFO >= 0.5 & CAC < 1)]) > 0) {
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
# Output SROS when p(CFO) < 0.5
fn_SROS_out <- function(pCFO, smooth_CFO, SROS_smooth, SROS) {
  ROS <- if (isTRUE(smooth_CFO)) {
    SROS_smooth[which(pCFO < 0.5)]
  } else {
    SROS[which(pCFO < 0.5)]
  }
  return(ROS)
}
# Output CROS_P when p(CFO) >= 0.5 and CAC < 1
fn_CROS_P_out <- function(pCFO, CAC, smooth_CFO, CROS_P_smooth, CROS_P) {
  ROS <- if (isTRUE(smooth_CFO)) {
    CROS_P_smooth[which(pCFO >= 0.5 & CAC < 1)]
  } else {
    CROS_P[which(pCFO >= 0.5 & CAC < 1)]
  }
  return(ROS)
}
# Output CROS_A if p(CFO) >= 0.5 and CAC > 1
fn_CROS_A_out <- function(
    pCFO,
    CAC,
    smooth_CFO,
    CROS_P,
    CROS_A_smooth,
    CROS_A
) {
  ROS <- if (
    isTRUE(smooth_CFO) & sum(CROS_P[which(pCFO >= 0.5 & CAC < 1)]) == 0
  ) {
    CROS_A_smooth[which(pCFO >= 0.5 & CAC > 1)]
  } else {
    CROS_A[which(pCFO >= 0.5 & CAC > 1)]
  }
  return(ROS)
}
