#' Calculate probability of crown fire occurrence (pCFO), crowning thresholds,
#' and rate of spread (ROS) for any number of scenarios
#'
#' `conpyro()` calculates pCFO, crowning thresholds, and ROS for any number of
#' fuel and fire weather configurations using the Canadian Conifer Pyrometrics
#' (ConPyro) model system. See Perrakis et al. (2023) for details.
#'
#' @param input A data frame of least `8` columns and `1` row. Each row defines
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
#'   consumption in kg/m^2. May be estimated using [tool_sfc_fbp()] or
#'   [tool_sfc_degroot()].
#'   * `CBD`: A numeric value between `0.01` and `0.8` (inclusive). Crown bulk
#'   density in kg/m^3.
#'   * `smooth_CFI`: Defines whether crown fire initiation is modeled as a
#'   smooth transition (`TRUE`) or an instantaneous occurrence (`FALSE`).
#'
#'   Additionally, there are several optional columns:
#'   * `ws_min`: A numeric value between `0` and `60` (inclusive). Minimum wind
#'   speed in km/h. Allows wind speed to be specified on a per-scenario basis.
#'   If this column *and* the `ws_max` column are present, they will override
#'   the `ws` argument.
#'   * `ws_max`: A numeric value between `0` and `60` (inclusive). Maximum wind
#'   speed in km/h. Allows wind speed to be specified on a per-scenario basis.
#'   If this column *and* the `ws_min` column are present, they will override
#'   the `ws` argument.
#'   * `FFMC`: A numeric value between 80 and 99 (inclusive). The Fine Fuel
#'   Moisture Code (FFMC) as per the Canadian Forest Fire Weather Index System.
#'   Allows FFMC to be specified on a per-scenario basis. If this column is
#'   present, it will override the `ffmc` argument.
#'   * `DMC`: A numeric value between 80 and 99 (inclusive). The Duff Moisture
#'   Code (DMC) as per the Canadian Forest Fire Weather Index (FWI) System.
#'   Allows DMC to be specified on a per-scenario basis. If this column is
#'   present, it will override the `dmc` argument.
#'   * `model_conpyro`: Choose one of `7`, `8`, `10`, or `11`. The ConPyro
#'   model form used for calculations (see Perrakis et al., 2023). Models `7`
#'   and `10` use FFMC-based fine fuel moisture content (mcFFMC) while models
#'   `8` and `11` use stand-adjusted moisture content (mcSA). Default is `11`.
#'   * `model_sros`: Choose one of `1`, `2`, `3`, or `4`. The surface fire ROS
#'   model used for calculations.
#'      * `1`: Aggregated FBPS surf. V4 (default)
#'      * `2`: D-1 FBPS
#'      * `3`: C-6 (surface only) FBPS
#'      * `4`: IsaSFC
#'   * `model_cros`: Choose one of `1` or `2`. The crown fire ROS model used
#'   for calculations.
#'      * `1`: Adapted Cruz, Alexander, & Wakimoto (2005): ws, mc, CBD (default)
#'      * `2`: Adapted Cruz & Alexander (2019): ws only
#' @param ws An integer vector of length `2`, with each element being between
#'   `0` and `60` (inclusive). The first value defines the minimum wind speed
#'   and the second defines the maximum, in km/h. Calculations are carried out
#'   on the sequence of integer values from the minimum to the maximum
#'   (inclusive).
#' @param ffmc A numeric value between 80 and 99 (inclusive). The Fine Fuel
#'   Moisture Code (FFMC) as per the Canadian Forest Fire Weather Index System.
#' @param dmc A numeric value between 5 and 200 (inclusive). The Duff Moisture
#'   Code (DMC) as per the Canadian Forest Fire Weather Index (FWI) System.
#' @param plot A character vector to control plotting of output. Choose any of
#'   the following:
#'   * `pcfo`: Crown fire occurrence probability.
#'   * `sros`: Surface fire rate of spread.
#'   * `cros_p`: Passive crown fire rate of spread.
#'   * `cros_a`: Active crown fire rate of spread.
#'   * `cac`: Criterion for active crowning.
#'   * `ros_full`: Complete composite rate of spread plot with crowning
#'   thresholds.
#' @md
#'
#' @returns A list of lists, with each sub-list containing outputs for a single
#'   scenario (input row). Optionally plots output.
#' @export
#'
#' @examples {
#' ## TODO: examples.
#'   ## library(conpyro.pkg) ## double check if need
#'
#'   ## data(input)
#'
#'   ## conpyro(input)
#' }
#'
#' @importFrom cffdrs fbp
#' @importFrom checkmate assert_data_frame assert_subset assert_true
#'   assert_choice assert_number assert_logical assert_integerish assert_numeric
#' @importFrom utils read.csv
conpyro <- function(
    input,
    ws   = c(0, 40),
    ffmc = 91,
    dmc  = 70,
    plot = c(
      # "pcfo",
      # "sros",
      # "cros_a",
      # "cac",
      # "cros_p",
      # "sros_smooth",
      # "cros_p_smooth",
      # "cros_a_smooth",
      # "ros_aio",
      # "ros_full"
    )
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
      "cbd",
      "smooth_cfi"
    ),
    colnames(input)
  )
  assert_true(
    length(input$id) == length(unique(input$id)),
    .var.name = "Input IDs must be unique"
  )
  lapply(
    input$season,
    assert_choice,
    choices = c("spring","sp-su", "summer", "fall"),
    .var.name = "season"
  )
  lapply(
    input$density,
    assert_choice,
    choices = c("light", "moderate", "dense"),
    .var.name = "density"
  )
  lapply(
    input$stand,
    assert_choice,
    choices = c("deciduous", "douglas-fir", "mixedwood", "pine", "spruce"),
    .var.name = "stand"
  )
  lapply(input$fsg, assert_number, lower = 0.5, upper = 20, .var.name = "FSG")
  lapply(input$sfc, assert_number, lower = 0.1, upper = 6, .var.name = "SFC")
  lapply(input$cbd, assert_number, lower = 0.01, upper = 0.8, .var.name = "CBD")
  lapply(input$smooth_cfi, assert_logical, .var.name = "smooth_cfi")
  if ("ws_min" %in% names(input)) {
    assert_integerish(input$ws_min, lower = 0, upper = 59, .var.name = "ws_min")
  }
  if ("ws_max" %in% names(input)) {
    assert_integerish(input$ws_max, lower = 1, upper = 60, .var.name = "ws_max")
  }
  if ("ffmc" %in% names(input)) {
    assert_numeric(input$ffmc, lower = 80, upper = 99, .var.name = "ffmc")
  }
  assert_integerish(
    ws,
    lower  = 0,
    upper  = 60,
    len    = 2,
    unique = TRUE,
    sorted = TRUE
  )
  assert_number(ffmc, lower = 80, upper = 99)
  assert_number(dmc, lower = 5, upper = 200)
  assert_subset(
    plot,
    c(
      "pcfo",
      "sros",
      "cros_a",
      "cac",
      "cros_p",
      "sros_smooth",
      "cros_p_smooth",
      "cros_a_smooth",
      "ros_aio",
      "ros_full"
    )
  )

  # Initialize data structures
  out       <- list()
  ggdata    <- data.frame()
  # Loop through input rows (scenarios)
  for (i in 1:nrow(input)) {
    # Assign inputs to vars
    id            <- input$id[i]
    season        <- input$season[i]
    density       <- input$density[i]
    stand         <- input$stand[i]
    fsg           <- input$fsg[i]
    sfc           <- input$sfc[i]
    cbd           <- input$cbd[i]
    smooth_cfi    <- input$smooth_cfi[i]
    ws_seq        <- if (
      "ws_min" %in% names(input) & "ws_max" %in% names(input)
    ) {
      seq(input$ws_min[i], input$ws_max[i], 1)
    } else {
      seq(ws[1], ws[2], 1)
    }
    ffmc          <- if ("ffmc" %in% names(input)) {
      input$ffmc[i]
    } else {
      ffmc
    }
    dmc           <- if ("dmc" %in% names(input)) {
      input$dmc[i]
    } else {
      dmc
    }
    model_conpyro <- if ("model_conpyro" %in% names(input)) {
      input$model_conpyro[i]
    } else {
      11
    }
    model_sros    <- if ("model_sros" %in% names(input)) {
      input$model_sros[i]
    } else {
      1
    }
    model_cros    <- if ("model_cros" %in% names(input)) {
      input$model_cros[i]
    } else {
      1
    }
    # Do calculations
    mcffmc        <- fn_mcffmc(ffmc)
    mcdmc         <- fn_mcdmc(dmc)
    idx           <- fn_mcsa_idx(season, density, stand)
    mcsa          <- fn_mcsa(idx, mcffmc, mcdmc)
    mc            <- switch(
      as.character(model_conpyro),
      "7"  = mcffmc,
      "8"  = mcsa,
      "10" = mcffmc,
      "11" = mcsa
    )
    pcfo          <- fn_pcfo(model_conpyro, ws_seq, fsg, sfc, mc)
    cf_ci         <- fn_cf_ci(ws_seq, pcfo)
    sros          <- fn_sros(model_sros, ws_seq, mc, ffmc, sfc)
    cros_a        <- fn_cros_a(model_cros, mc, ws_seq, cbd)
    cac           <- fn_cac(cros_a, cbd)
    cros_p        <- fn_cros_p(cros_a, cac)
    sros_smooth   <- fn_sros_smooth(cros_p, pcfo, cac, sros, cros_a)
    cros_p_smooth <- fn_cros_p_smooth(sros, pcfo, cros_p)
    cros_a_smooth <- fn_cros_a_smooth(sros, pcfo, cros_a)
    sros_out      <- fn_sros_out(pcfo, smooth_cfi, sros_smooth, sros)
    cros_p_out    <- fn_cros_p_out(
      pcfo,
      cac,
      smooth_cfi,
      cros_p_smooth,
      cros_p
    )
    cros_a_out    <- fn_cros_a_out(
      pcfo,
      cac,
      smooth_cfi,
      cros_p,
      cros_a_smooth,
      cros_a
    )
    ros_aio       <- c(sros_out, cros_p_out, cros_a_out)
    # Prepare output data
    results <- list(
      "mcffmc"        = round(mcffmc, 2),
      # "mcdmc"         = mcdmc,
      # "mcsa_idx"      = idx,
      "mcsa"          = round(mcsa, 2),
      "ws_seq"        = ws_seq,
      "pcfo"          = round(pcfo, 2),
      # "sros"          = sros,
      # "cros_a"        = cros_a,
      # "cac"           = cac,
      # "cros_p"        = cros_p,
      # "sros_smooth"   = sros_smooth,
      # "cros_p_smooth" = cros_p_smooth,
      # "cros_a_smooth" = cros_a_smooth,
      # "sros_out"      = sros_out,
      # "cros_p_out"    = cros_p_out,
      # "cros_a_out"    = cros_a_out,
      "ros"           = round(ros_aio, 2),
      "cf_ci"         = cf_ci,
      "cf_passive_ws" = if (length(sros_out) > 0 & length(cros_p_out > 0)) {
        ws_seq[length(sros_out) + 1]
      } else {
        NULL
      },
      "cf_active_ws"  = if (
        (length(sros_out) > 0 | length(cros_p_out)) > 0 & length(cros_a_out) > 0
      ) {
        ws_seq[length(ws_seq) - length(cros_a_out) + 1]
      } else {
        NULL
      }
    )
    out[[i]] <- results
    # Prepare plotting data
    ggdata_pcfo          <- fn_prep_ggdata(id, ws_seq, pcfo)
    ggdata_sros          <- fn_prep_ggdata(id, ws_seq, sros)
    ggdata_cros_a        <- fn_prep_ggdata(id, ws_seq, cros_a)
    ggdata_cac           <- fn_prep_ggdata(id, ws_seq, cac)
    ggdata_cros_p        <- fn_prep_ggdata(id, ws_seq, cros_p)
    ggdata_sros_smooth   <- fn_prep_ggdata(id, ws_seq, sros_smooth)
    ggdata_cros_p_smooth <- fn_prep_ggdata(id, ws_seq, cros_p_smooth)
    ggdata_cros_a_smooth <- fn_prep_ggdata(id, ws_seq, cros_a_smooth)
    ggdata_sros_out      <- fn_prep_ggdata(
      id,
      ws_seq[which(pcfo < 0.5)],
      sros_out
    )
    ggdata_cros_p_out    <- fn_prep_ggdata(
      id,
      ws_seq[which(pcfo >= 0.5 & cac < 1)],
      cros_p_out
    )
    ggdata_cros_a_out    <- fn_prep_ggdata(
      id,
      ws_seq[which(pcfo >= 0.5 & cac > 1)],
      cros_a_out
    )
    ggdata_cf_ci         <- data.frame(
      id  = rep(id, times = 2),
      var = rep("cf_ci", times = 2),
      ws  = cf_ci,
      val = if (length(cros_p_out) > 0) {
        rep(cros_p_out[1], times = 2)
      } else if (length(cros_a_out) > 0) {
        rep(cros_a_out[1], times = 2)
      } else NULL
    )
    ggdata_ros_aio       <- fn_prep_ggdata(id, ws_seq, ros_aio)
    ggdata_sros_cros_p   <- if (
      length(sros_out) > 0 & length(cros_p_out) > 0
    ) {
      data.frame(
        id  = rep(id, times = 2),
        var = rep("sros_cros_p", times = 2),
        ws  = ws_seq[c(length(sros_out), length(sros_out) + 1)],
        val = c(max(sros_out), min(cros_p_out))
      )
    } else {
      NULL
    }
    ggdata_sros_cros_a   <- if (
      length(sros_out) > 0 & length(cros_a_out) > 0 & length(cros_p_out) == 0
    ) {
      data.frame(
        id  = rep(id, times = 2),
        var = rep("sros_cros_a", times = 2),
        ws  = ws_seq[c(length(sros_out), length(sros_out) + 1)],
        val = c(max(sros_out), min(cros_a_out))
      )
    } else {
      NULL
    }
    ggdata_cros_p_cros_a <- if (
      length(cros_p_out) > 0 & length(cros_a_out) > 0
    ) {
      data.frame(
        id  = rep(id, times = 2),
        var = rep("cros_p_cros_a", times = 2),
        ws  = ws_seq[c(
          length(ws_seq) - length(cros_a_out),
          length(ws_seq) - length(cros_a_out) + 1
        )],
        val = c(max(cros_p_out), min(cros_a_out))
      )
    } else {
      NULL
    }
    ggdata_cf_pt_scp  <- if (
      length(sros_out) > 0 & length(cros_p_out) > 0
    ) {
      data.frame(
        id  = id,
        var = "cf_pt_scp",
        ws  = ggdata_sros_cros_p[2, 3],
        val = ggdata_sros_cros_p[2, 4]
      )
    } else {
      NULL
    }
    ggdata_cf_pt_sca  <- if (
      length(sros_out) > 0 & length(cros_a_out) > 0 & length(cros_p_out) == 0
    ) {
      data.frame(
        id  = id,
        var = "cf_pt_sca",
        ws  = ggdata_sros_cros_a[2, 3],
        val = ggdata_sros_cros_a[2, 4]
      )
    } else {
      NULL
    }
    ggdata_cf_pt_cpca <- if (
      length(cros_p_out) > 0 & length(cros_a_out) > 0
    ) {
      data.frame(
        id  = id,
        var = "cf_pt_cpca",
        ws  = ggdata_cros_p_cros_a[2, 3],
        val = ggdata_cros_p_cros_a[2, 4]
      )
    } else {
      NULL
    }
    ggdata <- rbind(
      ggdata,
      ggdata_pcfo,
      ggdata_sros,
      ggdata_cros_a,
      ggdata_cac,
      ggdata_cros_p,
      ggdata_sros_smooth,
      ggdata_cros_p_smooth,
      ggdata_cros_a_smooth,
      ggdata_sros_out,
      ggdata_cros_p_out,
      ggdata_cros_a_out,
      ggdata_cf_ci,
      ggdata_ros_aio,
      ggdata_sros_cros_p,
      ggdata_sros_cros_a,
      ggdata_cros_p_cros_a,
      ggdata_cf_pt_scp,
      ggdata_cf_pt_sca,
      ggdata_cf_pt_cpca
    )
  }
  # Plotting
  if ("pcfo" %in% plot) {
    fig_pcfo <- fn_plot(
      ggdata,
      "pcfo",
      "Wind Speed [km/h]",
      "Probability of Crown Fire Occurrence [0-1]"
    )
    print(fig_pcfo)
  }
  if ("sros" %in% plot) {
    fig_sros <- fn_plot(
      ggdata,
      "sros",
      "Wind Speed [km/h]",
      "Equilibrium Surface Fire Rate of Spread [m/min]"
    )
    print(fig_sros)
  }
  if ("cros_a" %in% plot) {
    fig_cros_a <- fn_plot(
      ggdata,
      "cros_a",
      "Wind Speed [km/h]",
      "Equilibrium Active Crown Fire Rate of Spread [m/min]"
    )
    print(fig_cros_a)
  }
  if ("cac" %in% plot) {
    fig_cac <- fn_plot(
      ggdata,
      "cac",
      "Wind Speed [km/h]",
      "Criterion for Active Crowning [0-1]"
    )
    print(fig_cac)
  }
  if ("cros_p" %in% plot) {
    fig_cros_p <- fn_plot(
      ggdata,
      "cros_p",
      "Wind Speed [km/h]",
      "Equilibrium Passive Crown Fire Rate of Spread [m/min]"
    )
    print(fig_cros_p)
  }
  if ("sros_smooth" %in% plot) {
    fig_sros_smooth <- fn_plot(
      ggdata,
      "sros_smooth",
      "Wind Speed [km/h]",
      "sros_smooth"
    )
    print(fig_sros_smooth)
  }
  if ("cros_p_smooth" %in% plot) {
    fig_cros_p_smooth <- fn_plot(
      ggdata,
      "cros_p_smooth",
      "Wind Speed [km/h]",
      "cros_p_smooth"
    )
    print(fig_cros_p_smooth)
  }
  if ("cros_a_smooth" %in% plot) {
    fig_cros_a_smooth <- fn_plot(
      ggdata,
      "cros_a_smooth",
      "Wind Speed [km/h]",
      "cros_a_smooth"
    )
    print(fig_cros_a_smooth)
  }
  if ("ros_aio" %in% plot) {
    fig_ros_aio <- fn_plot(
      ggdata,
      "ros_aio",
      "Wind Speed [km/h]",
      "Equilibrium Rate of Spread [m/min]"
    )
    print(fig_ros_aio)
  }
  # Main ROS plot
  if ("ros_full" %in% plot) {
    fig_ros_full <- ggplot() +
      # SROS
      geom_line(
        data = subset(ggdata, var == "sros_out"),
        mapping = aes(ws, val, color = id),
        linewidth = 1.5
      ) +
      # CROSp
      geom_line(
        data = subset(ggdata, var == "cros_p_out"),
        mapping = aes(ws, val, color = id),
        linewidth = 1.5
      ) +
      # CROSa
      geom_line(
        data = subset(ggdata, var == "cros_a_out"),
        mapping = aes(ws, val, color = id),
        linewidth = 1.5
      ) +
      # Transition lines
      geom_line(
        data = subset(ggdata, var == "sros_cros_p"),
        mapping = aes(ws, val, color = id),
        linewidth = 1.5,
        linetype = "dotted"
      ) +
      geom_line(
        data = subset(ggdata, var == "sros_cros_a"),
        mapping = aes(ws, val, color = id),
        linewidth = 1.5,
        linetype = "dotted"
      ) +
      geom_line(
        data = subset(ggdata, var == "cros_p_cros_a"),
        mapping = aes(ws, val, color = id),
        linewidth = 1.5,
        linetype = "dotted"
      ) +
      # CF confidence intervals
      geom_line(
        data = subset(ggdata, var == "cf_ci"),
        mapping = aes(ws, val, color = id),
        linewidth = 1,
        alpha = 0.5
      ) +
      # Transition points
      # Crown fire point SROS CROS_P
      geom_point(
        data = subset(ggdata, var == "cf_pt_scp"),
        mapping = aes(ws, val, color = id),
        shape = 16,
        size = 4
      ) +
      # Crown fire point SROS CROS_A
      geom_point(
        data = subset(ggdata, var == "cf_pt_sca"),
        mapping = aes(ws, val, color = id),
        shape = 15,
        size = 4
      ) +
      # Crown fire point CROS_P CROS_A
      geom_point(
        data = subset(ggdata, var == "cf_pt_cpca"),
        mapping = aes(ws, val, color = id),
        shape = 15,
        size = 4
      ) +
      # scale_x_continuous(breaks = seq(0, 60, 5), limits = c(0, 60)) +
      # scale_y_continuous(breaks = seq(0, 120, 10), limits = c(0, 110)) +
      labs(color = "Scenario") +
      xlab("Wind Speed [km/h]") +
      ylab("Equilibrium Rate of Spread [m/min]") +
      scale_color_viridis_d()
    print(fig_ros_full)
  }
  # Output
  names(out) <- input$id
  return(out)
}

# Internal functions ----
# __Fine fuel moisture content estimates (mcFFMC, mcSA) ----
fn_mcffmc   <- function(ffmc) {147.27723 * (101 - ffmc) / (59.5 + ffmc)}
fn_mcdmc    <- function(dmc) {20 + exp(-(dmc - 244.72) / 43.43)}
fn_mcsa_idx <- function(season, density, stand) {
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
fn_mcsa <- function(idx, mcffmc, mcdmc) {
  coefs <- read.csv("R/coefs_mcsa.csv")
  c     <- 0.002232
  calc_mcsa <- function(idx, mcffmc, mcdmc) {
    a     <- coefs[which(coefs[1] == idx), 2]
    b     <- coefs[which(coefs[1] == idx), 3]
    mcsa  <- exp(a + b * log(mcffmc) + c * mcdmc)
  }
  if (idx < 400) {
    mcsa <- calc_mcsa(idx, mcffmc, mcdmc)
  } else {
    idx_sp  <- as.numeric(paste0(1, substr(idx, 2, 3)))
    mcsa_sp <- calc_mcsa(idx_sp, mcffmc, mcdmc)
    idx_su  <- as.numeric(paste0(2, substr(idx, 2, 3)))
    mcsa_su <- calc_mcsa(idx_su, mcffmc, mcdmc)
    mcsa    <- mean(c(mcsa_sp, mcsa_su))
  }
}

# __Probability of crown fire occurrenc (pCFO) ----
fn_pcfo <- function(model, ws_seq, fsg, sfc, mc) {
  coefs <- read.csv("R/coefs_pcfo.csv")
  b0 <- coefs[which(coefs[1] == model), 2]
  b1 <- coefs[which(coefs[1] == model), 3]
  b2 <- coefs[which(coefs[1] == model), 4]
  b3 <- coefs[which(coefs[1] == model), 5]
  b4 <- coefs[which(coefs[1] == model), 6]
  gx     <- b0 + b1 * ws_seq + b2 * fsg^1.5 + b4 * log(sfc) + b3 * mc * ws_seq
  pcfo   <- exp(gx) / (1 + exp(gx))
  return(pcfo)
}

# __Confidence intervals
fn_cf_ci <- function(ws_seq, pcfo) {
  lower <- ws_seq[min(which(pcfo > (0.5 - (90 / 200))))]
  upper <- ws_seq[min(which(pcfo > (0.5 + (90 / 200))))]
  out   <- c(lower, upper)
}

# __Surface fire rate of spread (SROS) ----
fn_sros <- function(model, ws_seq, mc, ffmc, sfc) {
  if (model == 1) {
    # Aggregated FBPS surf. V4 (default)
    f_w <- exp(0.05039 * ws_seq)
    f_f <- 91.9 * (exp(-0.1386 * mc) * (1 + (mc^5.31 / (4.93 * 10^7))))
    isi_mc <- 0.208 * f_w * f_f
    sros_under40 <- 25 * (1 - exp(-0.035177 * isi_mc))^1.9875
    f_w <- 1 - exp(-0.0818 * (ws_seq - 28))
    isi_mc <- 0.208 * 12 * f_w * f_f
    sros_over40 <- 25 * (1 - exp(-0.035177 * isi_mc))^1.9875
    sros <- c(
      sros_under40[which(ws_seq <= 40)],
      sros_over40[which(ws_seq > 40)]
    )
    return(sros)
  } else if (model == 2) {
    # D-1 FBPS
    cffdrs_in <- data.frame(
      id = seq(1, length(ws_seq), 1),
      fueltype = rep("D-1", times = length(ws_seq)),
      lat = rep(55, times = length(ws_seq)),
      long = rep(-120, times = length(ws_seq)),
      ffmc = rep(ffmc, times = length(ws_seq)),
      bui = rep(32, times = length(ws_seq)),
      ws = ws_seq,
      gs = rep(0, times = length(ws_seq)),
      dj = rep(180, times = length(ws_seq)),
      aspect = rep(0, times = length(ws_seq))
    )
    cffdrs_out <- cffdrs::fbp(cffdrs_in,output = "Secondary")
    isi <- cffdrs_out$ISI
    rsi_d1 <- 30 * (1 - exp(-0.0232 * isi))^1.6
    sros <- rsi_d1
    return(sros)
  } else if (model == 3) {
    # C-6 (surface only) FBPS
    cffdrs_in <- data.frame(
      id = seq(1, length(ws_seq), 1),
      fueltype = rep("C-6", times = length(ws_seq)),
      lat = rep(55, times = length(ws_seq)),
      long = rep(-120, times = length(ws_seq)),
      ffmc = rep(ffmc, times = length(ws_seq)),
      bui = rep(62, times = length(ws_seq)),
      ws = ws_seq,
      gs = rep(0, times = length(ws_seq)),
      dj = rep(180, times = length(ws_seq)),
      aspect = rep(0, times = length(ws_seq))
    )
    cffdrs_out <- cffdrs::fbp(cffdrs_in,output = "Secondary")
    isi <- cffdrs_out$ISI
    rsi_c6 <- 30 * (1 - exp(-0.08 * isi))^3
    sros <- rsi_c6
    return(sros)
  } else if (model == 4) {
    # IsaSFC
    b1 <- 0.015822
    b2 <- 0.344379
    f_w <- exp(0.05039 * ws_seq)
    f_f <- 91.9 * (exp(-0.1386 * mc) * (1 + (mc^5.31 / (4.93 * 10^7))))
    isi_mc <- 0.208 * f_w * f_f
    sros_under40 <- b1 * isi_mc^2 + b2 * sfc
    f_w <- 1 - exp(-0.0818 * (ws_seq - 28))
    isi_mc <- 0.208 * 12 * f_w * f_f
    sros_over40 <- b1 * isi_mc^2 + b2 * sfc
    sros <- c(
      sros_under40[which(ws_seq <= 40)],
      sros_over40[which(ws_seq > 40)]
    )
    return(sros)
  }
}

# __Crown fire rate of spread (CROS) ----
# Active crown fire rate of spread (CROS_A)
fn_cros_a <- function(model, mc, ws, cbd) {
  if (model == 1) {
    effm_mod <- 0.0079 + 3.6059 * log(mc)
    cros_a <- 11.02 * (ws^0.9) * cbd^0.19 * exp(-0.17 * effm_mod)
  } else if (model == 2) {
    cros_a <- 0.084 * ws * 1000 / 60
  }
  return(cros_a)
}
# Criteria for active crowning (CAC)
fn_cac <- function(cros_a, cbd) {cros_a / (3 / cbd)}
# Passive crown fire rate of spread (CROS_P)
fn_cros_p <- function(cros_a, cac) {cros_a * exp(-cac)}

# __Smooth crown fire initiation ----
fn_sros_smooth <- function(cros_p, pcfo, cac, sros, cros_a) {
  if (sum(cros_p[which(pcfo >= 0.5 & cac < 1)]) > 0) {
    (sros * (1 - pcfo)) + (cros_p * pcfo)
  } else {
    (sros * (1 - pcfo)) + (cros_a * pcfo)
  }
}
fn_cros_p_smooth <- function(sros, pcfo, cros_p) {
  (sros * (1 - pcfo)) + (cros_p * pcfo)
}
fn_cros_a_smooth <- function(sros, pcfo, cros_a) {
  (sros * (1 - pcfo)) + (cros_a * pcfo)
}

# __Final outputs ----
# Output SROS when p(CFO) < 0.5
fn_sros_out <- function(pcfo, smooth_cfi, sros_smooth, sros) {
  ros = if (isTRUE(smooth_cfi)) {
    sros_smooth[which(pcfo < 0.5)]
  } else {
    sros[which(pcfo < 0.5)]
  }
}
# Output CROS_P when p(CFO) >= 0.5 and CAC < 1
fn_cros_p_out <- function(pcfo, cac, smooth_cfi, cros_p_smooth, cros_p) {
  ros = if (isTRUE(smooth_cfi)) {
    cros_p_smooth[which(pcfo >= 0.5 & cac < 1)]
  } else {
    cros_p[which(pcfo >= 0.5 & cac < 1)]
  }
}
# Output CROS_A if p(CFO) >= 0.5 and CAC > 1
fn_cros_a_out <- function(
    pcfo,
    cac,
    smooth_cfi,
    cros_p,
    cros_a_smooth,
    cros_a
) {
  ros = if (
    isTRUE(smooth_cfi) & sum(cros_p[which(pcfo >= 0.5 & cac < 1)]) == 0
  ) {
    cros_a_smooth[which(pcfo >= 0.5 & cac > 1)]
  } else {
    cros_a[which(pcfo >= 0.5 & cac > 1)]
  }
}
