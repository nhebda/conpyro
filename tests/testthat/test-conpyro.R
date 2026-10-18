make_conpyro_data <- function() {
  data.frame(
    FSG = c(6, 6, 6),
    SFC = c(2, 2, 2),
    CBD = c(0.1, 0.1, 0.2),
    season = c(2, 2, 2),
    density = c(2, 2, 2),
    stand = c("p", "p", "p")
  )
}

test_that("conpyro returns a named list of scenario outputs", {
  dat <- make_conpyro_data()

  out <- conpyro(
    data = dat,
    WS10 = 0:20,
    FFMC = c(90, 91, 92),
    DMC = 60,
    ID = "scenario"
  )

  expect_type(out, "list")
  expect_length(out, 3L)
  expect_named(out, c("scenario_1", "scenario_2", "scenario_3"))

  expected_names <- c(
    "mcFFMC (%)",
    "mcsa (%)",
    "WS10 (km/h)",
    "Crown Fire Occurrence Probability",
    "Passive Crown Fire WS10 Threshold (km/h)",
    "Active Crown Fire WS10 Threshold (km/h)",
    "Composite Rate of Spread (m/min)"
  )

  for (nm in names(out)) {
    expect_type(out[[nm]], "list")
    expect_named(out[[nm]], expected_names)

    expect_type(out[[nm]][["mcFFMC (%)"]], "double")
    expect_length(out[[nm]][["mcFFMC (%)"]], 1L)

    expect_type(out[[nm]][["WS10 (km/h)"]], "double")
    expect_equal(out[[nm]][["WS10 (km/h)"]], as.numeric(0:20))

    expect_type(
      out[[nm]][["Crown Fire Occurrence Probability"]],
      "double"
    )
    expect_length(
      out[[nm]][["Crown Fire Occurrence Probability"]],
      length(0:20)
    )

    expect_type(
      out[[nm]][["Composite Rate of Spread (m/min)"]],
      "double"
    )
  }
})

test_that("conpyro supports argument-only input with recycling", {
  out <- conpyro(
    data = NULL,
    WS10 = 0:20,
    FFMC = c(90, 91, 92),
    FSG = 6,
    SFC = 2,
    CBD = 0.1,
    DMC = 60,
    season = 2,
    density = 2,
    stand = "p",
    smooth_CFO = FALSE,
    ID = "arg"
  )

  expect_type(out, "list")
  expect_length(out, 3L)
  expect_named(out, c("arg_1", "arg_2", "arg_3"))
})

test_that("conpyro supports mixed data-frame and argument input", {
  dat <- make_conpyro_data()[, c("FSG", "SFC", "CBD")]

  out <- conpyro(
    data = dat,
    WS10 = 0:20,
    FFMC = 91,
    DMC = 60,
    season = 2,
    density = 2,
    stand = "p",
    ID = "mix"
  )

  expect_type(out, "list")
  expect_length(out, 3L)
  expect_named(out, c("mix_1", "mix_2", "mix_3"))
})

test_that("conpyro errors when the same input is supplied twice", {
  dat <- make_conpyro_data()

  expect_error(
    conpyro(
      data = dat,
      WS10 = 0:20,
      FFMC = 91,
      FSG = 6,
      SFC = 2,
      CBD = 0.1,
      DMC = 60
    ),
    regexp = "both|data frame|argument|supplied",
    fixed = FALSE
  )
})

test_that("conpyro falls back to mcF when mcsa inputs are incomplete", {
  out <- conpyro(
    data = NULL,
    WS10 = 0:20,
    FFMC = 91,
    FSG = 6,
    SFC = 2,
    CBD = 0.1,
    DMC = NA,
    season = 2,
    density = 2,
    stand = "p",
    ID = "mcf"
  )

  expect_true(is.na(out[["mcf"]][["mcsa (%)"]]))
  expect_false(is.na(out[["mcf"]][["mcFFMC (%)"]]))
})

test_that("conpyro uses mcsa path when all required inputs are present", {
  out <- conpyro(
    data = NULL,
    WS10 = 0:20,
    FFMC = 91,
    FSG = 6,
    SFC = 2,
    CBD = 0.1,
    DMC = 60,
    season = 2,
    density = 2,
    stand = "p",
    ID = "mcsa"
  )

  expect_false(is.na(out[["mcsa"]][["mcsa (%)"]]))
  expect_false(is.na(out[["mcsa"]][["mcFFMC (%)"]]))
})

test_that("conpyro default ID behavior is deterministic", {
  out <- conpyro(
    data = NULL,
    WS10 = 0:20,
    FFMC = c(90, 91),
    FSG = 6,
    SFC = 2,
    CBD = 0.1,
    DMC = 60,
    season = 2,
    density = 2,
    stand = "p",
    ID = NA
  )

  expect_named(out, c("1", "2"))
})

test_that("conpyro smooth_CFO changes ROS but not scenario names", {
  base_args <- list(
    data = NULL,
    WS10 = 0:20,
    FFMC = 91,
    FSG = 6,
    SFC = 2,
    CBD = 0.1,
    DMC = 60,
    season = 2,
    density = 2,
    stand = "p",
    ID = "smooth"
  )

  out_false <- do.call(
    conpyro,
    c(base_args, list(smooth_CFO = FALSE))
  )
  out_true <- do.call(
    conpyro,
    c(base_args, list(smooth_CFO = TRUE))
  )

  expect_named(out_false, "smooth")
  expect_named(out_true, "smooth")

  expect_false(
    isTRUE(
      all.equal(
        out_false[["smooth"]][["Composite Rate of Spread (m/min)"]],
        out_true[["smooth"]][["Composite Rate of Spread (m/min)"]]
      )
    )
  )
})

test_that("conpyro output is consistent with tool functions", {
  ws <- 11
  ffmc <- 91
  dmc <- 60
  fsg <- 6
  sfc <- 2
  cbd <- 0.2

  out <- conpyro(
    data = NULL,
    WS10 = ws,
    FFMC = ffmc,
    FSG = fsg,
    SFC = sfc,
    CBD = cbd,
    DMC = dmc,
    season = 2,
    density = 2,
    stand = "p",
    ID = "tool"
  )

  mcsa_val <- t_mcsa(
    FFMC = ffmc,
    DMC = dmc,
    season = 2,
    density = 2,
    stand = "p"
  )

  ft_ros <- t_ROS(
    WS10 = ws,
    mcsa = mcsa_val,
    FSG = fsg,
    SFC = sfc,
    CBD = cbd
  )

  expect_equal(out[["tool"]][["mcsa (%)"]], round(mcsa_val, 1))
  expect_equal(
    out[["tool"]][["Composite Rate of Spread (m/min)"]],
    unname(ft_ros[["Composite Rate of Spread (m/min)"]])
  )
})

test_that("conpyro validates plot input case-insensitively", {
  skip_if_not_installed("ggplot2")

  expect_no_error(
    conpyro(
      data = NULL,
      WS10 = 0:20,
      FFMC = 91,
      FSG = 6,
      SFC = 2,
      CBD = 0.1,
      DMC = 60,
      season = 2,
      density = 2,
      stand = "p",
      ID = "plot",
      plot = c("pCFO", "ROS")
    )
  )
})

test_that("conpyro errors on invalid plot values", {
  expect_error(
    conpyro(
      data = NULL,
      WS10 = 0:20,
      FFMC = 91,
      FSG = 6,
      SFC = 2,
      CBD = 0.1,
      DMC = 60,
      season = 2,
      density = 2,
      stand = "p",
      ID = "plot",
      plot = "fire"
    ),
    regexp = "plot|subset|choice|Must be",
    fixed = FALSE
  )
})

test_that("conpyro enforces strict common length across scenario inputs", {
  expect_error(
    conpyro(
      data = NULL,
      WS10 = 0:20,
      FFMC = c(90, 91, 92),
      FSG = c(6, 7),
      SFC = 2,
      CBD = 0.1,
      DMC = 60,
      season = 2,
      density = 2,
      stand = "p",
      ID = "len"
    ),
    regexp = "length|common length|Inputs",
    fixed = FALSE
  )
})

test_that("conpyro enforces strict numeric types for numeric inputs", {
  expect_error(
    conpyro(
      data = NULL,
      WS10 = "0:20",
      FFMC = 91,
      FSG = 6,
      SFC = 2,
      CBD = 0.1,
      DMC = 60,
      season = 2,
      density = 2,
      stand = "p",
      ID = "type"
    ),
    regexp = "WS10|numeric|Must be",
    fixed = FALSE
  )

  expect_error(
    conpyro(
      data = NULL,
      WS10 = 0:20,
      FFMC = "91",
      FSG = 6,
      SFC = 2,
      CBD = 0.1,
      DMC = 60,
      season = 2,
      density = 2,
      stand = "p",
      ID = "type"
    ),
    regexp = "FFMC|numeric|Must be",
    fixed = FALSE
  )
})

test_that("conpyro recognizes Douglas-fir aliases for m18c", {
  # Incomplete moisture inputs isolate the sROS stand adjustment.
  args <- list(
    data = NULL,
    WS10 = c(0, 11, 20),
    FFMC = 91,
    FSG = 6,
    SFC = 2,
    CBD = 0.1,
    DMC = NA_real_,
    smooth_CFO = FALSE,
    model_sROS = "18c",
    ROS_output = "sROS",
    ID = "alias"
  )

  reference <- do.call(
    what = conpyro,
    args = c(args, list(stand = "df"))
  )

  for (stand_value in c("Douglas-fir", "douglas-fir", "DOUGLAS-FIR", "DF")) {
    actual <- do.call(
      what = conpyro,
      args = c(args, list(stand = stand_value))
    )

    expect_equal(actual, reference)
  }

  pine <- do.call(
    what = conpyro,
    args = c(args, list(stand = "p"))
  )

  ros_name <- "Surface Rate of Spread (m/min)"
  expect_true(
    all(reference[["alias"]][[ros_name]] > pine[["alias"]][[ros_name]])
  )
})

run_ordering_scenario <- function(
    ws,
    smooth = FALSE,
    output_mode = NULL
) {
  args <- list(
    data = NULL,
    WS10 = ws,
    FFMC = 91,
    FSG = 6,
    SFC = 2,
    CBD = 0.1,
    DMC = NA_real_,
    smooth_CFO = smooth,
    ID = "ordering"
  )

  if (!is.null(output_mode)) {
    args$output_mode <- output_mode
  }

  out <- do.call(what = conpyro, args = args)
  return(out[["ordering"]])
}

test_that("composite output preserves wind order and duplicates", {
  ws <- 0:60
  index_sets <- list(
    rev(seq_along(ws)),
    c(seq(1, length(ws), by = 2), seq(2, length(ws), by = 2), 12, 12)
  )
  ros_name <- "Composite Rate of Spread (m/min)"
  pcfo_name <- "Crown Fire Occurrence Probability"
  threshold_names <- c(
    "Passive Crown Fire WS10 Threshold (km/h)",
    "Active Crown Fire WS10 Threshold (km/h)"
  )

  for (smooth in c(FALSE, TRUE)) {
    reference <- run_ordering_scenario(ws = ws, smooth = smooth)
    reference_fd <- run_ordering_scenario(
      ws = ws,
      smooth = smooth,
      output_mode = "fueldash"
    )

    # Ensure this fixture exercises all three regimes.
    expect_setequal(
      as.character(unique(reference_fd$series$fire_type)),
      c("s", "p", "a")
    )

    for (index in index_sets) {
      actual <- run_ordering_scenario(
        ws = ws[index],
        smooth = smooth
      )
      actual_fd <- run_ordering_scenario(
        ws = ws[index],
        smooth = smooth,
        output_mode = "fueldash"
      )

      expect_equal(actual[["WS10 (km/h)"]], as.numeric(ws[index]))
      expect_equal(actual[[ros_name]], reference[[ros_name]][index])
      expect_equal(actual[[pcfo_name]], reference[[pcfo_name]][index])
      expect_equal(actual[threshold_names], reference[threshold_names])

      expected_series <- reference_fd$series[index, , drop = FALSE]
      rownames(expected_series) <- NULL
      expect_equal(actual_fd$series, expected_series)

      for (j in seq_along(threshold_names)) {
        regime <- c("p", "a")[[j]]
        selected <- actual_fd$series$fire_type == regime
        expected_threshold <- min(actual_fd$series$ws[selected])

        expect_equal(
          actual[[threshold_names[[j]]]],
          expected_threshold
        )
      }
    }
  }
})

test_that("unsmoothed composite ROS agrees with single-wind calls", {
  ws <- c(60, 0, 20, 10, 40, 10)
  ros_name <- "Composite Rate of Spread (m/min)"

  actual <- run_ordering_scenario(ws = ws)
  expected <- vapply(
    X = ws,
    FUN = function(wind) {
      single <- run_ordering_scenario(ws = wind)
      single[[ros_name]]
    },
    FUN.VALUE = numeric(1)
  )

  expect_equal(actual[[ros_name]], expected)
})

test_that("absent crowning regimes retain NA thresholds", {
  actual <- run_ordering_scenario(ws = 0)

  expect_true(is.na(
    actual[["Passive Crown Fire WS10 Threshold (km/h)"]]
  ))
  expect_true(is.na(
    actual[["Active Crown Fire WS10 Threshold (km/h)"]]
  ))
  expect_length(actual[["Composite Rate of Spread (m/min)"]], 1L)
})

test_that("conpyro applies the m18c bound to FuelDash output", {
  for (smooth in c(FALSE, TRUE)) {
    out <- conpyro(
      data = NULL,
      WS10 = 0,
      FFMC = 80,
      FSG = 6,
      SFC = 2,
      CBD = 0.1,
      DMC = NA_real_,
      stand = "d",
      smooth_CFO = smooth,
      model_sROS = "18c",
      output_mode = "fueldash",
      ID = "bounded"
    )

    expect_equal(out[["bounded"]]$series$ros, 0)
  }
})

test_that("passive component is available when its regime is absent", {
  # Model 2 gives active ROS = 1.4 * WS10.
  # With CBD = 0.1, the critical active ROS is 30 m/min.
  ws <- c(60, 0, 40, 60)
  active_ros <- 1.4 * ws
  expected <- round(active_ros * exp(-active_ros / 30), 1)
  ros_name <- "Passive Crowning Rate of Spread (m/min)"

  args <- list(
    data = NULL,
    WS10 = ws,
    FFMC = 91,
    FSG = 6,
    SFC = 2,
    CBD = 0.1,
    DMC = NA_real_,
    model_cROS = 2,
    ROS_output = "cROS_P",
    ID = "passive"
  )

  for (smooth in c(FALSE, TRUE)) {
    out <- do.call(
      what = conpyro,
      args = c(args, list(smooth_CFO = smooth))
    )

    scenario <- out[["passive"]]
    expect_length(scenario[[ros_name]], length(ws))
    expect_equal(scenario[[ros_name]], expected)
    expect_true(is.na(
      scenario[["Passive Crown Fire WS10 Threshold (km/h)"]]
    ))

    # Confirm that the fixture contains no passive-crown observations.
    fd <- do.call(
      what = conpyro,
      args = c(
        args,
        list(smooth_CFO = smooth, output_mode = "fueldash")
      )
    )

    expect_false(any(fd[["passive"]]$series$fire_type == "p"))

    # Compare the explicit component with the public scalar tool.
    tool_ros <- vapply(
      X = ws,
      FUN = function(wind) {
        result <- t_ROS(
          WS10 = wind,
          mcF = fd[["passive"]]$mcF,
          FSG = 6,
          SFC = 2,
          CBD = 0.1,
          smooth_CFO = smooth,
          model_cROS = 2,
          ROS_output = "cROS_P"
        )
        result[[ros_name]]
      },
      FUN.VALUE = numeric(1)
    )

    expect_equal(scenario[[ros_name]], tool_ros)
  }
})
