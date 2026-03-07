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
    unname(ft_ros[["Rate of spread (m/min)"]])
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
