make_fueldash_args <- function() {
  list(
    data = NULL,
    WS10 = c(60, 0, 11, 30, 11),
    FFMC = c(91.37, 91.37),
    FSG = 6,
    SFC = 2,
    CBD = 0.1,
    DMC = c(60, NA_real_),
    season = "summer",
    density = "moderate",
    stand = "p",
    smooth_CFO = FALSE,
    ID = c("mcsa", "mcf")
  )
}

test_that("output_mode defaults to ordinary output", {
  args <- make_fueldash_args()

  omitted <- do.call(what = conpyro, args = args)
  explicit <- do.call(
    what = conpyro,
    args = c(args, list(output_mode = "default"))
  )

  expect_equal(explicit, omitted)
  expect_true(
    "Composite Rate of Spread (m/min)" %in% names(omitted[["mcsa"]])
  )
})

test_that("output_mode rejects invalid values clearly", {
  args <- make_fueldash_args()
  invalid_modes <- list(
    NULL,
    NA_character_,
    NA,
    character(0),
    c("default", "fueldash"),
    "",
    "unknown",
    "FuelDash",
    1,
    TRUE,
    list("fueldash"),
    factor("fueldash")
  )

  for (mode in invalid_modes) {
    expect_error(
      do.call(
        what = conpyro,
        args = c(args, list(output_mode = mode))
      ),
      regexp = paste0(
        "`output_mode` must be either ",
        "\"default\" or \"fueldash\"."
      ),
      fixed = TRUE
    )
  }
})

test_that("FuelDash preserves shape, moisture precision, and wind order", {
  args <- make_fueldash_args()
  out <- do.call(
    what = conpyro,
    args = c(args, list(output_mode = "fueldash"))
  )

  expect_named(out, c("mcsa", "mcf"))

  expected_mcf <- 147.2 * (101 - 91.37) / (59.5 + 91.37)
  idx <- conpyro:::mcsa_idx(
    FFMC = 91.37,
    season = "summer",
    density = "moderate",
    stand = "p",
    model_mcsa = "corrected"
  )
  expected_mcsa <- conpyro:::mcsa(
    idx = idx,
    mcFFMC = expected_mcf,
    mcDMC = conpyro:::mcDMC(DMC = 60),
    coefs = conpyro:::sysdata$coefs_MCSA
  )

  for (id in names(out)) {
    scenario <- out[[id]]

    expect_named(scenario, c("mcF", "mcsa", "series"))
    expect_length(scenario$mcF, 1L)
    expect_length(scenario$mcsa, 1L)
    expect_equal(scenario$mcF, expected_mcf, tolerance = 1e-12)
    expect_s3_class(scenario$series, "data.frame")
    expect_named(scenario$series, c("ws", "pcfo", "fire_type", "ros"))
    expect_equal(nrow(scenario$series), length(args$WS10))
    expect_equal(scenario$series$ws, args$WS10)
    expect_type(scenario$series$pcfo, "double")
    expect_type(scenario$series$ros, "double")
    expect_s3_class(scenario$series$fire_type, "factor")
    expect_identical(
      levels(scenario$series$fire_type),
      c("s", "p", "a")
    )
  }

  expect_equal(out[["mcsa"]]$mcsa, expected_mcsa, tolerance = 1e-12)
  expect_true(is.na(out[["mcf"]]$mcsa))

  # Ensure the precision fixture would detect premature rounding.
  expect_gt(abs(expected_mcf - round(expected_mcf, 2)), 1e-6)
  expect_gt(abs(expected_mcsa - round(expected_mcsa, 2)), 1e-6)
})

test_that("FuelDash maps full-precision components to the correct rows", {
  args <- make_fueldash_args()
  out <- do.call(
    what = conpyro,
    args = c(args, list(output_mode = "fueldash"))
  )

  for (id in names(out)) {
    scenario <- out[[id]]
    use_mcsa <- id == "mcsa"
    mc <- if (use_mcsa) scenario$mcsa else scenario$mcF

    pcfo <- conpyro:::pCFO(
      WS10 = args$WS10,
      mc = mc,
      FSG = args$FSG,
      SFC = args$SFC,
      model_pCFO = if (use_mcsa) 11L else 10L,
      coefs = conpyro:::sysdata$coefs_pCFO
    )
    surface_ros <- conpyro:::sROS(
      WS10 = args$WS10,
      mc = mc,
      SFC = args$SFC,
      model_sROS = if (use_mcsa) 13L else 12L,
      stand = args$stand
    )
    active_ros <- conpyro:::cROS_A(
      WS10 = args$WS10,
      mc = mc,
      CBD = args$CBD,
      model_cROS = 1L
    )
    cac <- active_ros / (3 / args$CBD)
    passive_ros <- active_ros * exp(-cac)

    expected_type <- ifelse(
      pcfo < 0.5,
      "s",
      ifelse(cac < 1, "p", "a")
    )
    expected_ros <- ifelse(
      expected_type == "s",
      surface_ros,
      ifelse(expected_type == "p", passive_ros, active_ros)
    )

    expect_equal(scenario$series$pcfo, pcfo, tolerance = 1e-12)
    expect_equal(scenario$series$ros, expected_ros, tolerance = 1e-12)
    expect_identical(
      as.character(scenario$series$fire_type),
      expected_type
    )

    # At least one value must distinguish raw from ordinary precision.
    expect_true(any(abs(pcfo - round(pcfo, 2)) > 1e-6))
    expect_true(any(abs(expected_ros - round(expected_ros, 1)) > 1e-6))
  }
})

test_that("FuelDash returns composite ROS regardless of ROS_output", {
  args <- make_fueldash_args()

  for (smooth in c(FALSE, TRUE)) {
    args$smooth_CFO <- smooth

    reference <- do.call(
      what = conpyro,
      args = c(
        args,
        list(output_mode = "fueldash", ROS_output = "composite")
      )
    )

    for (component in c("sROS", "cROS_P", "cROS_A")) {
      actual <- do.call(
        what = conpyro,
        args = c(
          args,
          list(output_mode = "fueldash", ROS_output = component)
        )
      )

      expect_equal(actual, reference)
    }

    ordinary <- do.call(what = conpyro, args = args)

    for (id in names(reference)) {
      expect_equal(
        round(reference[[id]]$series$ros, 1),
        ordinary[[id]][["Composite Rate of Spread (m/min)"]]
      )
      expect_equal(
        round(reference[[id]]$series$pcfo, 2),
        ordinary[[id]][["Crown Fire Occurrence Probability"]]
      )
    }
  }
})
