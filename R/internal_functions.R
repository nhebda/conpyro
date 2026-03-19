# Helper functions ----

#' Collect a named list of arguments from a parent function, including dots
#'
#' @keywords internal
#' @importFrom checkmate assert_choice assert_data_frame
#'
introspect_args <- function(
    .env = parent.frame(),
    .fn = sys.function(sys.parent())
) {
  # Evaluate named dots
  dots <- evalq(list(...), envir = .env)
  dots <- dots[nzchar(names(dots))]

  # Start with dots
  out <- dots

  # Override / add formals
  formal_names <- setdiff(names(formals(.fn)), "...")
  for (nm in formal_names) {
    val <- tryCatch(
      get(nm, envir = .env, inherits = FALSE),
      error = function(e) NULL
    )
    if (!is.null(val)) out[[nm]] <- val
  }

  return(out)
}

#' Decide where input values come from and which take precedence
#'
#' @keywords internal
#'
resolve_input <- function(
    args_list,
    data = NULL,
    name,
    required = TRUE
) {
  has_arg <- name %in% names(args_list) && !is.null(args_list[[name]])
  has_col <- !is.null(data) && (tolower(name) %in% names(data))
  if (has_arg && has_col) {
    stop(
      paste0("`", name, "`", " was supplied both as an argument and in ",
        "`", match.call()[["data"]], "`",
        ". Please supply it in only one place."
      ),
      call. = FALSE
    )
  }
  if (has_col) return(data[[tolower(name)]])
  if (has_arg) return(args_list[[name]])
  if (isTRUE(required)) {
    stop(
      paste0(
        "Missing required input: please supply ", "`", name, "`",
        " as an argument or as a column in `data`."
      ),
      call. = FALSE
    )
  }
  return(NA)
}

#' Determine common input length with strict recycling rules
#'
#' All inputs must have length 1 or the common length `n`. Otherwise, an error
#' is thrown.
#'
#' @param ... One or more input objects.
#'
#' @returns Integer scalar giving the common length.
#' @keywords internal
#'
check_common_length <- function(...) {
  inputs <- list(...)
  lengths <- vapply(inputs, length, integer(1))
  n <- max(lengths)
  bad <- lengths != 1L & lengths != n
  if (any(bad)) {
    stop(
      "Inputs must have length 1 or length ", n, ". Got: ",
      paste(names(lengths), "=", lengths, collapse = ", "),
      call. = FALSE
    )
  }
  return(n)
}

#' Enforce strict input length
#'
#' All inputs must have length `n`. Otherwise, an error is thrown.
#'
#' @param n A single integer; the length to enforce.
#' @param ... One or more input objects.
#'
#' @keywords internal
#'
check_length <- function(n, ...) {
  inputs <- list(...)
  lengths <- vapply(inputs, length, integer(1))
  bad <- lengths != as.integer(n)
  if (any(bad)) {
    stop(
      "Inputs must have length ", n, ". Got: ",
      paste(names(lengths), "=", lengths, collapse = ", "),
      call. = FALSE
    )
  }
}

#' Follows the FWI System ISI formulation (Van Wagner, 1987) with the addition
#' of the FBP System modification for WS > 40 (ST-X-3 Eq. 53a)
#'
#' @keywords internal
#'
ISI <- function(WS10, mc) {
  fw_low <- exp(0.05039 * WS10)
  fw_high <- 12 * (1 - exp(-0.0818 *(WS10 - 28)))
  ff <- 91.9 * exp(-0.1386 * mc) * (1 + (mc^5.31) / 4.93e+07)
  ISI_val <- numeric(length(WS10))
  low <- WS10 <= 40
  high <- !low
  ISI_val[low] <- 0.208 * fw_low[low] * ff
  ISI_val[high] <- 0.208 * fw_high[high] * ff

  return(ISI_val)
}

# Fuel moisture content ----

#' Follows Eq. 2b in Van Wagner (1987). A more precise multiplier (e.g.,
#' 147.2772277228) could be used to ensure a scale length closer to exactly 250,
#' but the standard value of 147.2, as specified in NOR-X-424 (2015), is used
#' here to ensure consistency with other implementations, giving a scale length
#' of ~249.89
#'
#' @keywords internal
#'
mcFFMC <- function(FFMC) {
  out <- 147.2 * (101 - FFMC) / (59.5 + FFMC)

  return(out)
}

#' Follows Eq. 16 in Van Wagner (1987)
#'
#' @keywords internal
#'
mcDMC <- function(DMC) {
  out <- 20 + exp(-(DMC - 244.72) / 43.43)

  return(out)
}

#' Convert combinations of stand attributes to numeric codes
#'
#' @keywords internal
#'
mcsa_idx <- function(
    FFMC,
    season,
    density,
    stand,
    model_mcsa
) {
  # Map codes
  season_map <- c(
    "spring" = 1, "1" = 1,
    "summer" = 2, "2" = 2,
    "fall" = 3, "3" = 3,
    "sp-su" = 4, "1.5" = 4
  )
  density_map <- c(
    "light" = 1, "1" = 1,
    "moderate" = 2, "2" = 2,
    "dense" = 3, "3" = 3
  )
  stand_map <- c(
    "deciduous" = 1, "d"  = 1,
    "douglas-fir" = 2, "df" = 2,
    "mixedwood" = 3, "m"  = 3,
    "pine" = 4, "p" = 4,
    "spruce" = 5, "s" = 5
  )

  # Assign codes
  season_code <- season_map[[season]]
  density_code <- density_map[[density]]
  stand_code <- stand_map[[stand]]

  # Density adjustment
  density_adj <- density_code
  if (model_mcsa == "corrected") {
    if (density_code == 1 && FFMC > 96.15) density_adj <- 2
    if (density_code == 3 && FFMC > 92.93) density_adj <- 2
  }

  # Encode
  out <- as.integer(
    paste0(season_code, density_adj, stand_code)
  )

  return(out)
}

#' Calculate stand-adjusted moisture content with sp-su seasonal averaging
#'
#' @keywords internal
#'
mcsa <- function(idx, mcFFMC, mcDMC, coefs) {
  season <- idx %/% 100
  density <- (idx %% 100) %/% 10
  stand <- idx %% 10

  assert_choice(season, c(1L, 2L, 3L, 4L))
  assert_choice(density, c(1L, 2L, 3L))
  assert_choice(stand, c(1L, 2L, 3L, 4L, 5L))

  c_dmc <- 0.002232
  if (season < 4L) {
    out <- idx_to_mcsa(idx, mcFFMC, mcDMC, coefs, c_dmc)
  } else {
    base <- idx %% 100
    out <- mean(
      c(
        idx_to_mcsa(100L + base, mcFFMC, mcDMC, coefs, c_dmc),
        idx_to_mcsa(200L + base, mcFFMC, mcDMC, coefs, c_dmc)
      )
    )
  }

  return(out)
}

#' Calculate stand-adjusted moisture content
#'
#' @keywords internal
#'
idx_to_mcsa <- function(idx, mcFFMC, mcDMC, coefs, c_dmc) {
  row <- match(idx, coefs[[1L]])
  if (is.na(row)) stop("Invalid mcsa index: ", idx, call. = FALSE)
  a <- coefs[row, 2L]
  b <- coefs[row, 3L]
  out <- exp(a + b * log(mcFFMC) + c_dmc * mcDMC)

  return(out)
}

# Crown fire probability ----

#' Probability of crown fire occurrence
#'
#' @keywords internal
#'
pCFO <- function(
    WS10,
    mc,
    FSG,
    SFC,
    model_pCFO,
    coefs
) {
  row <- match(model_pCFO, coefs[[1L]])
  if (is.na(row)) stop("Invalid pCFO model: ", model_pCFO, call. = FALSE)
  b0 <- coefs[row, 2L]
  b1 <- coefs[row, 3L]
  b2 <- coefs[row, 4L]
  b3 <- coefs[row, 5L]
  b4 <- coefs[row, 6L]

  gx <- b0 + b1 * WS10 + b2 * FSG^1.5 + b4 * log(SFC) + b3 * mc * WS10
  out <- exp(gx) / (1 + exp(gx))

  return(out)
}

#' Precise WS10 crowning threshold
#'
#' @keywords internal
#'
WS10_CF_thresh <- function(
    mc,
    FSG,
    SFC,
    CF_thresh,
    model_pCFO,
    coefs
) {
  row <- match(model_pCFO, coefs[[1L]])
  if (is.na(row)) stop("Invalid pCFO model: ", model_pCFO, call. = FALSE)
  b0 <- coefs[row, 2L]
  b1 <- coefs[row, 3L]
  b2 <- coefs[row, 4L]
  b3 <- coefs[row, 5L]
  b4 <- coefs[row, 6L]

  out <- (
    log(CF_thresh / (1 - CF_thresh)) - b0 - b2 * FSG^1.5 - b4 * log(SFC)
  ) / (b1 + b3 * mc)

  return(out)
}

# Rate of spread ----

#' Surface fire rate of spread
#'
#' @keywords internal
#'
sROS <- function(WS10, mc, SFC, model_sROS) {
  ISI <- ISI(WS10 = WS10, mc = mc)

  out <- switch(
    as.character(model_sROS),
    # FBPS aggregated surf. V4
    "1" = 25 * (1 - exp(-0.035177 * ISI))^1.9875, # ST-X-3 Eq. 26
    # FBPS D-1 (no BE)
    "2" = 30 * (1 - exp(-0.0232 * ISI))^1.6, # ST-X-3 Eq. 26 & Tbl. 6
    # FBPS C-6 (surface only, no BE)
    "3" = 30 * (1 - exp(-0.08 * ISI))^3, # ST-X-3 Eq. 62
    # ISI2SFC
    "4" = 0.015822 * ISI^2 + 0.344379 * SFC,
    # m12 sl.con.ISI (Perrakis et al., 2026): default for mcFFMC
    "12" = (0.15 * ISI + 13) * (1-exp(-0.13498 * ISI))^5.773107, # Tbls. 1 & A2
    # m13 sl.con.isim (Perrakis et al., 2026): default for mcsa
    "13" = (0.15 * ISI + 13) * (1-exp(-0.101379 * ISI))^4.164469 # Tbls. 1 & A2
  )

  return(out)
}

#' Active crown fire rate of spread
#'
#' @keywords internal
#'
cROS_A <- function(WS10, mc, CBD, model_cROS) {
  if (model_cROS == 1) {
    effm_mod <- -0.4812 + 3.8842 * log(mc) # Coefficients updated 2026/02/13
    out <- 11.02 * (WS10^0.9) * CBD^0.19 * exp(-0.17 * effm_mod)
  } else if (model_cROS == 2) {
    out <- 0.084 * WS10 * 1000 / 60
  } else if (model_cROS == 3) {
    out <- 0.1 * WS10 * 1000 / 60
  } else {
    stop("Invalid cROS model: ", model_cROS, call. = FALSE)
  }

  return(out)
}

#' Criteria for active crowning
#'
#' @keywords internal
#'
CAC <- function(cROS_A, CBD) {
  out <- cROS_A / (3 / CBD)

  return(out)
}

#' Passive crown fire rate of spread
#'
#' @keywords internal
#'
cROS_P <- function(cROS_A, CAC) {
  out <- cROS_A * exp(-CAC)

  return(out)
}

# Smooth crown fire occurrence ----

#' Smooth transition from surface rate of spread to passive or active crowning
#'
#' @keywords internal
#'
ROS_smooth <- function(
    pCFO,
    passive_crowning,
    sROS,
    cROS_P,
    cROS_A
) {
  out <- if (any(passive_crowning)) {
    sROS * (1 - pCFO) + cROS_P * pCFO
  } else {
    sROS * (1 - pCFO) + cROS_A * pCFO
  }

  return(out)
}
