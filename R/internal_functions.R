# General ----

# Follows the FWI System ISI formulation (Van Wagner, 1987) with the addition of
# the FBP System modification for WS > 40 (ST-X-3 Eq. 53a)
#' @keywords internal
fn_ISI <- function(ws_seq, mc) {
  # Normalize
  ws_seq <- as.numeric(ws_seq)
  mc <- as.numeric(mc)
  # Validate
  fn_validate_input(ws_seq = ws_seq, mc = mc)
  # Calculate
  fw_low <- exp(0.05039 * ws_seq)
  fw_high <- 12 * (1 - exp(-0.0818 *(ws_seq - 28)))
  ff <- 91.9 * exp(-0.1386 * mc) * (1 + (mc^5.31) / 4.93e+07)
  ISI <- numeric(length(ws_seq))
  low <- ws_seq <= 40
  high <- !low
  ISI[low] <- 0.208 * fw_low[low] * ff
  ISI[high] <- 0.208 * fw_high[high] * ff
  return(ISI)
}

# Internal helper functions ----

# Decide where input values come from and which take precedence
#' @keywords internal
resolve_input <- function(arg = NULL, data = NULL, name, required = TRUE) {
  if (!is.null(data)) {
    assert_data_frame(data)
    names(data) <- tolower(names(data))
  }
  has_arg <- !is.null(arg)
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
  if (has_col) {
    out <- data[[tolower(name)]]
    test <- match.call()[["name"]]
    validate_input(test = out)
    return(out)
  }
  if (has_arg) {
    out <- arg
    validate_input("FFMC" = out)
    return(out)
  }
  if (isTRUE(required)) {
    stop(
      paste0(
        "Missing required input: please supply ", "`", name, "`",
        " as an argument or as a column in `data`."
      ),
      call. = FALSE
    )
  }
}

# Fuel moisture content estimates ----

# Follows Eq. 2b in Van Wagner (1987). A more precise multiplier (e.g.,
# 147.2772277228) could be used to ensure a scale length closer to exactly 250,
# but the standard value of 147.2, as specified in NOR-X-424 (2015), is used
# here to ensure consistency with other implementations, giving a scale length
# of ~249.89
#' @keywords internal
fn_mcFFMC <- function(FFMC) {
  FFMC <- as.numeric(FFMC)
  mcFFMC <- 147.2 * (101 - FFMC) / (59.5 + FFMC)
  return(mcFFMC)
}

# Follows Eq. 16 in Van Wagner (1987)
#' @keywords internal
fn_mcDMC <- function(DMC) {
  DMC <- as.numeric(DMC)
  mcDMC <- 20 + exp(-(DMC - 244.72) / 43.43)
  return(mcDMC)
}

# Convert combinations of stand attributes to numeric codes
#' @keywords internal
fn_mcsa_idx <- function(
    FFMC,
    season,
    density,
    stand,
    model_mcsa
) {
  # Normalize input
  FFMC <- as.numeric(FFMC)
  season <- tolower(as.character(season))
  density <- tolower(as.character(density))
  stand <- tolower(as.character(stand))
  model_mcsa <- tolower(model_mcsa)
  # Validate input
  fn_validate_input(
    FFMC = FFMC,
    season = season,
    density = density,
    stand = stand,
    model_mcsa = model_mcsa
  )
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
  idx <- as.numeric(
    paste0(season_code, density_adj, stand_code)
  )
  return(idx)
}

# Calculate stand-adjusted moisture content
#' @keywords internal
fn_idx2mcsa <- function(idx, mcFFMC, mcDMC, coefs, c_dmc) {
  row <- match(idx, coefs[[1L]])
  a <- coefs[row, 2L]
  b <- coefs[row, 3L]
  mcsa <- exp(a + b * log(mcFFMC) + c_dmc * mcDMC)
  return(mcsa)
}

# Calculate stand-adjusted moisture content
#' @keywords internal
fn_mcsa <- function(idx, mcFFMC, mcDMC) {
  # Normalize
  idx <- as.integer(idx)
  mcFFMC <- as.numeric(mcFFMC)
  mcDMC <- as.numeric(mcDMC)
  # Validate
  assert_int(idx, lower = 111, upper = 435)
  season <- idx %/% 100
  density <- (idx %% 100) %/% 10
  stand <- idx %% 10
  assert_choice(season, c(1L, 2L, 3L, 4L))
  assert_choice(density, c(1L, 2L, 3L))
  assert_choice(stand, c(1L, 2L, 3L, 4L, 5L))
  assert_number(mcFFMC, lower = 1.85, upper = 22.16, finite = TRUE)
  assert_number(mcDMC, lower = 20, upper = 270, finite = TRUE)
  # Calculate
  coefs <- sysdata$coefs_MCSA
  c_dmc <- 0.002232
  if (season < 4L) {
    mcsa <- fn_idx2mcsa(idx, mcFFMC, mcDMC, coefs, c_dmc)
  } else {
    base <- idx %% 100
    mcsa <- mean(
      c(
        fn_idx2mcsa(100L + base, mcFFMC, mcDMC, coefs, c_dmc),
        fn_idx2mcsa(200L + base, mcFFMC, mcDMC, coefs, c_dmc)
      )
    )
  }
  return(mcsa)
}

# Probability of crown fire occurrence (pCFO) ----
#' @keywords internal
fn_pCFO <- function(
    ws_seq,
    mc,
    FSG,
    SFC,
    mc_type,
    model_pCFO_mcF,
    model_pCFO_mcsa
) {
  # Normalize input
  ws_seq <- as.numeric(ws_seq)
  mc <- as.numeric(mc)
  FSG <- as.numeric(FSG)
  SFC <- as.numeric(SFC)
  mc_type <- as.character(mc_type)
  model_pCFO_mcF <- as.integer(model_pCFO_mcF)
  model_pCFO_mcsa <- as.integer(model_pCFO_mcsa)
  # # Validate input
  # fn_validate_input(
  #   model_pCFO = model_pCFO,
  #   ws_seq = ws_seq,
  #   FSG = FSG,
  #   SFC = SFC,
  #   mc = MC
  # )
  # Calculate
  coefs <- sysdata$coefs_pCFO
  row <- match(model_pCFO, coefs[[1L]])
  b0 <- coefs[row, 2L]
  b1 <- coefs[row, 3L]
  b2 <- coefs[row, 4L]
  b3 <- coefs[row, 5L]
  b4 <- coefs[row, 6L]
  gx <- b0 + b1 * ws_seq + b2 * FSG^1.5 + b4 * log(SFC) + b3 * mc * ws_seq
  pCFO <- exp(gx) / (1 + exp(gx))
  return(pCFO)
}

