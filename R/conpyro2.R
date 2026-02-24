#' @keywords internal
conpyro2 <- function(
    data,
    WS10,
    FFMC,
    DMC,
    FSG,
    SFC,
    CBD,
    smooth_CFO,
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

  # Resolve optional inputs
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
  smooth_CFO <- resolve_input(
    args_list = args_list,
    data = data,
    name = "smooth_CFO",
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
    CF_thresh = CF_thresh,
    model_mcsa = model_mcsa,
    model_pCFO = model_pCFO,
    model_sROS = model_sROS,
    model_cROS = model_cROS
  )

  # Load coefficients
  coefs_mcsa <- sysdata$coefs_MCSA
  coefs_pCFO <- sysdata$coefs_pCFO

  # Initialize output list
  out <- list()

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
    CF_thresh_val <- if (is.na(CF_thresh[[i]])) 0.5 else CF_thresh[[i]]
    smooth_CFO_val <- if (is.na(smooth_CFO[[i]])) FALSE else smooth_CFO[[i]]
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
    if (isTRUE(smooth_CFO)) {
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
    out[[i]] <- list(
      mcFFMC = round(mcFFMC_val, 1),
      mcsa = round(mcsa_val, 1),
      WS10 = WS10,
      pCFO = round(pCFO_val, 2),
      surface_fire = any(surface_fire),
      passive_crowning = any(passive_crowning),
      active_crowning = any(active_crowning),
      iROS = round(iROS_out, 1)
    )
  }

  # Prepare output
  names(out) <- 1:n

  return(out)
}
