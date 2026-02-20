#' @keywords internal
conpyro2 <- function(
    data,
    WS10,
    FFMC,
    DMC,
    FSG,
    SFC,
    CBD,
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
  DMC <- resolve_input(
    args_list = args_list,
    data = data,
    name = "DMC"
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

  # Validate inputs
  validate_input(
    WS10 = WS10,
    FFMC = FFMC,
    DMC = DMC,
    FSG = FSG,
    SFC = SFC,
    CBD = CBD,
    season = season,
    density = density,
    stand = stand,
    CF_thresh = CF_thresh,
    model_mcsa = model_mcsa,
    model_pCFO = model_pCFO,
    model_sROS = model_sROS,
    model_cROS = model_cROS
  )

  out <- list(
    WS10 = WS10,
    FFMC = FFMC,
    DMC = DMC,
    FSG = FSG,
    SFC = SFC,
    CBD = CBD,
    season = season,
    density = density,
    stand = stand,
    CF_thresh = CF_thresh,
    model_mcsa = model_mcsa,
    model_pCFO = model_pCFO,
    model_sROS = model_sROS,
    model_cROS = model_cROS
  )
  return(out)
}
