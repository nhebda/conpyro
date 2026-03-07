#' Centralized input normalization function
#'
#' @keywords internal
#'
normalize_input <- function(x, name) {
  normalizers <- list(
    "WS10" = function(x) as.numeric(x),
    "FFMC" = function(x) as.numeric(x),
    "FSG" = function(x) as.numeric(x),
    "SFC" = function(x) as.numeric(x),
    "CBD" = function(x) as.numeric(x),
    "DMC" = function(x) as.numeric(x),
    "season" = function(x) tolower(as.character(x)),
    "density" = function(x) tolower(as.character(x)),
    "stand" = function(x) tolower(as.character(x)),
    "smooth_CFO" = function(x) as.logical(x),
    "plot" = function(x) tolower(as.character(x)),
    "CF_thresh" = function(x) as.numeric(x),
    "model_mcsa" = function(x) tolower(as.character(x)),
    "model_pCFO" = function(x) as.integer(x),
    "model_sROS" = function(x) as.integer(x),
    "model_cROS" = function(x) as.integer(x),
    "month" = function(x) as.integer(x),
    "day" = function(x) as.integer(x),
    "canopy_closure" = function(x) as.numeric(x),
    "mc" = function(x) as.numeric(x),
    "LAT" = function(x) as.numeric(x),
    "LONG" = function(x) as.numeric(x),
    "ELV" = function(x) as.numeric(x),
    "Dj" = function(x) as.integer(x),
    "BUI" = function(x) as.numeric(x),
    "PC" = function(x) as.numeric(x),
    "FFL" = function(x) as.numeric(x),
    "FWFL" = function(x) as.numeric(x),
    "cons" = function(x) as.numeric(x),
    "cl" = function(x) as.numeric(x),
    "hs" = function(x) as.numeric(x),
    "zs" = function(x) as.numeric(x),
    "zp" = function(x) as.numeric(x),
    "lnfl" = function(x) as.numeric(x),
    "sapling_FMC" = function(x) as.numeric(x),
    "actual_SFC" = function(x) as.numeric(x)
  )

  fn <- normalizers[[name]]
  if (is.null(fn)) stop("No normalizer for ", name, call. = FALSE)

  return(fn(x))
}
