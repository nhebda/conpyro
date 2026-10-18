#' Example input data for `conpyro()`
#'
#' A dataset containing example stand and fuel inputs. Supply wind speed,
#' FFMC, and any additional inputs separately when calling [conpyro()].
#'
#' @format A `data.frame` with columns:
#' \describe{
#'   \item{ID}{Scenario identifier.}
#'   \item{Season}{Season category for stand-adjusted fuel moisture.}
#'   \item{Density}{Canopy density category for stand-adjusted fuel moisture.}
#'   \item{Stand}{Stand type for moisture and applicable surface ROS models.}
#'   \item{FSG}{Fuel strata gap in metres.}
#'   \item{SFC}{Surface fuel consumption in kg/m^2.}
#'   \item{CBD}{Crown bulk density in kg/m^3.}
#' }
#'
#' @seealso [conpyro()], [t_mcSeason()], [t_mcDensity()]
"default_input"
