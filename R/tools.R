#' pCFO calculator
#'
#' @param model
#' @param mc
#' @param ws
#' @param fsg
#' @param sfc
#'
#' @returns
#' @export
#'
#' @examples {
#' ## TODO
#' }
tool_pcfo <- function(model = 11, mc = 7.8, ws = 20, fsg = 9.5, sfc = 1.8) {
  assert_choice(model, c(7, 8, 10, 11))
  assert_number(mc, lower = 5, upper = 20)
  assert_number(ws, lower = 0, upper = 60)
  assert_number(fsg, lower = 0.5, upper = 20)
  assert_number(sfc, lower = 0.1, upper = 6)
  pcfo <- fn_pcfo(model, ws, fsg, sfc, mc)
  out  <- list("pcfo" = pcfo)
  out
}
