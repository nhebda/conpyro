#' Internal plotting helper functions
#'
#' @keywords internal
#'
#' @importFrom ggplot2 ggplot geom_line aes geom_point labs xlab ylab
#'   scale_color_viridis_d
#'
prep_ggdata <- function(ID, name, WS10, data) {
  data.frame(
    ID = rep(as.character(ID), length.out = length(WS10)),
    name = rep(deparse(substitute(name)), times = length(WS10)),
    WS10 = WS10,
    val = data
  )
}

fn_plot <- function(data, par, xlab, ylab) {
  ## check for `ggplot2` and warn user if not installed
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    gginput <- subset(data, var == par)
    p <- ggplot(
      gginput,
      aes(ws, val, color = ID)
    ) +
      geom_line(linewidth = 1.5) +
      labs(color = "Scenario") +
      xlab(xlab) +
      ylab(ylab) +
      scale_color_viridis_d()
  } else {
    stop("Plotting is active, but 'ggplot2' cannot be found.\nPlease install
         'ggplot2'")
  }
}
