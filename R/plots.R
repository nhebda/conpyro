#' Internal plotting helper functions
#'
#' @keywords internal
#'
#' @importFrom ggplot2 ggplot geom_line aes geom_point labs xlab ylab
#'   scale_color_viridis_d
#'
prep_ggdata <- function(ID, name, WS10, data) {
  out <- data.frame(
    ID = rep(as.character(ID), length.out = length(WS10)),
    name = rep(name, times = length(WS10)),
    WS10 = WS10,
    val = data
  )

  return(out)
}

plot_ggdata <- function(data, var, xlab, ylab) {
  # Check for `ggplot2` and warn user if not installed
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    gginput <- subset(data, name == var)
    p <- ggplot(
      gginput,
      aes(WS10, val, color = ID)
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
