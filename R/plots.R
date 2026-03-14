# Internal plotting helper functions

#' Format data for ggplot2
#'
#' @keywords internal
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


#' Plot data using ggplot2
#'
#' @keywords internal
#'
plot_ggdata <- function(data, var, xlab, ylab) {
  # Check for `ggplot2` and warn user if not installed
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    gginput <- subset(data, name == var)
    p <- ggplot2::ggplot(
      gginput,
      ggplot2::aes(WS10, val, color = ID)
    ) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::labs(color = "Scenario") +
      ggplot2::xlab(xlab) +
      ggplot2::ylab(ylab) +
      ggplot2::scale_color_viridis_d()

    return(p)
  } else {
    stop("Plotting is active, but 'ggplot2' cannot be found.\n
         Please install 'ggplot2'",
         call. = FALSE
    )
  }
}
