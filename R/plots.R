#' @importFrom ggplot2 ggplot geom_line aes geom_point labs xlab ylab
#'   scale_color_viridis_d
fn_prep_ggdata <- function(id, WS_seq, var) {
  data.frame(
    id  = rep(as.character(id), times = length(WS_seq)),
    var = rep(deparse(substitute(var)), times = length(WS_seq)),
    WS  = WS_seq,
    val = var
  )
}

fn_plot <- function(data, par, xlab, ylab) {
  ## check for `ggplot2` and warn user if not installed
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    gginput <- subset(data, var == par)
    p <- ggplot(
      gginput,
      aes(WS, val, color = id)
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
