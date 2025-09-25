## Internal plotting-related functions
## TODO: documenting these functions is optional, but can be a good idea to
## make future development and transferring to other developers easier

#' Prep ggplot2 input data
#'
#' @param id TODO: description.
#' @param ws_seq TODO: description.
#' @param var TODO: description.
#'
#' @returns
fn_prep_ggdata <- function(id, ws_seq, var) {
  data.frame(
    id  = rep(as.character(id), times = length(ws_seq)),
    var = rep(deparse(substitute(var)), times = length(ws_seq)),
    ws  = ws_seq,
    val = var
  )
}

#' Plot data using ggplot2
#'
#' @param data TODO: description.
#' @param par TODO: description.
#' @param xlab TODO: description.
#' @param ylab TODO: description.
#'
#' @returns
#' @importFrom ggplot2 ggplot geom_line geom_point labs xlab ylab scale_color_viridis_d aes
fn_plot <- function(data, par, xlab, ylab) {
  ## check for `ggplot2` and warn user if not installed
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    gginput <- subset(data, var == par)
    p <- ggplot(
      gginput,
      aes(ws, val, color = id)
    ) +
      geom_line(linewidth = 1.5) +
      labs(color = "Scenario") +
      xlab(xlab) +
      ylab(ylab) +
      scale_color_viridis_d()
  } else {
    stop("Plotting is active, but 'ggplot2' cannot be found.\nPlease install 'ggplot2'")
  }
}
