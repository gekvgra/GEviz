#' Adjust ticks of second y-axis in plotly
#'
#' @param y1_data,y2_data Vectors or dataframe columns with the data.
#' @export
#' @return A \code{list} object with y1 & y2 range min & max and the dticks.
#' @author Yannick Schwarz
#' @seealso \href{https://github.com/VictorBezak/Plotly_Multi-Axes_Gridlines}{link}
#' @examples \dontrun{
#' ax <- adjust_plotly_axis(mtcars[1], mtcars[2])
#' plot_ly() %>%
#' plotly_gelayout(hovermode = "closest") %>%
#'   layout(
#'      yaxis = list(
#'         overlaying = "y2",
#'         range = c(ax$y1_range_min, ax$y1_range_max),
#'         dtick = ax$y1_dtick / n_ticks
#'         ),
#'      yaxis2 = list(
#'         side = "right",
#'         range = c(ax$y2_range_min, ax$y2_range_max),
#'         dtick = ax$y2_dtick / n_ticks
#'         )
#'      )
#'}
#'
adjust_plotly_axis <- function(y1_data,
                               y2_data) {
  # y1
  y1_min <- min(y1_data, na.rm = TRUE)
  y1_max <- max(y1_data, na.rm = TRUE)

  if (y1_min < 0) {
    y1_range <- y1_max - y1_min
  } else {
    y1_range <- y1_max
  }

  y1_dtick <- as.numeric(substr(as.character(y1_range), 1, 1)) * 10^(nchar(as.character(as.integer(y1_range))) - 1)

  # y2
  y2_min <- min(y2_data, na.rm = TRUE)
  y2_max <- max(y2_data, na.rm = TRUE)

  if (y2_min < 0) {
    y2_range <- y2_max - y2_min
  } else {
    y2_range <- y2_max
  }

  y2_dtick <- as.numeric(substr(as.character(y2_range), 1, 1)) * 10^(nchar(as.character(as.integer(y2_range))) - 1)

  # dtick ratio
  y1_dtick_ratio <- y1_range / y1_dtick
  y2_dtick_ratio <- y2_range / y2_dtick

  global_dtick_ratio <- max(y1_dtick_ratio, y2_dtick_ratio)

  # Range minimums
  negative <- FALSE

  if (y1_min < 0) {
    negative <- TRUE
    y1_negative_ratio <- abs(y1_min / y1_range) * global_dtick_ratio
  } else {
    y1_negative_ratio <- 0
  }

  if (y2_min < 0) {
    negative <- TRUE
    y2_negative_ratio <- abs(y2_min / y2_range) * global_dtick_ratio
  } else {
    y2_negative_ratio <- 0
  }

  # Increase the ratio by 0.1 so that your range minimums are extended just far enough to not cut off any part of your lowest value
  global_negative_ratio <- max(y1_negative_ratio, y2_negative_ratio) + 0.1

  if (negative) {
    y1_range_min <- (global_negative_ratio) * y1_dtick * -1
    y2_range_min <- (global_negative_ratio) * y2_dtick * -1
  } else {
    y1_range_min <- 0
    y2_range_min <- 0
  }

  # Range maximums
  y1_positive_ratio <- abs(y1_max / y1_range) * global_dtick_ratio
  y2_positive_ratio <- abs(y2_max / y2_range) * global_dtick_ratio

  global_positive_ratio <- max(y1_positive_ratio, y2_positive_ratio) + 0.1

  y1_range_max <- global_positive_ratio * y1_dtick
  y2_range_max <- global_positive_ratio * y2_dtick

  out_list <- list(
    "y1_range_min" = y1_range_min,
    "y1_range_max" = y1_range_max,
    "y1_dtick" = y1_dtick,
    "y2_range_min" = y2_range_min,
    "y2_range_max" = y2_range_max,
    "y2_dtick" = y2_dtick
  )

  return(out_list)
}
