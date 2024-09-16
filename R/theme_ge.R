#' Theme for ggplot2
#'
#' GE KVG branded theme for ggplot2
#'
#' @param panel.grid.major Character vector. "grid.x" for a vertical grid on the x-axis. "grid.y" for a horizontal grid on the y-axis. "none" for no grid at all.
#' @param angle.x Controll the angle of the labels on the x-axis.
#' @param title.x,title.y Title of the axis if needed.
#' @param text.size Text size of all text elements in the plot.
#' @param axis.text.y Logical if y-axis should have text labels.
#' @param sec.axis.y Logical if y-axis should have a second axis.
#' @param sec.axis.y.color Character vector with the color of the second y-axis. Default is lavender".
#' @param legend.position Position of the legend. Default is "bottom". "none" removes the legend.
#' @param family Character vector with the font which should be used. Default is "sans" which works fine with pdf and html output. For a clean \code{ggplotly} output use "arial".
#' @param ... Further arguments to pass on to \link[ggplot2]{theme}
#' @export
#' @author Magnus Vieten, Yannick Schwarz
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   year = c(2019, 2020, 2021),
#'   value = c(4.2, 10, 29.5)
#' )
#'
#' ggplot2::ggplot(data = df, ggplot2::aes(x = year, y = value)) +
#'   ggplot2::geom_bar(stat = "identity") +
#'   GEviz::theme_ge()
#' }
theme_ge <- function(panel.grid.major = "grid.y",
                     title.x = FALSE,
                     title.y = TRUE,
                     sec.axis.y = FALSE,
                     sec.axis.y.color = "lavender",
                     angle.x = 0,
                     text.size = 9,
                     axis.text.y = TRUE,
                     legend.position = "bottom",
                     family = "sans",
                     ...) {
  ggplot2::theme(
    text = ggplot2::element_text(
      size = text.size,
      family = family,
      color = "black"
    ),
    legend.position = legend.position,
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = text.size),
    axis.text.x = ggplot2::element_text(
      angle = angle.x, color = "black", size = text.size
    ),
    axis.text.y = if (axis.text.y) {
      ggplot2::element_text(color = "black", size = text.size)
    } else {
      ggplot2::element_blank()
    },
    axis.title.x = if (title.x) {
      ggplot2::element_text(margin = ggplot2::margin(t = 20), size = text.size)
    } else {
      ggplot2::element_blank()
    },
    axis.title.y = if (title.y) {
      ggplot2::element_text(margin = ggplot2::margin(r = 20), size = text.size)
    } else {
      ggplot2::element_blank()
    },
    plot.margin = if (sec.axis.y) {
      grid::unit(c(0, 1, ifelse(legend.position == "none", 1, 0), 0), "cm")
    } else {
      grid::unit(c(0, 0, ifelse(legend.position == "none", 1, 0), 0), "cm")
    },
    axis.title.y.right = if (sec.axis.y) {
      ggplot2::element_text(vjust = 10, color = sec.axis.y.color)
    } else {
      ggplot2::element_blank()
    },
    axis.text.y.right = if (sec.axis.y) {
      ggplot2::element_text(color = sec.axis.y.color, size = text.size)
    } else {
      ggplot2::element_blank()
    },
    axis.ticks = ggplot2::element_blank(),
    panel.grid = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(
      size = 0.5,
      linetype = ifelse(panel.grid.major == "grid.y", "solid", "blank"),
      color = GEviz::colors_ge("grau")
    ),
    panel.grid.major.x = ggplot2::element_line(
      size = 0.5,
      linetype = ifelse(panel.grid.major == "grid.x", "solid", "blank"),
      color = GEviz::colors_ge("grau")
    ),
    panel.background = ggplot2::element_rect(fill = "white"),
    # panel.border = ggplot2::element_rect(color = "white", fill = NA, size = 8),
    ...
  )
}
