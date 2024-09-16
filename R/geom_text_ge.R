#' Text layer for ggplot2
#'
#' @param label Variable from input object for naming the data points in the plot.
#' @param flip Do you flip the coordinates of this plot at the end? If yes, activate flip = TRUE.
#' @importFrom rlang .data
#' @export
#' @author Yannick Schwarz
#' @examples
#' df <- data.frame(
#'    year = c(2019, 2020, 2021),
#'    value = c(4.2, 10, 29.5))
#'
#' ggplot2::ggplot(data = df, ggplot2::aes(x = year, y = value)) +
#'    ggplot2::geom_bar(stat = "identity") +
#'    GEviz::geom_text_ge(label = "year")
#'
geom_text_ge <- function(label, flip = FALSE) {
  ggplot2::geom_text(
    ggplot2::aes(label = .data[[label]]),
    vjust = 0.31,
    position = ggplot2::position_stack(vjust = 0.5),
    color = "white",
    size = 9 * 0.28
    # Text size is in points not in mm, thus conversion is needed
    # https://stackoverflow.com/questions/17311917/ggplot2-the-unit-of-size/17312440#17312440
  )
}
