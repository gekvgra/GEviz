#' Function to extract GE KVG colors as hex codes
#'
#' @param ... Character names of additional colors
#' @export
#' @examples
#' df <- data.frame(
#'    year = c(2019, 2020, 2021),
#'    value = c(4.2, 10, 29.5)
#'    )
#'
#' ggplot2::ggplot(data = df, ggplot2::aes(y = value, x = year)) +
#'    ggplot2::geom_line(color = GEviz::colors_ge("hellrot"))
#'
colors_ge <- function(...) {
  ge_raw_colors <- c(
    `schwarz` = "#000000",
    `hellrot` = "#C7362F", # aus CI, rgb(199/255, 54/255, 47/255)
    `dunkelrot` = "#781F19", # aus CI, col2rgb("#781F19")
    `dunkelblau` = "#181C28", # aus CI
    `midnightblue` = "#004483", # aus Geschaeftsbericht
    `hellblau` = "#2C4B63", # aus CI
    `lightsteelblue` = "#b9c2df", # aus Geschaeftsbericht
    `lavender` = "#e2e6f3", # aus Geschaeftsbericht
    `whitesmoke` = "#f1f3f9", # aus Geschaeftsbericht
    `grau` = "#878787",
    `hellgrau` = "#bfbfbf",
    `weiss` = "#FFFFFF"
  )
  cols <- c(...)
  if (is.null(cols)) {
    return(ge_raw_colors)
  }
  ge_raw_colors[cols]
}
