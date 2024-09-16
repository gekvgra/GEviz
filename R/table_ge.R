#' Table with GE style
#'
#' Generated with huxtable
#'
#' @param df A \code{data.frame} object as input.
#' @param titel Character string used for the caption above of the table.
#' @param beschrieb Character string used for the description below the table.
#' @param quelle Character string declarating the source of the table.
#' @param digits Integer indicating the number of digits.
#' @param relative_width Fraction indicating the fraction of the page which the table should consume.
#' @importFrom magrittr "%>%"
#' @export
#' @author Yannick Schwarz
#' @seealso \link[huxtable]{huxtable} and for quick output see [huxtable::quick_xlsx()] and use \code{set_font("Arial")} before.
#' @examples table_ge(mtcars[1:5,], titel = "Kurzer Titel", beschrieb = "Langer Beschrieb.")
#'
table_ge <- function(
    df,
    titel,
    beschrieb,
    quelle = "Eigene Darstellung, GE KVG.",
    digits = 2,
    relative_width = 1
){
  huxtable::as_hux(df) %>%
    huxtable::set_align(col = 1, value = "left") %>%
    huxtable::set_align(col = 2:ncol(.), value = "center") %>%
    huxtable::set_valign(row = 1, value = "middle") %>%
    huxtable::set_bold(row = 1, value = TRUE) %>%
    huxtable::set_background_color(row = 1, value = colors_ge("midnightblue")) %>%
    huxtable::set_text_color(row = 1, value = colors_ge("weiss")) %>%
    huxtable::set_bottom_border(
      huxtable::brdr(
        thickness = 0.4,
        style = "solid",
        color = colors_ge("midnightblue")
      )
    ) %>%
    huxtable::add_footnote(
      text = paste(beschrieb, " Quelle: " ,quelle, sep = " "),
      border = 1
    ) %>%
    huxtable::set_caption(titel) %>%
    huxtable::set_caption_pos("topleft") %>%
    huxtable::set_position("center") %>%
    huxtable::set_latex_float("h") %>%
    huxtable::set_number_format(digits) %>%
    huxtable::set_width(relative_width) %>%
    huxtable::set_font_size(9)
}
