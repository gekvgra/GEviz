#' Fill scale constructor for GE KVG colors
#'
#' @param palette Character name of palette in ge_palettes. "main" is default, others are: "rotblau", "rot", "blau".
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#' @export
#'
scale_fill_ge <- function(palette = "main",
                          discrete = TRUE,
                          reverse = FALSE,
                          ...) {
  pal <- GEviz::palette_ge(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale(
      "fill",
      paste0("ge_", palette),
      palette = pal,
      ...
    )
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}
