#' Return function to interpolate a ge color palette
#'
#' @param palette Character name of palette in ge_palettes. "main" is default, others are: "rotblau", "rot", "blau".
#' @param reverse Boolean indicating whether the palette should be reversed.
#' @param ... Additional arguments to pass to colorRampPalette().
#' @importFrom TSP solve_TSP as.TSP
#' @export
#'
palette_ge <- function(palette = "main", reverse = FALSE, ...) {
  pals_ge <- list(
    `main` = GEviz::colors_ge()[TSP::solve_TSP(TSP::as.TSP(stats::dist(t(grDevices::col2rgb(GEviz::colors_ge())))))],
    `rotblau` = GEviz::colors_ge("midnightblue", "hellrot"),
    `rot` = GEviz::colors_ge("dunkelrot", "hellrot"),
    `blau` = GEviz::colors_ge("midnightblue", "lavender")
  )
  pal <- pals_ge[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}
