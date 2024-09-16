#' GE KVG defaults for ggplotly
#'
#' @param ggplot_object An object from \code{ggplot2}.
#' @param legend_h_adjust Numeric value which can be positive and negative. Default is -0.1.
#' @param hovermode Character vector with default "x". Other possible values are "y", "closest", FALSE, "x unified" and "y unified".
#' @param legend_title_space Character vector with white space to align legend title.
#' @param legend_title Character vector for the title of the legend.
#' @param ... Further arguments for \code{ggplotly()}. See \link[plotly]{ggplotly} for more.
#' @importFrom magrittr "%>%"
#' @importFrom stringr str_split
#' @export
#'
ggplotly_ge <- function(ggplot_object,
                        legend_h_adjust = -0.1,
                        hovermode = "x",
                        legend_title_space = NULL,
                        legend_title = "",
                        ...) {
  plotly_prep <- plotly::ggplotly(
    ggplot_object,
    tooltip = "text"
  ) %>%
    plotly::layout(
      legend = list(
        orientation = "h",
        itemclick = "toggle",
        itemdoubleclick = "toggleothers",
        y = legend_h_adjust,
        x = 0.5,
        xanchor = "center",
        title = list(
          text = paste0(legend_title_space, legend_title),
          side = "top"
        )
      ),
      font = list(family = "arial"),
      xaxis = list(
        showgrid = ifelse(
          ggplot_object$theme$panel.grid.major.x$linetype != "blank",
          TRUE,
          FALSE
        )
      ),
      yaxis = list(
        showgrid = ifelse(
          ggplot_object$theme$panel.grid.major.y$linetype != "blank",
          TRUE,
          FALSE
        ),
        title = list(
          standoff = as.numeric(ggplot_object$theme$axis.title.y$margin[2])
        )
      ),
      margin = list(pad = 5),
      hovermode = hovermode,
      clickmode = "select",
      ...
    ) %>%
    plotly::config( # Configuration of the modebar
      modeBarButtonsToRemove = c(
        "pan",
        "zoom",
        "zoomIn",
        "zoomOut",
        "autoScale",
        "resetScale",
        "toggleSpikelines",
        "lasso2d",
        "select2d",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      )
    )
  for (i in 1:length(plotly_prep$x$data)) {
    if (!is.null(plotly_prep$x$data[[i]]$name)) {
      plotly_prep$x$data[[i]]$name =  gsub("\\(","",stringr::str_split(plotly_prep$x$data[[i]]$name,",")[[1]][1])
    }
  }
  plotly_prep
}
