#' Plotly layout with GE defaults
#'
#' @param plotly_object A plotly object.
#' @param plot_title,xaxis_title,yaxis_title,yaxis2_title,legend_title Character vectors with the titles.
#' @param legend_h_adjust Numeric value which can be positive and negative. Default is -0.1.
#' @param hovermode Character vector with default "x". Other possible values are "y", "closest", FALSE, "x unified" and "y unified".
#' @param legend_title_space Character vector with white space to align legend title.
#' @param yaxis2 Logical if a secondary y-axis is desired. Default is FALSE.
#' @param yaxis2_args Further arguments which are passed on to \code{layout}.
#' @param font_size Integer with the font size.
#' @param ... Further arguments for \code{ggplotly()}. See \link[plotly]{ggplotly} for more.
#' @importFrom magrittr "%>%"
#' @export
#'
plotly_gelayout <- function(plotly_object,
                            plot_title = "",
                            xaxis_title = "",
                            yaxis_title = "",
                            yaxis2_title = "",
                            legend_title = "",
                            legend_h_adjust = -0.2,
                            hovermode = "x",
                            legend_title_space = NULL,
                            yaxis2 = FALSE,
                            yaxis2_args = NULL,
                            font_size = 12,
                            ...) {
  yaxis2_args_default <- list(
    overlaying = "y",
    side = "right",
    title = yaxis2_title,
    titlefont = list(size = font_size),
    showgrid = FALSE,
    fixedrange = TRUE,
    zeroline = FALSE
  )
  if (is.null(yaxis2_args)) {
    yaxis2_args_default <- append(
      yaxis2_args_default,
      list(
        dtick = 100 / 7,
        tick0 = 100 / 7,
        tickformat = ".0f"
      )
    )
  } else {
    yaxis2_args_default <- append(
      yaxis2_args_default,
      yaxis2_args
    )
  }
  plotly_object %>%
    plotly::layout(
      title = plot_title,
      xaxis = list(
        title = xaxis_title,
        titlefont = list(size = font_size),
        showgrid = FALSE
      ),
      yaxis = list(
        title = yaxis_title,
        titlefont = list(size = font_size),
        zeroline = FALSE
      ),
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
      font = list(family = "arial", size = font_size),
      hovermode = hovermode,
      clickmode = "select",
      autosize = TRUE,
      margin = list(
        l = 50,
        r = 50,
        b = 50,
        t = 50,
        pad = 4
      ),
      ...
    ) %>%
    {
      if (yaxis2) {
        plotly::layout(
          .,
          yaxis2 = yaxis2_args_default
        )
      } else {
        .
      }
    } %>%
    plotly::config(
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
}
