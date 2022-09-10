#' Apply default formatting to plot
#'
#' @param fig a plotly plot
#' @param title plot title
#' @param x.title x-axis title
#' @param y.title x-axis title
#' @return The formatted plotly plot
#' @export
add_titles <- function(
  fig, title=NULL, x.title=NULL, y.title=NULL, leg.title=NULL
) {
  # Add titles and return
  fig <- fig %>%
    plotly::layout(
      title = list(
        text = title
      ),
      xaxis = list(
        title = x.title
      ),
      yaxis=list(
        title = y.title
      ),
      legend = list(
        title = list(text=leg.title)
      )
    )
  return(fig)
}

#' Apply default formatting to plot
#'
#' @param fig a plotly plot
#' @return The plotly plot with bar buttons removed
#' @export
clean_plotly <- function(fig) {
  plot %>% plotly::config(
    modeBarButtonsToRemove = c(
      'zoom2d',
      'zoomIn2d',
      'zoomOut2d',
      'pan2d',
      'select2d',
      'lasso2d', 
      'autoScale2d',
      'hoverClosestCartesian',
      'hoverCompareCartesian',
      'toggleSpikelines',
      'resetScale2d',
      'hoverClosestPie'
    ),
    displaylogo = FALSE
  )
}