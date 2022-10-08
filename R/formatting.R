#' Add title to plotly plot
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


#' Order x and y axis if they are factors
#'
#' @param fig a plotly plot
#' @param plot_data The data from which the plot was created
#' @return The formatted plotly plot
#' @export
order_xy_axis <- function(
  fig, plot_data
) {
  # Order x axis
  if (is.factor(plot_data[['x']])) {
    fig <- fig %>%
      plotly::layout(
        xaxis = list(
          categoryorder = 'array',
          categoryarray = levels(plot_data$x)
        )
      )
  }
  # Order y axis
  if (is.factor(plot_data[['y']])) {
    fig <- fig %>%
      plotly::layout(
        yaxis = list(
          categoryorder = 'array',
          categoryarray = levels(plot_data$y)
        )
      )
  }
  return(fig)
}


#' Add veritcal and horizontal lines to plot
#'
#' @param fig a plotly plot
#' @param yintercepts y-axis intercept of horizontal lins
#' @param xintercepts x-axis intercept of vertical lines
#' @param color color of lines
#' @param opacity opacity of lines
#' @return The formatted plotly plot
#' @export
add_lines <- function(
    fig, yintercepts=NULL, xintercepts=NULL, color='black', opacity=1
) {
  # Check arguments
  if (length(c(yintercepts, xintercepts)) == 0) {
    stop('one of xintercepts and xintercepts must be supplied')
  }
  if (opacity < 0 | opacity > 1) {
    stop('opacity must be between 0 and 1')
  }
  # Create shapes for horizontal lines
  h_lines = list()
  for (i in seq_along(yintercepts)) {
    h_lines[[i]] <- list(
      type = "line",
      x0 = 0,
      x1 = 1,
      xref = "paper",
      y0 = yintercepts[i],
      y1 = yintercepts[i],
      line = list(color = color),
      opacity=opacity
    )
  }
  # Create shapes for vertical lines
  v_lines = list()
  for (i in seq_along(xintercepts)) {
    v_lines[[i]] <- list(
      type = "line",
      x0 = xintercepts[i],
      x1 = xintercepts[i],
      y0 = 0,
      y1 = 1,
      yref = "paper",
      line = list(color = color),
      opacity=opacity
    )
  }
  # Create shapes, apply and return
  shapes <- c(h_lines, v_lines)
  fig <- fig %>% plotly::layout(shapes=shapes)
  return(fig)
}


#' Function to add watermark at centre of plot
#' @param fig a plotly plot
#' @param source source of image
#' @param opacity opacity of image
#' @param size size relative to plot
#' @export
add_watermark <- function(fig, source, opacity, size=1) {
  # Add figure and return
  fig <- fig %>%
    plotly::layout(
      images=list(
        list(
          source = source,
          xref = 'paper',
          yref = 'paper',
          x = 0.5,
          y = 0.5,
          xanchor = 'center',
          yanchor = 'middle',
          sizex = size,
          sizey = size,
          opacity = opacity
        )
      )
    )
  return(fig)
}


#' Clean plotly plots
#'
#' @param fig a plotly plot
#' @return The plotly plot with bar buttons removed
#' @export
clean_plotly <- function(fig) {
  fig <- fig %>%
    plotly::config(
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
  return(fig)
}