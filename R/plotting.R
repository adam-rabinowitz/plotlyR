#' Create a line plot using plotly
#'
#' @param plot_data A data.frame containing plot data
#' @param title Figure title
#' @param x.title Title of x-axis
#' @param y.title Title of y-axis
#' @param leg.title Title of the legend
#' @param colors A named vector supplying colors
#' @param date_format Format to apply to date columns
#' @return A plotly plot
#' @export
line_plotly <- function(
  plot_data, title=NULL, x.title=NULL, y.title=NULL, leg.title=NULL,
  colors=NULL, date_format=NULL
) {
  # Check columns
  check_column_names(plot_data, c('x', 'y', 'trace', 'text'))
  # Format dates
  if (!is.null(date_format)) {
    plot_data <- factor_dates(plot_data, date_format=date_format)
  }
  # Create plot and return
  fig <- plotly::plot_ly(
    data = plot_data,
    x = ~x, 
    y = ~y,
    color = ~trace,
    type = "scatter",
    mode = "lines+markers",
    hovertext = ~text,
    hoverinfo = 'text',
    colors= colors
  )
  # Add titles and format
  fig <- add_titles(
    fig, title=title, x.title=x.title, y.title=y.title, leg.title=leg.title
  )
  fig <- order_xy_axis(fig, plot_data)
  # Return plot
  return(fig)
}


#' Create a vertically stacked bar chart
#'
#' @param plot.data A data.frame containing plot data
#' @param date_format Format to apply to date columns
#' @param title Figure title
#' @param x.title Title of x-axis
#' @param y.title Title of y-axis
#' @param leg.title Title of the legend
#' @param fig A pre-existing plot to which the traces can be added
#' @return A plotly plot
#' @export
bar_plotly <- function(
  plot_data, title=NULL, x.title=NULL, y.title=NULL, leg.title=NULL,
  colors=NULL, date_format=NULL
) {
  # Check columns
  check_column_names(plot_data, c('x', 'y', 'trace', 'text'))
  # Format dates
  if (!is.null(date_format)) {
    plot_data <- factor_dates(plot_data, date_format=date_format)
  }
  # Create bar plot
  fig <- plotly::plot_ly(
    data = plot_data,
    x = ~x, 
    y = ~y,
    color = ~trace,
    type = "bar",
    hovertext = ~text,
    hoverinfo = 'text',
    colors= colors
  )
  # Add titles, format and return
  fig <- add_titles(
    fig, title=title, x.title=x.title, y.title=y.title, leg.title=leg.title
  )
  fig <- order_xy_axis(fig, plot_data)
  return(fig)
}


#' Create a pie chart
#'
#' @param plot.data A data.frame containing plot data
#' @param title Figure title
#' @param leg.title Title of the legend
#' @return A plotly plot
#' @export
pie_plotly <- function(
  plot_data, hole=0.0, title=NULL, leg.title=NULL, colors=NULL
) {
  # Check columns
  check_column_names(plot_data, c('x', 'y', 'text'))
  # Format data
  stopifnot(!any(duplicated(plot_data$x)))
  if (is.factor(plot_data$x)) {
    plot_data <- dplyr::arrange(plot_data, x)
  }
  if (!is.null(colors)) {
    colors <- colors[as.character(plot_data$x)]
  }
  # Create figure
  fig <- plotly::plot_ly(
    plot_data, labels=~x, values=~y, type='pie', hole=hole,
    marker=list(colors=~colors), direction='clockwise', sort=F,
    textposition='none', text=~text, hoverinfo='text'
  )
  # Add titles, format and return
  fig <- add_titles(
    fig, title=title, leg.title=leg.title
  )
  return(fig)
}