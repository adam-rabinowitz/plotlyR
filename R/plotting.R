#' Create a line plot using plotly
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
line_plotly <- function(
  plot.data, date_format=NULL, title=NULL, x.title=NULL, y.title=NULL,
  leg.title=NULL, fig=plotly::plot_ly()
) {
  # Check columns
  expected_cols <- c('x', 'y', 'line', 'color', 'text')
  if (base::length(base::setdiff(expected_cols, base::colnames(plot.data)))) {
    stop('plot.data has missing columns')
  }
  # Format dates
  if (!is.null(date_format)) {
    plot.data <- factor_dates(plot.data, date_format=date_format)
  }
  # Split data by line
  if (!is.factor(plot.data$line)) {
    plot.data$line <- factor(plot.data$line, levels=unique(plot.data$line))
  }
  plot.data.list <- split(plot.data, plot.data$line)
  # Add traces to plot and return
  for (line in names(plot.data.list)) {
    color <- plot.data.list[[line]]$color[1]
    fig <- fig %>%
      plotly::add_trace(
        x = plot.data.list[[line]]$x,
        y = plot.data.list[[line]]$y,
        name = line,
        type = 'scatter',
        mode = 'lines+markers',
        hovertext = plot.data.list[[line]]$text,
        hoverinfo = 'text',
        line = list(color = color),
        marker = list(color = color)
      )
  }
  # Format and add titles
  fig <- fig %>%
    add_titles(
      title=title, x.title=x.title, y.title=y.title, leg.title=leg.title
    )
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
  plot.data, date_format=NULL, title=NULL, x.title=NULL, y.title=NULL,
  leg.title=NULL, fig=plotly::plot_ly()
) {
  # Check columns
  expected_cols <- c('x', 'y', 'bar', 'color', 'text')
  if (base::length(base::setdiff(expected_cols, base::colnames(plot.data)))) {
    stop('plot.data has missing columns')
  }
  # Format dates
  if (!is.null(date_format)) {
    plot.data <- factor_dates(plot.data, date_format=date_format)
  }
  # Split data by line
  if (!is.factor(plot.data$bar)) {
    plot.data$bar <- base::factor(plot.data$bar, levels=unique(plot.data$bar))
  }
  plot.data.list <- base::split(plot.data, plot.data$bar)
  # Add traces to plot
  for (bar in names(plot.data.list)) {
    color <- plot.data.list[[bar]]$color[1]
    fig <- fig %>%
      plotly::add_trace(
        x = plot.data.list[[bar]]$x,
        y = plot.data.list[[bar]]$y,
        name = bar,
        type = 'bar',
        hovertext = plot.data.list[[bar]]$text,
        hoverinfo = 'text',
        marker = list(color = color)
      )
  }
  # Format, add titles and return
  fig <- fig %>%
    add_titles(
      title=title, x.title=x.title, y.title=y.title, leg.title=leg.title
    )
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
  plot.data, hole=0.0, title=NULL, leg.title=NULL
) {
  # Check columns
  expected_cols <- c('x', 'y', 'color', 'text')
  if (base::length(base::setdiff(expected_cols, base::colnames(plot.data)))) {
    stop('plot.data has missing columns')
  }
  # Create figure
  fig <- plotly::plot_ly(
    plot.data, labels=~x, values=~y, type='pie', hole=hole,
    marker = list(colors=plot.data$color), direction='clockwise', sort=F,
    textposition='none', text=plot.data$text, hoverinfo='text'
  )
  # Add titles and return
  fig <- fig %>%
    add_titles(title=title, leg.title=leg.title)
  return(fig)
}

# 
# 
# test.data <- data.frame(
#   x = rep(
#     as.Date(c('01-01-2021', '01-02-2021', '01-03-2021'), format='%d-%m-%Y'),
#     times=3
#   ),
#   y = c(10, 20, 70, 30, 30, 40, 40, 50, 10),
#   bar = rep(letters[1:3], each=3),
#   line = rep(letters[1:3], each=3),
#   color = rep(c('red', 'blue', 'green'), each=3)
# )
# test.data$text <- paste(test.data$bar, test.data$y)
# stacked_bar_plotly(test.data, date_format='%b-%Y')





