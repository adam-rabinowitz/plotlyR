#' Find name of columns containing date data
#'
#' @param tb A data.frame or tibble
#' @param min_cols Minimum number of columns to be found
#' @return Names of columns containing dates
#' @export
find_date_cols <- function(
  tb, min_cols=1
) {
  # Find columns that are dates
  col_classes <- base::lapply(tb, class)
  date_classes <- base::lapply(
    col_classes, base::grepl, pattern='^(Date|POSIX)'
  )
  date_cols <- base::sapply(date_classes, any)
  # Extract names, check and return
  date_col_names <- base::colnames(tb)[date_cols]
  base::stopifnot(base::length(date_col_names) >= min_cols)
  return(date_col_names)
}


#' Format and factor columns containing date data
#'
#' @param tb A data.frame or tibble
#' @param date_format Format to use with date columns
#' @param min_cols Minimum number of columns to be found
#' @return The data.frame or tibble with formatted columns
#' @export
factor_dates <- function(
  tb, date_format, min_cols=1
) {
  # Find columns that are dates
  date_cols <- find_date_cols(tb=tb, min_cols=min_cols)
  # Format date columns
  for (date_col in date_cols) {
    # Get for
    uniq_dates <- base::sort(base::unique(tb[[date_col]]))
    uniq_levels <- base::format(uniq_dates, format=date_format)
    # Factor dates
    tb[[date_col]] <- factor(
      base::format(tb[[date_col]], format=date_format),
      levels=uniq_levels 
    )
  }
  # Return formatted data
  return(tb)
}


#' Checks expected columns are in a table
#'
#' Raises an error if expected columns are absent.
#'
#' @param tb A data.frame or tibble
#' @param expected_names Expected columns names within tb
#' @export
check_column_names <- function(
  tb, expected_names  
) {
  # Check column names
  missing <- base::setdiff(expected_names, base::colnames(tb))
  # Raise an error if any are missing
  if (length(missing) > 0) {
    base::stop(
      base::paste(
        'the following columns are missing:',
        base::paste(missing, collapse=', ')
      )
    )
  }
  base::invisible()
}