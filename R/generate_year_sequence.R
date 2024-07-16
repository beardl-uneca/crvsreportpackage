#' Generate year sequence
#'
#' The generate_year_sequence function generates a sequence of consecutive years,
#' starting with the most recent year provided as input. The sequence includes
#' the specified number of years, counting backwards from the latest year given.
#'
#' @param latest_year The most recent year in the sequence. Must be a numeric value representing a year, such as 2023.
#' @param num_yrs The number of years to include in the sequence, counting backwards from the latest year. Default is 5.
#'
#' @return numeric vector containing the sequence of years
#' @export
#'
#' @examples
#' generate_year_sequence(2023, 5)

generate_year_sequence <- function(latest_year, num_yrs = 5) {
  # Check if the inputs are numeric
  if (!is.numeric(latest_year) || !is.numeric(num_yrs)) {
    stop("Both latest_year and num_yr must be numeric")
  }

  # Check if the inputs are positive
  if (latest_year <= 0 || num_yrs <= 0) {
    stop("Both latest_year and num_yr must be positive")
  }

  # Check if latest_year is a realistic year
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  if (latest_year < 1950 || latest_year > current_year) {
    stop("latest_year must be a realistic year value between 1950 and the current year")
  }

  # Generate the sequence of years
  seq(latest_year - (num_yrs - 1), latest_year)
}
