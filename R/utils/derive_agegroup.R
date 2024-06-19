
#' @title derive_age_groups
#' @description  Derives agegroupings according to how the parameters are set.
#'
#' @param age_in_yrs_col name of the age in years variable used to derive group.
#' @param start_age this is the lowest age for the first group, excluding < 1,
#' if set to 5 then first group will be 01-04.
#' @param max_band this is the final band in the age groupings e.g. 85+.
#' @param step_size this is the width of each age band in years.
#' @param under_1 set to TRUE if you require an 'under one year old' group.
#'
#' @return returns an agegroup based on set parameters
#'
#' @examples
#' data <- data %>%
#' mutate(AGEGROUP = derive_age_groups(AGEINYRS, start_age = 5, max_band = 95,
#' step_size = 5, under_1 = TRUE))
#' data <- data %>%
#' mutate(AGEGROUP = derive_age_groups(AGEINYRS, start_age = 30, max_band = 95,
#' step_size = 5, under_1 = FALSE))
#'
#' @import dplyr
#'
#' @export
#'
derive_age_groups <- function(age_in_yrs_col, start_age, max_band, step_size,
                              under_1 = TRUE) {
  if(under_1 == TRUE) {
    intervals <- c(0, 1, seq(start_age, max_band, step_size), Inf)

    ages <- mapply(function(range_start, range_end) {
    paste0(sprintf("%02d", range_start), "-", sprintf("%02d", range_end - 1))},
    intervals[2:(length(intervals) - 2)], intervals[3:(length(intervals) - 1)]
    )

    ages <- c("Under 1", ages, paste0(max_band, " and over"))

    agegroup <- as.character(cut(age_in_yrs_col, breaks = intervals,
                                 right = FALSE, labels = ages))

  } else {
    intervals <- c(0, seq(start_age, max_band, step_size), Inf)

    ages <- mapply(function(range_start, range_end) {
    paste0(sprintf("%02d", range_start), "-", sprintf("%02d", range_end - 1))},
    intervals[2:(length(intervals) - 2)], intervals[3:(length(intervals) - 1)]
    )

    ages <- c(paste0("Under ", start_age), ages, paste0(max_band, " and over"))

    agegroup <- as.character(cut(age_in_yrs_col, breaks = intervals,
                                 right = FALSE, labels = ages))
  }
  return(agegroup)
}
