# Authors@R: aspeyb
#
#'@title round_excel
#'
#'@description A function to implement the Excel method of rounding.
#'
#'@details Given a decimal, the function will round the value to the specified number of decimal
#'places using the Excel method of rounding (rounding up from .5).
#'
#'@param x Value or values to round
#'
#'@param n Number of significant digits
#'
#'@return A rounded value or vector of values
#'
#'@examples round_excel(c(1.6, 5.9, 2.5), 0)
#'
#'@export

round_excel <- function(x, n = 0){
  scale <- 10^n
  rounded <- trunc(x * scale + sign(x) * 0.5) / scale

  return(rounded)
}
