#' Format Numeric Values
#'
#' @description  The `fmt_num` function formats a numeric value to a specified number of 
#' decimal places and a minimum field width, returning it as a character string.
#'
#' @param x A numeric value or a vector of numeric values to be formatted.
#' @param digits An integer specifying the number of decimal places to format `x`.
#' @param width An integer specifying the minimum field width of the formatted string. Default is `digits + 4`.
#' @return A character string or a vector of character strings representing the formatted number(s).
#' @details The `fmt_num` function uses the `formatC` function to format the numeric value `x`. 
#' The `formatC` function is part of base R and is used to format numbers in a flexible and precise manner.
#' 
#' - `format = "f"` indicates that the number should be formatted as a fixed-point number.
#' - `digits` determines the number of decimal places.
#' - `width` sets the minimum width of the resulting string, including the digits before and 
#' after the decimal point, the decimal point itself, and any leading or trailing spaces.
#'
#' @examples
#' # Format a single number
#' fmt_num(123.45678, 2)
#' # [1] " 123.46"
#'
#' # Format a number with a custom width
#' fmt_num(123.45678, 2, width = 10)
#' # [1] "    123.46"
#'
#' # Format multiple numbers in a vector
#' fmt_num(c(123.456, 78.9), 3)
#' # [1] " 123.456" "  78.900"
#' 
#' @seealso \code{\link{formatC}}
#' @export

fmt_num <- function(x, digits, width = digits + 4) {
  formatC(x,
          digits = digits,
          format = "f",
          width = width
  )
}


#' Generate Custom Breaks for Plotting
#' 
#' @description The `custom_breaks` function generates a sequence of breaks for plotting purposes,
#' based on the input value `x` and the desired number of breaks `break_num`.
#' The breaks are rounded to the nearest multiple of 10 if `x` is greater than 10.
#'
#' @param x Numeric. The value used to determine the range of the breaks.
#' @param break_num Integer. The desired number of breaks in the sequence.
#'
#' @return A numeric vector of breaks. If `x` is greater than 10, the breaks are rounded
#' to the nearest multiple of 10. If `x` is 10 or less, the breaks are not rounded.
#'
#' @examples
#' custom_breaks(123, 5)
#' custom_breaks(8, 4)
#'
#' @details
#' This function first checks if `x` is greater than 10. If so, it determines the largest 
#' nearest multiple of 10 greater than or equal to `x`. It then calculates the increment 
#' for the breaks by dividing `x` by `break_num` and rounding to the nearest multiple of 10. 
#' Finally, it generates a sequence of breaks from 0 to the largest nearest multiple of 10, 
#' using the calculated increment.
#' 
#' If `x` is 10 or less, the function calculates the increment by dividing `x` by `break_num`
#' without rounding to the nearest multiple of 10. It then generates a sequence of breaks 
#' from 0 to `x`, using the calculated increment.
#'
#' @export

custom_breaks <- function(x, break_num) {
  
  if(x > 10) {
    largest_nearest_10 <- round(x / 10) * 10
    increment <- round((x/break_num)/ 10) * 10
    breaks <- seq(0, largest_nearest_10, by = increment)
  } else {
    increment <- round(x/break_num)
    breaks <- seq(0, x, by = increment)
  }
  
  return(breaks)
}


#' Round Numeric Values in SAS Style
#'
#' @description The `round_sas` function rounds numeric values in a manner similar to 
#' SAS rounding. It adds a small epsilon to the value to address floating-point precision issues.
#'
#' @param x A numeric value or a vector of numeric values to be rounded.
#' @param digits An integer specifying the number of decimal places to round to. Default is 0.
#' @return A numeric value or a vector of numeric values rounded to the specified number of decimal places.
#' 
#' @details 
#' The `round_sas` function performs rounding by first adjusting the value to the desired precision, 
#' then adding 0.5 plus a small epsilon (to address floating-point precision issues), truncating the result, 
#' and finally adjusting back to the original precision.
#' 
#' The function uses the `sign` function to preserve the sign of the original values and 
#' the `.Machine$double.eps` to handle floating-point arithmetic precision.
#'
#' @examples
#' # Round a single number to 0 decimal places
#' round_sas(123.456)
#'
#' # Round a single number to 2 decimal places
#' round_sas(123.456, 2)
#'
#' # Round multiple numbers in a vector to 1 decimal place
#' round_sas(c(123.456, 78.9), 1)
#' 
#' @export

round_sas <- function(x, digits = 0) {
  
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- z + 0.5 + sqrt(.Machine$double.eps)
  z <- trunc(z)
  z <- z/10^digits
  z * posneg
  
}