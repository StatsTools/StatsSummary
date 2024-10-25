#' Mathematical rounding function with conversion to string
#'
#' @param x A number or array to be rounded.
#' @param decimals A number with the number of decimals points to round.
#'
#' @return Rounded number string.
#' @export
#'
#' @examples
#' roundmath_str(x = 3.995, decimals = 1)
roundmath_str <- function(x, decimals = 1) {
 stopifnot(
  "`x` must be numeric." = is.numeric(x)
 )

 stopifnot(
  "`decimals` must be provided." = !is.na(decimals),
  "`decimals` must be numeric." = is.numeric(decimals),
  "`decimals` cannot be an array." = length(decimals) == 1
 )

 value <- stringr::str_trim(format(roundmath(x, decimals), nsmall = decimals, scientific = FALSE))
 value <- ifelse(value == "NA", NA_character_, value)
 
 return(value)
}
