#' Mathematical rounding function with conversion to string
#'
#' @param x A number or array to be rounded.
#' @param decimals A number with the number of decimals points to round.
#' @param dec_sep A character to specify the decimal separator to be used.
#'
#' @return Rounded number string.
#' @export
#'
#' @examples
#' roundmath_str(x = 3.995, decimals = 1, dec_sep = ".")
roundmath_str <- function(x, decimals = 1, dec_sep = ".") {
# Validation Step -------------------------------------------------------------
 stopifnot(
  "`x` must be numeric." = is.numeric(x)
 )

 stopifnot(
  "`decimals` must be provided." = !is.na(decimals),
  "`decimals` must be numeric." = is.numeric(decimals),
  "`decimals` cannot be an array." = length(decimals) == 1
 )
 
 stopifnot(
  "`dec_sep` must be provided." = !is.na(dec_sep),
  "`dec_sep` must be character." = is.character(dec_sep),
  "`dec_sep` cannot be an array." = length(dec_sep) == 1
 )
# Development Step ------------------------------------------------------------
 value <- stringr::str_trim(format(roundmath(x, decimals), nsmall = decimals, scientific = FALSE))
 value <- ifelse(value == "NA", NA_character_, value)

 if (dec_sep != '.') {
  value <- gsub("\\.", dec_sep, value)
 }
  
 return(value)
}
