#' Mathematical rounding function
#'
#' @param x A number or array to be rounded.
#' @param decimals A number with the number of decimals points to round.
#'
#' @return Rounded number.
#' @export
#'
#' @examples
#' x <- roundmath(x = 0.456, decimals = 1)
roundmath <- function(x, decimals = 1) {
 negative <- x < 0
 x <- abs(x)
 
 word <- trunc(readr::parse_number(as.character(x*10^(decimals + 1))))
 num_c <- word %% 10
 word <- trunc(readr::parse_number(as.character(x*10^(decimals))))
 
 value <- ifelse(num_c >= 5, word + 1, word) / (10^(decimals))
 value <- ifelse(negative, -value, value)

 return(value)
}
