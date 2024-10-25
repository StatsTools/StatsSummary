#' Round function to follow IB Stats POP. 'This function is deprecated. Please, use `roundmath` function instead.'
#'
#' @param x A number to round.
#' @param digits A number with number of digits to round.
#'
#' @return A rounded number.
#' @export
#'
#' @examples
#' x <- round1(0.456, 1)
round1 <- function(x, digits = 1) {
 negativo <- x < 0
 x <- abs(x)
 palavra <- trunc(readr::parse_number(as.character(x*10^(digits + 1))))
 num_c <- palavra %% 10
 palavra <- trunc(readr::parse_number(as.character(x*10^(digits))))
 resultado <- ifelse(num_c >= 5,palavra + 1,palavra) / (10^(digits))
 resultado <- ifelse(negativo,-resultado,resultado)

 warning('This function is deprecated. Please, use `roundmath` function instead.')

 return(resultado)
}
