#' Round1 function with conversion to string
#'
#' @param x A number or array to be rounded.
#' @param decimals A number with de number of decimals points to round.
#'
#' @return Rounded number.
#' @export
#'
#' @examples
#' data <- tibble(titer = c(rnorm(100),rbeta(100, 0.2, 0.2)), group = c(rep(1, 100), rep(2, 100)))
#' caller_gmtr(titer, group, groups_order = c('2', '1'), data, decimals = 2)
round1_str <- function(x, decimals = 1) {
 stopifnot(
  "`x` must be provided." = !is.na(x),
  "`x` must be numeric." = is.numeric(x)
 )

 stopifnot(
  "`decimals` must be provided." = !is.na(decimals),
  "`decimals` must be numeric." = is.numeric(decimals),
  "`decimals` cannot be an array." = length(decimals) == 1
 )

 stringr::str_trim(format(round1(x, decimals), nsmall = decimals, scientific = FALSE))
}
