#' Calculate Min and Max function
#'
#' @param data A tibble with the data to be used.
#' @param var_expr A name with the name of the variable in data.
#' @param group_expr A name with the name of the group variable in data.
#' @param decimals A number with the number of decimals points to present the results.
#' @param groups_order A character array with the names of the groups in order to be presented in the results.
#' @param dec_sep A character to specify the decimal separator to be used.
#' @param sep A character with a separator symbol
#' @param adjust A character with the name of the decimals adjustment function. Use 'roundmath', 'round' or 'trunc'.
#'
#' @return A tibble with the Min and Max results by group.
#' @export
#'
#' @examples
#' data <- tibble(titer = c(rnorm(100),rbeta(100, 0.2, 0.2)), group = c(rep(1, 100), rep(2, 100)))
#' calc_minmax(data, titer, group, dec_sep = '.', decimals = 2, groups_order = c('2', '1'), sep = ';', adjust = 'roundmath')
calc_minmax <- function(data, var_expr, group_expr, decimals = 1, groups_order = NA_character_, dec_sep = '.', sep = '\u2012', adjust = 'roundmath') {
# Validation Step -------------------------------------------------------------
 var_expr <- substitute(var_expr)
 group_expr <- substitute(group_expr)

 stopifnot("`data` must be a tibble." = tibble::is_tibble(data))

 stopifnot(
  "`var_expr` must be a name." = is.name(var_expr),
  "`var_expr` is missing in data." = as.character(var_expr) %in% names(data)
 )

 stopifnot(
  "`group_expr` must be a name." = is.name(group_expr),
  "`group_expr` is missing in data." = as.character(group_expr) %in% names(data)
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

 stopifnot(
  "`sep` must be provided." = !is.na(sep),
  "`sep` must be a character." = is.character(sep),
  "`sep` cannot be an array." = length(sep) == 1
 )

 stopifnot(
    "`adjust` must be a character." = is.character(adjust),
    "`adjust` cannot be an array." = length(adjust) == 1,
    "`adjust` must be 'roundmath', 'round' or 'trunc'." = adjust %in% c('roundmath', 'round', 'trunc')
  )

  trunc1 <- function(x, decimals) {
    trunc(x * 10^decimals) / 10^decimals
  }

  if (adjust == 'roundmath') {
    adjust <- roundmath
  } else if (adjust == 'round') {
    adjust <- round
  } else if (adjust == 'trunc') {
    adjust <- trunc1
  }

 if (any(!is.na(groups_order))) {

  if (is.factor(data |> dplyr::pull(!!group_expr))) {
   groups <- levels(data |> dplyr::pull(!!group_expr))
  } else {
   groups <- data |> dplyr::pull(!!group_expr) |> unique() |> as.character()
  }

  stopifnot(
   "`groups_order` must be a character." = is.character(groups_order),
   "Groups are missing in `groups_order`." = length(groups_order) == length(groups),
   "Groups are missing in `groups_order`." = all(sort(groups_order) == sort(groups))
  )
  groups <- groups_order
 } else {
  if (is.factor(data |> dplyr::pull(!!group_expr))) {
   groups <- levels(data |> dplyr::pull(!!group_expr))
  } else {
   groups <- data |> dplyr::pull(!!group_expr) |> unique() |> as.character()
  }
 }

# Development Step ------------------------------------------------------------
 values_names <- expand.grid(var = c('N', 'RES'), group = groups) |> dplyr::mutate(var_final = paste0(var, '_', group)) |> dplyr::pull(var_final)

 result <- data |>
  dplyr::filter(!is.na(!!var_expr)) |>
  dplyr::mutate(group = as.character(!!group_expr)) |>
  dplyr::group_by(group) |>
  dplyr::reframe(n = dplyr::n(), min = min(!!var_expr), max = max(!!var_expr)) |>
  dplyr::right_join(tibble::tibble(group = groups), by = 'group') |>
  dplyr::rowwise() |>
  dplyr::mutate(
   n = ifelse(is.na(n), '0', roundmath_str(adjust(n, 0), 0)),
   min = ifelse(n == 0, 'NC', roundmath_str(adjust(min, decimals), decimals)),
   max = ifelse(n == 0, 'NC', roundmath_str(adjust(max, decimals), decimals))
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
   N = n,
   RES = ifelse(min == 'NC' | max == 'NC', 'NC', paste0(min, ' ', sep, ' ', max))
  ) |>
  dplyr::select(group, N, RES) |>
  tidyr::pivot_wider(names_from = group, values_from = c(N, RES)) |>
  dplyr::select(!!values_names)

 if (dec_sep != '.') {
   result <- result |>
     dplyr::mutate_all(function(x) gsub("\\.", dec_sep, x))
 }

 return(result)
}
