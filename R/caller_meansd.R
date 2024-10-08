#' Calculate Mean and Standard Deviation
#'
#' @param data A tibble with the data to be used.
#' @param var_expr A name with the name of the variable in data.
#' @param group_expr A name with the name of the group variable in data.
#' @param decimals A number with the number of decimals points to present the results.
#' @param groups_order A character array with the names of the groups in order to be presented in the results.
#'
#' @return A tibble with the mean and standard deviation results by group.
#' @export
#'
#' @examples
#' data <- tibble(titer = c(rnorm(100),rbeta(100, 0.2, 0.2)), group = c(rep(1, 100), rep(2, 100)))
#' caller_meansd(data, titer, group, decimals = 2, groups_order = c('2', '1'))
caller_meansd <- function(data, var_expr, group_expr, decimals = 1, groups_order = NA_character_) {
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

 values_names <- expand.grid(var = c('N', 'RES'), group = groups) |> dplyr::mutate(var_final = paste0(var, '_', group)) |> dplyr::pull(var_final)

 result <- data |>
  dplyr::filter(!is.na(!!var_expr)) |>
  dplyr::mutate(group = as.character(!!group_expr)) |>
  dplyr::group_by(group) |>
  dplyr::reframe(n = dplyr::n(), mean = mean(!!var_expr), sd = sd(!!var_expr)) |>
  dplyr::right_join(tibble::tibble(group = groups), by = 'group') |>
  dplyr::rowwise() |>
  dplyr::mutate(
   mean = ifelse(n == 0, 'NC', round1_str(mean, decimals)),
   sd = ifelse(n < 2, 'NC', round1_str(sd, decimals))
  ) |>
  dplyr::mutate(
   n = ifelse(is.na(n), '0', round1_str(n, 0))
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
   N = n,
   RES = paste0(mean, ' (', sd, ')')
  ) |>
  dplyr::mutate(RES = ifelse(RES == 'NC (NC)', 'NC', RES)) |>
  dplyr::select(group, N, RES) |>
  tidyr::pivot_wider(names_from = group, values_from = c(N, RES)) |>
  dplyr::select(!!values_names)

 return(result)
}
