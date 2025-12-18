#' Calculate N and Frequency function
#'
#' @param data A tibble with the data to be used.
#' @param var_expr A name with the name of the variable in data.
#' @param group_expr A name with the name of the group variable in data.
#' @param pop_size A named vector with population size by group. The names must be the names of the groups to be used.
#' @param with_ci A logical to specify if calculate CI.
#' @param ci_level A number to specify the confidence level for the returned confidence interval.
#' @param decimals A number with the number of decimals points to present the results.
#' @param groups_order A character array with the names of the groups in order to be presented in the results.
#' @param simplified A logical to specify if Simplify the frequency presentation.
#' @param dec_sep A character to specify the decimal separator to be used.
#' @param zero_to_less A logical to specify if zero percentage should include < signal. Only applicable if decimals is greater than zero.
#' @param remove_dec100 A logical to specify if 100.0 percent should be treated as 100. Only applicable if decimals is greater than zero.
#' @param ci_sep A character with a separator symbol to be used in CI.
#' @param adjust A character with the name of the decimals adjustment function. Use 'roundmath', 'round' or 'trunc'.
#'
#' @return A tibble with the N and Frequency results.
#' @export
#'
#' @examples
#' teste <- tibble(dat = factor(c('A', 'B', 'C', 'C', 'A', 'B'), levels = c('A', 'C', 'B', 'D')), group = c(1, 2, 1, 2, 1, 2))
#' calc_nfreq(teste, dat, group, pop_size = c('1' = 4, '2' = 4), simplified = TRUE, groups_order = c('2', '1'), decimals = 1, with_ci = TRUE, ci_sep = ';', adjust = 'roundmath')
calc_nfreq <- function(data, var_expr, group_expr, pop_size = NA_real_, with_ci = TRUE, ci_level = 0.95, decimals = 1, groups_order = NA_character_, simplified = FALSE, dec_sep = '.', zero_to_less = FALSE, remove_dec100 = TRUE, ci_sep = '\u2012', adjust = 'roundmath') {
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
  "`decimals` cannot be an array." = length(decimals) == 1,
  "`decimals` must be greater than or equal to zero." = decimals >= 0
 )

 stopifnot(
  "`ci_sep` must be provided." = !is.na(ci_sep),
  "`ci_sep` must be a character." = is.character(ci_sep),
  "`ci_sep` cannot be an array." = length(ci_sep) == 1
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

 stopifnot(
  "`with_ci` must be provided." = !is.na(with_ci),
  "`with_ci` must be logical." = is.logical(with_ci),
  "`with_ci` cannot be an array." = length(with_ci) == 1
 )

 stopifnot(
  "`simplified` must be provided." = !is.na(simplified),
  "`simplified` must be logical." = is.logical(simplified),
  "`simplified` cannot be an array." = length(simplified) == 1
 )

 stopifnot(
   "`dec_sep` must be provided." = !is.na(dec_sep),
   "`dec_sep` must be character." = is.character(dec_sep),
   "`dec_sep` cannot be an array." = length(dec_sep) == 1
 )

 stopifnot(
   "`zero_to_less` must be provided." = !is.na(zero_to_less),
   "`zero_to_less` must be logical." = is.logical(zero_to_less),
   "`zero_to_less` cannot be an array." = length(zero_to_less) == 1
 )

 if (zero_to_less == TRUE & decimals == 0) {
   stop("`zero_to_less` can only be applied with decimals is greater than zero.")
 }

 stopifnot(
   "`remove_dec100` must be provided." = !is.na(remove_dec100),
   "`remove_dec100` must be logical." = is.logical(remove_dec100),
   "`remove_dec100` cannot be an array." = length(remove_dec100) == 1
 )

 if (remove_dec100 == TRUE & decimals == 0) {
   stop("`remove_dec100` can only be applied with decimals is greater than zero.")
 }

 if (any(!is.na(pop_size))) {
  is.named.vector.numeric <- function(vector) is.vector(vector) & is.numeric(vector) & !is.null(names(vector)) & !any(is.na(names(vector)))

  stopifnot(
   "`pop_size` must be numeric and named with the groups names." = is.named.vector.numeric(pop_size),
   "Groups are missing in `pop_size`." = length(pop_size) == length(groups),
   "Groups are missing in `pop_size`." = all(sort(names(pop_size)) == sort(groups))
  )

  if (is.factor(data |> dplyr::pull(!!var_expr))) {
   var_cat <- levels(data |> dplyr::pull(!!var_expr))
  } else {
   var_cat <- data |> dplyr::pull(!!var_expr) |> unique() |> as.character()
  }

# Development Step ------------------------------------------------------------
  pop_tib <- tibble::tibble(group = names(pop_size), total = pop_size)
  grid_tib <- expand.grid(group = groups, var = var_cat)

  n_denom_group <- grid_tib |>
   dplyr::left_join(pop_tib, by = 'group')

 } else {
  if (is.factor(data |> dplyr::pull(!!var_expr))) {
   var_cat <- levels(data |> dplyr::pull(!!var_expr))
  } else {
   var_cat <- data |> dplyr::pull(!!var_expr) |> unique() |> as.character()
  }

  grid_tib <- expand.grid(group = groups, var = var_cat)

  n_denom_group <- data |>
   dplyr::filter(!is.na(!!var_expr)) |>
   dplyr::mutate(group = as.character(!!group_expr)) |>
   dplyr::group_by(group) |>
   dplyr::summarise(total = dplyr::n()) |>
   dplyr::right_join(grid_tib, by = 'group') |>
   dplyr::mutate(total = ifelse(is.na(total), 0, total)) |>
   dplyr::select(group, var, total)
 }

 result <- data |>
  dplyr::filter(!is.na(!!var_expr)) |>
  dplyr::mutate(group = as.character(!!group_expr)) |>
  dplyr::mutate(var = as.character(!!var_expr)) |>
  dplyr::group_by(group, var) |>
  dplyr::summarise(n = dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::right_join(n_denom_group, by = c('group', 'var')) |>
  dplyr::mutate(n = ifelse(is.na(n), 0, n)) |>
  dplyr::rowwise() |>
  dplyr::mutate(RES = ifelse(total == 0, 'NC', roundmath_str(adjust(100 * n / total, decimals), decimals)))

 if (with_ci == TRUE) {
  ic1_fun <- function(n, total) binom.test(n, total, conf.level = ci_level)$conf.int[1]
  ic2_fun <- function(n, total) binom.test(n, total, conf.level = ci_level)$conf.int[2]

  if (remove_dec100 == TRUE) {
    dec100 <- stringr::str_flatten(c('100.', rep('0', decimals)))

    result <- result |>
      dplyr::mutate(RES = ifelse(RES == dec100, '100', RES)) |>
      dplyr::mutate(
        CI95_1 = ifelse(total == 0, 'NC', roundmath_str(adjust(100 * mapply(ic1_fun, n, total), decimals), decimals)),
        CI95_2 = ifelse(total == 0, 'NC', roundmath_str(adjust(100 * mapply(ic2_fun, n, total), decimals), decimals))
      ) |>
      dplyr::mutate(CI95_1 = ifelse(CI95_1 == dec100, '100', CI95_1), CI95_2 = ifelse(CI95_2 == dec100, '100', CI95_2)) |>
      dplyr::mutate(CI95 = ifelse(total == 0, 'NC', paste0('(', CI95_1, ' ', ci_sep, ' ', CI95_2, ')')))
  } else {
    result <- result |>
      dplyr::mutate(
        CI95_1 = ifelse(total == 0, 'NC', roundmath_str(adjust(100 * mapply(ic1_fun, n, total), decimals), decimals)),
        CI95_2 = ifelse(total == 0, 'NC', roundmath_str(adjust(100 * mapply(ic2_fun, n, total), decimals), decimals))
      ) |>
      dplyr::mutate(CI95 = ifelse(total == 0, 'NC', paste0('(', CI95_1, ' \u2012 ', CI95_2, ')')))
  }

  values_names <- c('VAR', expand.grid(var = c('TOTAL', 'N', 'RES', 'CI95'), group = groups) |> dplyr::mutate(var_final = paste0(var, '_', group)) |> dplyr::pull(var_final))

  if (simplified == TRUE) {
   result <- result |>
    dplyr::mutate(M = ifelse(total == 0, 'NC', paste0(roundmath_str(adjust(n, 0), 0), '/', roundmath_str(adjust(total, 0), 0)))) |>
    dplyr::mutate(N = NA_character_)
  } else {
   result <- result |>
    dplyr::mutate(N = ifelse(total == 0, '0', roundmath_str(adjust(total, 0), 0))) |>
    dplyr::mutate(M = ifelse(total == 0, 'NC', roundmath_str(adjust(n, 0), 0)))
  }

  result <- result |>
  dplyr::ungroup() |>
  dplyr::select(group, var, N, M, RES, CI95) |>
  dplyr::rename(VAR = var, TOTAL = N) |>
  dplyr::rename(N = M)

  if (zero_to_less == TRUE) {
    zero <- stringr::str_flatten(c('0.', rep('0', decimals)))
    new_zero <- stringr::str_flatten(c('<0.', rep('0', decimals - 1), '1'))

    result <- result |>
      dplyr::rowwise() |>
      dplyr::mutate(RES = ifelse(RES == zero & substr(N, 1, 1) != '0', new_zero, RES)) |>
      dplyr::mutate(CI95 = ifelse(substr(N, 1, 1) != '0', gsub(paste0('\\(', zero, ' '), paste0('(', new_zero, ' '), CI95), CI95)) |>
      dplyr::mutate(CI95 = ifelse(substr(N, 1, 1) != '0', gsub(paste0(' ', zero, '\\)'), paste0(' ', new_zero, '\\)'), CI95), CI95)) |>
      dplyr::ungroup()
  }

  result <- result |>
  tidyr::pivot_wider(names_from = group, values_from = c(TOTAL, N, RES, CI95)) |>
  dplyr::select(!!values_names) |>
  dplyr::arrange(match(VAR, var_cat))
 } else {
   if (remove_dec100 == TRUE) {
     dec100 <- stringr::str_flatten(c('100.', rep('0', decimals)))

     result <- result |>
       dplyr::mutate(RES = ifelse(RES == dec100, '100', RES))
   }

  values_names <- c('VAR', expand.grid(var = c('TOTAL', 'N', 'RES'), group = groups) |> dplyr::mutate(var_final = paste0(var, '_', group)) |> dplyr::pull(var_final))

  if (simplified == TRUE) {
   result <- result |>
    dplyr::mutate(M = ifelse(total == 0, 'NC', paste0(roundmath_str(adjust(n, 0), 0), '/', roundmath_str(adjust(total, 0), 0)))) |>
    dplyr::mutate(N = NA_character_)
  } else {
   result <- result |>
    dplyr::mutate(N = ifelse(total == 0, '0', roundmath_str(adjust(total, 0), 0))) |>
    dplyr::mutate(M = ifelse(total == 0, 'NC', roundmath_str(adjust(n, 0), 0)))
  }

  result <- result |>
   dplyr::mutate(total = ifelse(total == 0, '0', roundmath_str(adjust(total, 0), 0))) |>
   dplyr::ungroup() |>
   dplyr::select(group, var, N, M, RES) |>
   dplyr::rename(VAR = var, TOTAL = N) |>
   dplyr::rename(N = M)

  if (zero_to_less == TRUE) {
    zero <- stringr::str_flatten(c('0.', rep('0', decimals)))
    new_zero <- stringr::str_flatten(c('<0.', rep('0', decimals - 1), '1'))

    result <- result |>
      dplyr::rowwise() |>
      dplyr::mutate(RES = ifelse(RES == zero & substr(N, 1, 1) != '0', new_zero, RES)) |>
      dplyr::ungroup()
  }

  result <- result |>
   dplyr::mutate(RES = paste0('(', RES, ')')) |>
   tidyr::pivot_wider(names_from = group, values_from = c(TOTAL, N, RES)) |>
   dplyr::select(!!values_names) |>
   dplyr::arrange(match(VAR, var_cat))
 }

 if (simplified == TRUE) {
  result <- result |>
   dplyr::select(-dplyr::starts_with('TOTAL_'))
 }

 if (dec_sep != '.') {
   result <- result |>
     dplyr::mutate_at(dplyr::vars(-VAR), function(x) gsub("\\.", dec_sep, x))
 }

 return(result)
}
