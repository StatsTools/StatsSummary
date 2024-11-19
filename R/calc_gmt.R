#' Calculate GMT function
#'
#' @param data A tibble with the data to be used.
#' @param var_expr A name with the name of the variable in data.
#' @param group_expr A name with the name of the group variable in data.
#' @param method_ci A character with method used to calculate CI. Use 't.test' or 'bootstrap'.
#' @param decimals A number with the number of decimals points to present the results.
#' @param groups_order A character array with the names of the groups in order to be presented in the results.
#' @param nboot A number with the number of replications to be applied if bootstrap is selected.
#' @param ci_sep A character with a separator symbol to be used in CI.
#' @param dec_sep A character to specify the decimal separator to be used.
#' @param base A number with the number to be used as base in the antilogarithm calculation.
#' @param adjust A character with the name of the decimals adjustment function. Use 'roundmath', 'round' or 'trunc'.
#'
#' @return A tibble with the GMT results by group.
#' @export
#'
#' @examples
#' data <- tibble(titer = c(rnorm(100),rbeta(100, 0.2, 0.2)), group = c(rep(1, 100), rep(2, 100)))
#' calc_gmt(data = data, var_expr = titer, group_expr = group, decimals = 2, groups_order = c('2', '1'), method_ci = 't.test', ci_sep = ";", dec_sep = ".", base = exp(1), adjust = 'roundmath')
calc_gmt <- function(data, var_expr, group_expr, method_ci = "t.test", decimals = 1, groups_order = NA_character_, nboot = 1000, ci_sep = '\u2012', dec_sep = ".", base = exp(1), adjust = 'roundmath') {
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
   "`nboot` must be provided." = !is.na(nboot),
   "`nboot` must be numeric." = is.numeric(nboot),
   "`nboot` cannot be an array." = length(nboot) == 1
  )

  stopifnot(
   "`base` must be provided." = !is.na(base),
   "`base` must be numeric." = is.numeric(base),
   "`base` cannot be an array." = length(base) == 1
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

# Development Step ------------------------------------------------------------
  grid_tib <- expand.grid(group = groups, type = c(1, 2, 3, 4))
  values_names <- expand.grid(var = c('N', 'RES', 'CI95'), group = groups) |> dplyr::mutate(var_final = paste0(var, '_', group)) |> dplyr::pull(var_final)

  stopifnot(
    "`method_ci` must be a character." = is.character(method_ci),
    "`method_ci` cannot be an array." = length(method_ci) == 1,
    "`method_ci` must be 't.test' or 'bootstrap'." = method_ci %in% c('t.test', 'bootstrap')
  )

  shapiro_results <- lapply(groups, function(group_name) {
    titer <- data |>
      dplyr::filter(!is.na(!!var_expr)) |>
      dplyr::mutate(group = as.character(!!group_expr)) |>
      dplyr::filter(group == group_name) |>
      dplyr::pull(!!var_expr)

    shapiro_result <- tryCatch(shapiro.test(titer), error = function(e) return(NA_real_))

    return(shapiro_result)
  })
  names(shapiro_results) <- groups

  result <- data |>
    dplyr::filter(!is.na(!!var_expr)) |>
    dplyr::mutate(group = as.character(!!group_expr)) |>
    dplyr::group_by(group) |>
    dplyr::reframe(gmt_val = gmt(!!var_expr, method_ci = method_ci, base = base, nboot = nboot)) |>
    dplyr::group_by(group) |>
    dplyr::mutate(type = seq(1, dplyr::n())) |>
    dplyr::ungroup() |>
    dplyr::right_join(grid_tib, by = c('group', 'type')) |>
    dplyr::mutate(Resultado = rep(c('Nº Participantes', 'Média', 'Limite Inferior', 'Limite Superior'), length(groups))) |>
    dplyr::mutate(gmt_val = ifelse(is.na(gmt_val) & type == 1, 0, gmt_val)) |>
    dplyr::select(-type) |>
    tidyr::pivot_wider(names_from = Resultado, values_from = gmt_val) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      N = roundmath_str(adjust(`Nº Participantes`, 0), 0),
      RES = ifelse(is.na(Média), 'NC', roundmath_str(adjust(Média, decimals), decimals)),
      CI95 = ifelse(is.na(`Limite Inferior`) | is.na(`Limite Superior`), 'NC', paste0('(', roundmath_str(adjust(`Limite Inferior`, decimals), decimals), ' ', ci_sep,' ', roundmath_str(adjust(`Limite Superior`, decimals), decimals), ')'))
    ) |>
    dplyr::ungroup() |>
    dplyr::select(group, N, RES, CI95) |>
    tidyr::pivot_wider(names_from = group, values_from = c(N, RES, CI95)) |>
    dplyr::select(!!values_names)

  attr(result, "normality.test") <- shapiro_results
  attr(result, "ic.method.used") <- method_ci

  if (dec_sep != '.') {
    result <- result |>
      dplyr::mutate_all(function(x) gsub("\\.", dec_sep, x))
  }


  values_names2 <- expand.grid(var = c('Nº Participantes', 'Média', 'Limite Inferior', 'Limite Superior'), group = groups) |> dplyr::mutate(var_final = paste0(var, '_', group)) |> dplyr::pull(var_final)

    raw.value <- data |>
    dplyr::filter(!is.na(!!var_expr)) |>
    dplyr::mutate(group = as.character(!!group_expr)) |>
    dplyr::group_by(group) |>
    dplyr::reframe(gmt_val = gmt(!!var_expr, method_ci = method_ci, base = base, nboot = nboot)) |>
    dplyr::group_by(group) |>
    dplyr::mutate(type = seq(1, dplyr::n())) |>
    dplyr::ungroup() |>
    dplyr::right_join(grid_tib, by = c('group', 'type')) |>
    dplyr::mutate(Resultado = rep(c('Nº Participantes', 'Média', 'Limite Inferior', 'Limite Superior'), length(groups))) |>
    dplyr::mutate(gmt_val = ifelse(is.na(gmt_val) & type == 1, 0, gmt_val)) |>
    dplyr::select(-type) |>
    tidyr::pivot_wider(names_from = Resultado, values_from = gmt_val) |>
    dplyr::select(group,`Nº Participantes`, `Média`, `Limite Inferior`, `Limite Superior`) |>
    tidyr::pivot_wider(names_from = group, values_from = c(`Nº Participantes`, `Média`, `Limite Inferior`, `Limite Superior`)) |>
    dplyr::select(!!values_names2)

  attr(result, "raw.value") <- raw.value

  return(result)
}

gmt <- function(x, method_ci, base, nboot = 1000){
  if (sum(!is.na(x)) < 1) {
    res <- c(sum(!is.na(x)), NA, NA, NA)
  } else if (sum(!is.na(x)) < 2) {
    res <- c(sum(!is.na(x)), base^(x), NA, NA)
  } else if (sd(x, na.rm = T) == 0) {
    res <- base^(rep(mean(x, na.rm = T), 3))
    res <- c(sum(!is.na(x)), res)
  } else {
    if (method_ci == "bootstrap") {
      m1 <- t.test(x)
      x_boot <- NA
      for (i in 1:nboot) {
        x_boot[i] <- mean(sample(x, replace = TRUE))
      }
      res <- c(sum(!is.na(x)), base^(c(m1$estimate, quantile(x_boot, prob = 0.025, na.rm = TRUE), quantile(x_boot, prob = 0.975, na.rm = TRUE))))
    } else if (method_ci == "t.test") {
      m1 <- t.test(x)
      res <- base^(c(m1$estimate,m1$conf.int))
      res <- c(sum(!is.na(x)), res)
    }
  }

  return(res)
}
