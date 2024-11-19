#' Calculate GMTR function
#'
#' @param data A tibble with the data to be used.
#' @param var_expr A name with the name of the variable in data.
#' @param group_expr A name with the name of the group variable in data.
#' @param method_ci A character with method used to calculate CI. Use 't.test' or 'bootstrap'.
#' @param decimals A number with the number of decimals points to present the results.
#' @param groups_order A character array with the names of the groups in order to be used in GMTR calculation. GMTR = First group / Second Group.
#' @param nboot A number with the number of replications to be applied if bootstrap is selected.
#' @param ci_sep A character with a separator symbol to be used in CI.
#' @param dec_sep A character to specify the decimal separator to be used.
#' @param base A number with the number to be used as base in the antilogarithm calculation.
#' @param adjust A character with the name of the decimals adjustment function. Use 'roundmath', 'round' or 'trunc'.

#' @return A tibble with the GMTR results.
#' @export
#'
#' @examples
#' data <- tibble(titer = c(rnorm(100),rbeta(100, 0.2, 0.2)), group = c(rep(1, 100), rep(2, 100)))
#' calc_gmtr(data = data, var_expr = titer, group_expr = group, groups_order = c('2', '1'), method_ci = 't.test', decimals = 2, ci_sep = '\u2012', dec_sep = ".", base = exp(1), adjust = 'roundmath')
calc_gmtr <- function(data, var_expr, group_expr, method_ci = "t.test", decimals = 1, groups_order, nboot = 1000, ci_sep = '\u2012', dec_sep = ".", base = exp(1), adjust = 'roundmath') {
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
  "`dec_sep` must be provided." = !is.na(dec_sep),
  "`dec_sep` must be character." = is.character(dec_sep),
  "`dec_sep` cannot be an array." = length(dec_sep) == 1
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


 if (is.factor(data |> dplyr::pull(!!group_expr))) {
  intern.groups <- levels(data |> dplyr::pull(!!group_expr))
 } else {
  intern.groups <- data |> dplyr::pull(!!group_expr) |> unique() |> as.character()
 }
 stopifnot(
  "`groups_order` must be a character." = is.character(groups_order),
  "Groups are missing in `groups_order`." = length(groups_order) == length(intern.groups),
  "Groups are missing in `groups_order`." = all(sort(groups_order) == sort(intern.groups)),
  "Number of groups must be two." = length(groups_order) == 2
 )
 intern.groups <- groups_order

  stopifnot(
   "`method_ci` must be a character." = is.character(method_ci),
   "`method_ci` cannot be an array." = length(method_ci) == 1,
   "`method_ci` must be 't.test' or 'bootstrap'." = method_ci %in% c('t.test', 'bootstrap')
  )

# Development Step ------------------------------------------------------------
  shapiro_results <- sapply(intern.groups, function(group_name) {
   titer <- data |>
    dplyr::filter(!is.na(!!var_expr)) |>
    dplyr::mutate(group = as.character(!!group_expr)) |>
    dplyr::filter(group == group_name) |>
    dplyr::pull(!!var_expr)

   shapiro_result <- tryCatch(shapiro.test(titer), error = function(e) return(NA_real_))

   return(shapiro_result)
  })
  names(shapiro_results) <- intern.groups

 titer1 <- data |>
  dplyr::filter(!is.na(!!var_expr)) |>
  dplyr::mutate(group = as.character(!!group_expr)) |>
  dplyr::filter(group == intern.groups[1]) |>
  dplyr::pull(!!var_expr)

 titer2 <- data |>
  dplyr::filter(!is.na(!!var_expr)) |>
  dplyr::mutate(group = as.character(!!group_expr)) |>
  dplyr::filter(group == intern.groups[2]) |>
  dplyr::pull(!!var_expr)

 gmtr_val <- gmtr(titer1, titer2, method_ci = method_ci, nboot = nboot, base = base)

 result <- tibble::tibble(
  N1 = roundmath_str(adjust(gmtr_val[1], 0), 0),
  N2 = roundmath_str(adjust(gmtr_val[2], 0), 0),
  RES = ifelse(is.na(gmtr_val[3]), 'NC', roundmath_str(adjust(gmtr_val[3], decimals), decimals)),
  CI95 = ifelse(is.na(gmtr_val[4]) | is.na(gmtr_val[5]), 'NC', paste0('(', roundmath_str(adjust(gmtr_val[4], decimals), decimals), ' ', ci_sep,' ', roundmath_str(adjust(gmtr_val[5], decimals), decimals), ')'))
 ) %>%
  rename_at(vars(N1), function(x) paste0('N_',intern.groups[1])) %>%
  rename_at(vars(N2), function(x) paste0('N_',intern.groups[2]))

 attr(result, "normality.test") <- shapiro_results
 attr(result, "ic.method.used") <- method_ci

 names(gmtr_val) <- c(paste0('N_',intern.groups[1]), paste0('N_',intern.groups[2]),
                      'GMTR', 'LInf', 'LSup')
 attr(result, "raw.value") <- gmtr_val

 if (dec_sep != '.') {
  result <- result |>
    dplyr::mutate_all(function(x) gsub("\\.", dec_sep, x))
 }

 return(result)
}

gmtr <- function(x, y, method_ci, base, nboot = 1000){
 if (sum(!is.na(x)) < 1 | sum(!is.na(y)) < 1) {
  res <- c(sum(!is.na(x)), sum(!is.na(y)), NA, NA, NA)
 } else if (sum(!is.na(x)) < 2 & sum(!is.na(y)) < 2) {
  res <- c(sum(!is.na(x)), sum(!is.na(y)), base^(x) - base^(y), NA, NA)
 } else {
   if (method_ci == "bootstrap") {
     m1 <- t.test(x, y , paired = FALSE)
     x_boot <- NA
     y_boot <- NA
   for (i in 1:nboot) {
    x_boot[i] <- mean(sample(x, replace = TRUE), na.rm = TRUE)
    y_boot[i] <- mean(sample(y, replace = TRUE), na.rm = TRUE)
   }
   res_boot <- x_boot - y_boot
   res <- c(sum(!is.na(x)), sum(!is.na(y)), base^(c(m1$estimate[1] - m1$estimate[2], quantile(res_boot, prob = 0.025, na.rm = TRUE), quantile(res_boot, prob = 0.975, na.rm = TRUE))))
  } else if (method_ci == "t.test") {
   m1 <- t.test(x, y)
   res <- base^(c(m1$estimate[1] - m1$estimate[2], m1$conf.int))
   res <- c(sum(!is.na(x)), sum(!is.na(y)), res)
  }
 }

 return(res)
}
