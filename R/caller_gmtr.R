#' Calculate GMTR
#'
#' @param data A tibble with the data to be used.
#' @param titer_expr A name with the name of the titer variable in data.
#' @param group_expr A name with the name of the group variable in data. Number of groups must be two.
#' @param force_ci A character to force method used to calculate CI. Use 't.test' or 'bootstrap'.
#' @param decimals A number with the number of decimals points to present the results.
#' @param groups_order A character array with the names of the groups in order to be used in GMTR calculation. GMTR = First group / Second Group.
#' @param nboot The number of replications to be applied if bootstrap is selected.
#'
#' @return A tibble with the GMTR results.
#' @export
#'
#' @examples
#' data <- tibble(titer = c(rnorm(100),rbeta(100, 0.2, 0.2)), group = c(rep(1, 100), rep(2, 100)))
#' caller_gmtr(data, titer, group, groups_order = c('2', '1'), decimals = 2)
caller_gmtr <- function(data, titer_expr, group_expr, groups_order, force_ci = NA_character_, decimals = 1, nboot = 1000) {
 titer_expr <- substitute(titer_expr)
 group_expr <- substitute(group_expr)

 stopifnot("`data` must be a tibble." = tibble::is_tibble(data))

 stopifnot(
  "`titer_expr` must be a name." = is.name(titer_expr),
  "`titer_expr` is missing in data." = as.character(titer_expr) %in% names(data)
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

 if (!is.na(force_ci)) {
  stopifnot(
   "`force_ci` must be a character." = is.character(force_ci),
   "`force_ci` cannot be an array." = length(force_ci) == 1,
   "`force_ci` must be 't.test' or 'bootstrap'." = force_ci %in% c('t.test', 'bootstrap')
  )

  ic_t <- if (force_ci == 't.test') TRUE else FALSE
  shapiro_results <- NA
 } else {
  shapiro_results <- sapply(intern.groups, function(group_name) {
   titer <- data |>
    dplyr::filter(!is.na(!!titer_expr)) |>
    dplyr::mutate(group = as.character(!!group_expr)) |>
    dplyr::filter(group == group_name) |>
    dplyr::pull(!!titer_expr)

   shapiro_result <- tryCatch(shapiro.test(titer)$p.value >= 0.05, error = function(e) return(NA_real_))

   return(shapiro_result)
  })
  names(shapiro_results) <- intern.groups

  ic_t <- if (is.na(all(shapiro_results))) FALSE else all(shapiro_results)
 }

 ic_method_used <- if (ic_t == TRUE) 't.test' else 'bootstrap'

 titer1 <- data |>
  dplyr::filter(!is.na(!!titer_expr)) |>
  dplyr::mutate(group = as.character(!!group_expr)) |>
  dplyr::filter(group == intern.groups[1]) |>
  dplyr::pull(!!titer_expr)

 titer2 <- data |>
  dplyr::filter(!is.na(!!titer_expr)) |>
  dplyr::mutate(group = as.character(!!group_expr)) |>
  dplyr::filter(group == intern.groups[2]) |>
  dplyr::pull(!!titer_expr)

 gmtr_val <- gmtr(titer1, titer2, boot = ic_t, nboot = nboot)

 result <- tibble::tibble(
  RES = ifelse(is.na(gmtr_val[1]), 'NC', round1_str(gmtr_val[1], decimals)),
  CI95 = ifelse(is.na(gmtr_val[2]) | is.na(gmtr_val[3]), 'NC', paste0('(', round1_str(gmtr_val[2], decimals), ' \u2012 ', round1_str(gmtr_val[3], decimals), ')'))
 )

 attr(result, "normality.test") <- shapiro_results
 attr(result, "ic.method.used") <- ic_method_used

 return(result)
}

gmtr <- function(x, y, boot, nboot = 1000){
 if (sum(!is.na(x)) < 1 | sum(!is.na(y)) < 1) {
  res <- c(NA, NA, NA)
 } else if (sum(!is.na(x)) < 2 & sum(!is.na(y)) < 2) {
  res <- c(exp(x) - exp(y), NA, NA)
 } else {
  if (boot) {
   m1 <- t.test(x, y , paired = FALSE)
   x_boot <- NA
   y_boot <- NA
   for (i in 1:nboot) {
    x_boot[i] <- mean(sample(x, replace = TRUE), na.rm = TRUE)
    y_boot[i] <- mean(sample(y, replace = TRUE), na.rm = TRUE)
   }
   res_boot <- x_boot - y_boot
   res <- c(exp(c(m1$estimate[1] - m1$estimate[2], quantile(res_boot, prob = 0.025, na.rm = TRUE), quantile(res_boot, prob = 0.975, na.rm = TRUE))))
  } else {
   m1 <- t.test(x, y)
   res <- exp(c(m1$estimate[1] - m1$estimate[2], m1$conf.int))
  }
 }

 return(res)
}
