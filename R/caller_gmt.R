#' Calculate GMT
#'
#' @param data A tibble with the data to be used.
#' @param titer_expr A name with the name of the titer variable in data.
#' @param group_expr A name with the name of the group variable in data.
#' @param force_ci A character to force method used to calculate CI. Use 't.test' or 'bootstrap'.
#' @param decimals A number with the number of decimals points to present the results.
#' @param groups_order A character array with the names of the groups in order to be presented in the results.
#' @param nboot The number of replications to be applied if bootstrap is selected.
#'
#' @return A tibble with the GMT results by group.
#' @export
#'
#' @examples
#' data <- tibble(titer = c(rnorm(100),rbeta(100, 0.2, 0.2)), group = c(rep(1, 100), rep(2, 100)))
#' caller_gmt(data, titer, group, decimals = 2, groups_order = c('2', '1'))
caller_gmt <- function(data, titer_expr, group_expr, force_ci = NA_character_, decimals = 1, groups_order = NA_character_, nboot = 1000) {
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

  grid_tib <- expand.grid(group = groups, type = c(1, 2, 3, 4))
  values_names <- expand.grid(var = c('N', 'RES', 'CI95'), group = groups) |> dplyr::mutate(var_final = paste0(var, '_', group)) |> dplyr::pull(var_final)

  if (!is.na(force_ci)) {
    stopifnot(
      "`force_ci` must be a character." = is.character(force_ci),
      "`force_ci` cannot be an array." = length(force_ci) == 1,
      "`force_ci` must be 't.test' or 'bootstrap'." = force_ci %in% c('t.test', 'bootstrap')
    )

    ic_t <- if (force_ci == 't.test') TRUE else FALSE
    shapiro_results <- NA
  } else {
    shapiro_results <- sapply(groups, function(group_name) {
      titer <- data |>
        dplyr::filter(!is.na(!!titer_expr)) |>
        dplyr::mutate(group = as.character(!!group_expr)) |>
        dplyr::filter(group == group_name) |>
        dplyr::pull(!!titer_expr)

      shapiro_result <- tryCatch(shapiro.test(titer)$p.value >= 0.05, error = function(e) return(NA_real_))

      return(shapiro_result)
    })
    names(shapiro_results) <- groups

    ic_t <- if (is.na(all(shapiro_results))) FALSE else all(shapiro_results)
  }

  ic_method_used <- if (ic_t == TRUE) 't.test' else 'bootstrap'

  result <- data |>
    dplyr::filter(!is.na(!!titer_expr)) |>
    dplyr::mutate(group = as.character(!!group_expr)) |>
    dplyr::group_by(group) |>
    dplyr::reframe(gmt_val = gmt(!!titer_expr, boot = ic_t, nboot = nboot)) |>
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
      N = round1_str(`Nº Participantes`, 0),
      RES = ifelse(is.na(Média), 'NC', round1_str(Média, decimals)),
      CI95 = ifelse(is.na(`Limite Inferior`) | is.na(`Limite Superior`), 'NC', paste0('(', round1_str(`Limite Inferior`, decimals), ' \u2012 ', round1_str(`Limite Superior`, decimals), ')'))
    ) |>
    dplyr::ungroup() |>
    dplyr::select(group, N, RES, CI95) |>
    tidyr::pivot_wider(names_from = group, values_from = c(N, RES, CI95)) |>
    dplyr::select(!!values_names)

  attr(result, "normality.test") <- shapiro_results
  attr(result, "ic.method.used") <- ic_method_used

  return(result)
}

gmt <- function(x, boot, nboot = 1000){
  if (sum(!is.na(x)) < 1) {
    res <- c(sum(!is.na(x)), NA, NA, NA)
  } else if (sum(!is.na(x)) < 2) {
    res <- c(sum(!is.na(x)), exp(x), NA, NA)
  } else if (sd(x, na.rm = T) == 0) {
    res <- exp(rep(mean(x, na.rm = T), 3))
    res <- c(sum(!is.na(x)), res)
  } else {
    if (boot) {
      m1 <- t.test(x)
      x_boot <- NA
      for (i in 1:nboot) {
        x_boot[i] <- mean(sample(x, replace = TRUE))
      }
      res <- c(sum(!is.na(x)), exp(c(m1$estimate, quantile(x_boot, prob = 0.025, na.rm = TRUE), quantile(x_boot, prob = 0.975, na.rm = TRUE))))
    } else {
      m1 <- t.test(x)
      res <- exp(c(m1$estimate,m1$conf.int))
      res <- c(sum(!is.na(x)), res)
    }
  }

  return(res)
}
