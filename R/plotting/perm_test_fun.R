# perm_test_fun <- function(data, formula, paired = FALSE, n_perm = 10000, ...) {
#   # Handles grouped dfs (dplyr) and ungrouped
#   is_grouped <- dplyr::is_grouped_df(data)
#   
#   if (is_grouped) {
#     group_vars <- dplyr::group_vars(data)
#     
#     nested_data <- data %>%
#       dplyr::group_by(!!!rlang::syms(group_vars)) %>%
#       tidyr::nest()
#     
#     results <- nested_data %>%
#       dplyr::mutate(
#         test_result = purrr::map(data, ~ run_single_test(.x, formula, paired, n_perm))
#       ) %>%
#       # drop groups that yielded 0 rows cleanly
#       dplyr::mutate(test_result = purrr::keep(test_result, ~ nrow(.x) >= 0)) %>%
#       tidyr::unnest(test_result) %>%
#       dplyr::ungroup()
#     
#     return(results)
#   } else {
#     return(run_single_test(data, formula, paired, n_perm))
#   }
# }

perm_test_fun <- function(data, formula, paired = FALSE, n_perm = 10000, ...) {
  # Handles grouped dfs (dplyr) and ungrouped
  is_grouped <- dplyr::is_grouped_df(data)
  
  if (is_grouped) {
    group_vars <- dplyr::group_vars(data)
    
    nested_data <- data %>%
      dplyr::group_by(!!!rlang::syms(group_vars)) %>%
      tidyr::nest() %>%
      dplyr::rename(data_input = data)  # RENAME to avoid conflict
    
    results <- nested_data %>%
      dplyr::mutate(
        test_result = purrr::map(data_input, ~ run_single_test(.x, formula, paired, n_perm))  # USE data_input
      ) %>%
      # drop groups that yielded 0 rows cleanly
      dplyr::mutate(test_result = purrr::keep(test_result, ~ nrow(.x) >= 0)) %>%
      tidyr::unnest(test_result) %>%  # NOW data only exists in test_result
      dplyr::select(-data_input) %>%  # Remove the input data column if not needed
      dplyr::ungroup()
    
    return(results)
  } else {
    return(run_single_test(data, formula, paired, n_perm))
  }
}

# Always returns a tibble (possibly 0 rows). Never returns NULL.
run_single_test <- function(dat, formula, paired = FALSE, n_perm = 10000) {
  required_packages <- c("dplyr", "tidyr", "coin", "rlang", "purrr", "tibble")
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("Package", pkg, "needed for this function to work. Please install it."))
    }
  }
  
  response_var <- all.vars(formula)[1]
  group_var    <- all.vars(formula)[2]
  id_col       <- "ID_Col"
  
  # Helper: create standardized empty result tibble
  empty_out <- tibble::tibble(
    .y. = character(), group1 = character(), group2 = character(),
    n1 = integer(), n2 = integer(),
    statistic = numeric(), p = numeric(), p.adj = numeric(),
    p.format = character(), p.signif = character(), method = character(),
    data = list()
  )
  
  # levels present (after dropping NAs)
  lvls <- unique(stats::na.omit(dat[[group_var]]))
  nlv  <- length(lvls)
  if (nlv < 2) {
    # nothing to compare
    return(empty_out)
  }
  
  # Inner runner that returns ONE row tibble for a two-level comparison
  run_two_level <- function(df2, g1, g2) {
    create_output <- function(p_value, stat_value = NA, method_name = "Permutation test") {
      p_signif <- if (is.na(p_value)) "ns" else
        if (p_value < 0.001) "***" else
          if (p_value < 0.01)  "**"  else
            if (p_value < 0.05)  "*"   else
              if (p_value < 0.1)   "."   else "ns"
      
      tibble::tibble(
        .y. = response_var,
        group1 = g1,
        group2 = g2,
        n1 = sum(!is.na(df2[df2[[group_var]] == g1, response_var])),
        n2 = sum(!is.na(df2[df2[[group_var]] == g2, response_var])),
        statistic = stat_value,
        p = p_value,
        p.adj = p_value,
        p.format = format.pval(p_value, digits = 3),
        p.signif = p_signif,
        method = method_name,
        data = list(df2)  # keep for yMax downstream
      )
    }
    
    if (paired) {
      if (!id_col %in% names(df2)) {
        warning("Paired permutation test requires an ID column. Please provide 'id'.")
        return(empty_out)
      }
      
      # wide for difference + complete pairs
      paired_w <- df2 %>%
        dplyr::select(!!rlang::sym(id_col), !!rlang::sym(group_var), !!rlang::sym(response_var)) %>%
        tidyr::pivot_wider(names_from = !!rlang::sym(group_var), values_from = !!rlang::sym(response_var))
      
      if (!all(c(g1, g2) %in% names(paired_w))) return(empty_out)
      
      diff_vec <- paired_w[[g1]] - paired_w[[g2]]
      if (all(is.na(diff_vec))) return(create_output(NA, NA, "Paired permutation test (failed)"))
      
      obs_stat <- mean(diff_vec, na.rm = TRUE)
      
      df_complete <- df2 %>%
        dplyr::group_by(!!rlang::sym(id_col)) %>%
        dplyr::filter(n() == 2) %>%
        dplyr::ungroup()
      
      if (nrow(df_complete) < 4) {
        return(create_output(NA, NA, "Paired permutation test (insufficient data)"))
      }
      
      f <- stats::as.formula(paste(response_var, "~", group_var, "|", id_col))
      ct <- coin::oneway_test(
        formula = f,
        data = df_complete,
        distribution = coin::approximate(nresample = n_perm),
        alternative = "two.sided"
      )
      p_val <- as.numeric(coin::pvalue(ct))
      return(create_output(p_val, obs_stat, "Paired permutation test"))
    } else {
      # Independent samples permutation via coin
      f <- stats::as.formula(paste(response_var, "~", group_var))
      # Use mean difference as readable stat
      obs_stat <- mean(df2[df2[[group_var]] == g1, response_var], na.rm = TRUE) -
        mean(df2[df2[[group_var]] == g2, response_var], na.rm = TRUE)
      
      ct <- coin::oneway_test(
        formula = f,
        data = df2,
        distribution = coin::approximate(nresample = n_perm),
        alternative = "two.sided"
      )
      p_val <- as.numeric(coin::pvalue(ct))
      return(create_output(p_val, obs_stat, "Independent permutation test"))
    }
  }
  
  # If exactly 2 levels → single comparison
  if (nlv == 2) {
    g1 <- lvls[1]; g2 <- lvls[2]
    df2 <- dat %>% dplyr::filter(.data[[group_var]] %in% c(g1, g2))
    return(run_two_level(df2, g1, g2))
  }
  
  # If >2 levels → do pairwise across all combinations
  pairs <- utils::combn(lvls, 2, simplify = FALSE)
  out   <- purrr::map_dfr(pairs, function(pr) {
    g1 <- pr[[1]]; g2 <- pr[[2]]
    df2 <- dat %>% dplyr::filter(.data[[group_var]] %in% c(g1, g2))
    run_two_level(df2, g1, g2)
  })
  
  # Always return tibble
  out
}