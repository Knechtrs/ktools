# Required packages
library(minpack.lm)
library(dplyr)
library(purrr)
library(tibble)

# General n-element Maxwell model (normalized to 1)
maxwell_model_n <- function(time, ...) {
  par <- list(...)
  n <- length(par) / 2
  A <- unlist(par[1:n])
  tau <- unlist(par[(n + 1):(2 * n)])
  1 - rowSums(sapply(1:n, function(i) A[i] * (1 - exp(-time / tau[i]))))
}

# Generalized fitting function
maxwell_fitting_function_general <- function(
    df,
    time = "Time",
    fit_var = "load_norm",
    model_type = 1,             # number of Maxwell elements (integer)
    scan_models = FALSE,        # if TRUE: tries all from 1 to model_type
    criterion = "AIC"           # model selection: "AIC", "BIC", or "R2"
) {
  
  # browser()
  
  # Check
  if (nrow(df) < 3 || !all(c(time, fit_var) %in% colnames(df))) return(NULL)
  
  # Preprocess
  df <- df[complete.cases(df[, c(time, fit_var)]), ]
  df <- df[df[[time]] > 0, ]
  if (nrow(df) < 3) return(NULL)
  
  max_elements <- as.numeric(model_type)
  range_elements <- if (scan_models) 1:max_elements else max_elements
  
  all_fits <- list()
  
  for (n_elements in range_elements) {
    param_names <- c(paste0("A", 1:n_elements), paste0("tau", 1:n_elements))
    formula_str <- paste0(fit_var, " ~ maxwell_model_n(", time, ", ", paste(param_names, collapse = ", "), ")")
    
    # Create multiple starting guesses
    starting_values_list <- lapply(1:5, function(i) {
      
      taus <- if (n_elements == 1) {
        mean(10, max(df[[time]]))* runif(n_elements, 0.01, 1.2)
      } else {
        seq(from = 10, to = max(df[[time]]), length.out = n_elements) * runif(n_elements, 0.9,1.1)
      }
      
      # taus <- exp(seq(log(min(df[[time]])), log(max(df[[time]]) * 10), length.out = n_elements)) * runif(n_elements, 0.5, 1.5)
      As <- rep(1 / n_elements, n_elements) * runif(n_elements, 0.5, 1.5)
      As <- As / sum(As) * 0.95  # ensure A1 + A2 ... ≤ 1
      setNames(c(As, taus), param_names)
    })
    
    best_fit <- NULL
    best_error <- Inf
    fit_attempts <- 0
    successful_fits <- 0
    
    for (start_values in starting_values_list) {
      fit_attempts <- fit_attempts + 1
      tryCatch({
        fit <- nlsLM(
          formula = as.formula(formula_str),
          data = df,
          start = start_values,
          lower = c(rep(0.0001, n_elements), rep(1, n_elements)),  # A > 0, τ > 1
          upper = c(rep(1, n_elements), rep(50000, n_elements)),         # reasonable caps: A < 1, t = 50'000
          # lower = rep(0.0001, 2 * n_elements),
          # upper = rep(c(10000, 2 * n_elements),
          control = nls.lm.control(maxiter = 1000)
        )
        pred <- predict(fit)
        successful_fits <- successful_fits + 1
        current_error <- sum((df[[fit_var]] - pred)^2)
        if (current_error < best_error) {
          best_fit <- fit
          best_error <- current_error
        }
      }, error = function(e) {})
    }
    
    # Only show message if all attempts failed
    if (successful_fits == 0) {
      patient_id <- if("PatientLetter" %in% colnames(df)) unique(df$PatientLetter) else "Unknown"
      message("All ", fit_attempts, " fitting attempts failed for Donor ", patient_id, ": ", n_elements, " elements")
    }
    
    if (!is.null(best_fit)) {
      params <- coef(fit)
      residuals <- df[[fit_var]] - predict(best_fit)
      df_predict <- tibble(!!time := df[[time]], !!fit_var := predict(best_fit))
      df_residuals <- tibble(!!time := df[[time]], residuals = residuals)
      
      ss_total <- sum((df[[fit_var]] - mean(df[[fit_var]]))^2)
      ss_residual <- sum(residuals^2)
      r_squared <- 1 - (ss_residual / ss_total)
      rmse <- sqrt(mean(residuals^2))
      aic <- AIC(best_fit)
      bic <- BIC(best_fit)
      
      all_fits[[as.character(n_elements)]] <- list(
        n_elements = n_elements,
        params = params,
        optimized_params = best_fit,
        df_predict = df_predict,
        df_residuals = df_residuals,
        r_squared = r_squared,
        rss = ss_residual,
        rmse = rmse,
        aic = aic,
        bic = bic,
        n = nrow(df)
      )
    }
  }
  
  # Compute chi-square tests for nested models if scan_models = TRUE
  
    # helper function
      lrt_chisq <- function(f1, f2, model1, model2) {
          out <- anova(f1, f2)   # ANOVA on nls fits
          
          rss_small <- out$`Res.Sum Sq`[1]
          rss_large <- out$`Res.Sum Sq`[2]
          df_small  <- out$`Res.Df`[1]
          df_large  <- out$`Res.Df`[2]
          
          df_diff   <- df_small - df_large
          rss_diff  <- rss_small - rss_large
          sigma2    <- rss_large / df_large
          
          chi_sq <- rss_diff / sigma2
          p_val  <- pchisq(chi_sq, df = df_diff, lower.tail = FALSE)
          
          tibble(
            comparison = paste0("n=", model1, " vs n=", model2),
            df_diff    = df_diff,
            chi_sq     = chi_sq,
            p_value    = p_val
          )
        }
  
  lrt_table <- NULL
  if (scan_models && length(all_fits) > 1) {
    n_models <- length(all_fits)
    lrt_table <- map2_dfr(1:(n_models-1), 2:n_models, function(k1, k2) {
      f1 <- all_fits[[as.character(k1)]]$optimized_params
      f2 <- all_fits[[as.character(k2)]]$optimized_params
      if (!is.null(f1) && !is.null(f2)) {
        lrt_chisq(f1, f2, k1, k2)
      }
    })
  }
  
  # No successful fits
  if (length(all_fits) == 0) return(NULL)
  
  # Build summary and residuals
  summary_table <- map_dfr(all_fits, ~{
    tibble(
      n_elements = .x$n_elements,
      params = .x$params,
      r_squared  = .x$r_squared,
      rmse       = .x$rmse,
      rss        = .x$rss,
      aic        = .x$aic,
      bic        = .x$bic,
      n_obs      = .x$n
    )
  })
  
  # Create fit data frame with consistent column names
  df_fit <- purrr::compact(all_fits) %>%   # remove NULLs
    purrr::map_dfr(~{
      tibble(
        n_elements = .x$n_elements,
        !!time := .x$df_residuals[[time]],  # Use consistent column naming
        !!paste0(fit_var, "_observed") := df[[fit_var]],  # More descriptive name
        !!paste0(fit_var, "_predicted") := .x$df_predict[[fit_var]],  # Consistent naming
        residuals = .x$df_residuals$residuals
      )
    })
  
    return(list(
      summary_table = summary_table,
      df_fit = df_fit,
      all_fits = all_fits,
      lrt_table = lrt_table
    ))
}
