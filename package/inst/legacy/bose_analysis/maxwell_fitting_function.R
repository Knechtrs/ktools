# Core fitting function for both models
maxwell_fitting_function <- function(
    df, # dataframe
    time = "Time", #column name of time data
    fit_var = "Load_norm", # column name of data to fit
    model_type = "two" # one or two element model
    ) {
  
  # browser()
  
  # df = list_df[[1]] %>% drop_units(.)
  # time = "Time"
  # fit_var = "Load_norm"
  # model_type = "two"
  
  # Basic data validation: check if time and fit_var is in dataframe
  if (nrow(df) < 3 || !all(c(sym(time), sym(fit_var)) %in% colnames(df))) {
    return(NULL)
  }
  
  # only keep rows with data across
  df <- df[complete.cases(df[, c(time, fit_var)]), ]
  # only for positive time values
  df <- df[df[[time]] > 0, ]
  
  # check if df is not empty afer filtering 
  if (nrow(df) < 3) return(NULL)
  
  # Attempt to fit the appropriate model
  if (model_type == "one") {
    # Try with multiple starting values if needed
    starting_values <- list(
      list(tau = 100, A = 0.5),
      list(tau = 10, A = 0.2),
      list(tau = 500, A = 0.8)
    )
  } else {
    # starting_values <- list(
    #   list(tau1 = 70, tau2 = 1500, A1 = 0.3, A2 = 0.7),
    #   list(tau1 = 200, tau2 = 1000, A1 = 0.4, A2 = 0.5),
    #   list(tau1 = 10, tau2 = 200, A1 = 0.4, A2 = 0.5)
    # )
    starting_values <- list(
      list(tau1 = 1,   tau2 = 100,  A1 = 0.1, A2 = 0.2),
      list(tau1 = 5,   tau2 = 500,  A1 = 0.2, A2 = 0.3),
      list(tau1 = 10,  tau2 = 1000, A1 = 0.2, A2 = 0.4),
      list(tau1 = 50,  tau2 = 2000, A1 = 0.3, A2 = 0.3),
      list(tau1 = 100, tau2 = 5000, A1 = 0.2, A2 = 0.4)
    )
  }
  
  best_fit <- NULL
  best_error <- Inf
  
  for (start_values in starting_values) {
    
    tryCatch({
      
      if (model_type == "one") {
        
        test_fit <- nlsLM(
          formula = as.formula(
            paste(fit_var, "~ maxwell_model_one(", time, ", tau, A)")
          ),
          data = df,
          start = start_values,
          lower = c(0.001, 0.001),
          upper = c(Inf, 1),
          control = nls.lm.control(maxiter = 1000)
        )
        
      } else {
        
        test_fit <- nlsLM(
          formula = as.formula(
            paste(
              fit_var,
              "~ maxwell_model_two(",
              time,
              ", tau1, tau2, A1, A2)"
            )
          ),
          data = df,
          start = start_values,
          lower = c(0, 0, 0, 0),
          upper = c(Inf, Inf, 1, 1),
          control = nls.lm.control(maxiter = 1000)
        )
      }
      
      current_error <- sum(
        (df[[fit_var]] - predict(test_fit))^2
      )
      
      if (current_error < best_error) {
        best_error <- current_error
        best_fit <- test_fit
      }
      
    }, error = function(e) {
      # ignore individual failed starting values
    })
  }
  
  if (is.null(best_fit)) {
    message("Fit failed for all starting values")
    return(NULL)
  }
  
  if (is.null(best_fit)) return(NULL)
  
  # Generate predictions
  # Time_vec <- seq(0.01, t_max, by = 0.01) 
  time_vec <- df[[time]] #use time data from vector
  
  if (model_type == "one") {
    stress_predict <- maxwell_model_one(
      time_vec,
      coef(best_fit)["tau"],
      coef(best_fit)["A"]
    )
  } else {
    stress_predict <- maxwell_model_two(
      time_vec,
      coef(best_fit)["tau1"],
      coef(best_fit)["tau2"],
      coef(best_fit)["A1"],
      coef(best_fit)["A2"]
    )
  }
  
  df_predict <- tibble(!!time := time_vec, !!fit_var := stress_predict)
  residuals <- df[[fit_var]] - predict(best_fit, newdata = df)
  df_residuals <- tibble(!!time := df[[time]], residuals = residuals)
  
  # Calculate R-squared
  ss_total <- sum((df[[fit_var]] - mean(df[[fit_var]]))^2)
  ss_residual <- sum(residuals^2)
  r_squared <- 1 - (ss_residual/ss_total)
  
  rmse <- sqrt(mean(residuals^2))
  aic <- AIC(best_fit)
  bic <- BIC(best_fit)
  
  return(list(
    optimized_params = best_fit,
    df_predict = df_predict,
    df_residuals = df_residuals,
    r_squared = r_squared,
    rss = ss_residual,
    rmse = rmse,
    aic = aic,
    bic = bic,
    n = nrow(df)
  ))
}
