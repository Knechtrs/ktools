maxwell_main_analysis <- function(
    df = data_stressrelax,
    t_max = params$tMax,
    model_type = "two",
    id = id, # sample identifier. Analysis is performed per sample 
    time = "Time", # time column name
    fit_var = "Load_norm" # column name of data to fit
  ){
  
# Clean and split data by experimental groups and name list items
  list_df <- df %>%
    droplevels() %>%
    group_by(across(all_of(id))) %>%
    filter(!!sym(time) <= .env$t_max) # ensure fit is only until t_max

  names_list <- group_keys(list_df)[[1]]  # assumes `id` is a single column

  list_df <- group_split(list_df)         # now split
  names(list_df) <- names_list            # assign names


# Apply fitting to each dataset:  uses custom maxwell_fitting_function
list_fitted <- lapply(list_df, function(df) {
  maxwell_fitting_function(
    df, #%>% drop_units(.),
    time = time,
    fit_var = fit_var,
    model_type = model_type
  )
})


# Extract parameters
params_fitted <- map_df(list_fitted, ~ coef(.x$optimized_params), .id = id)

# add total relaxation column to two element model
if(model_type == "two") {
  params_fitted <- params_fitted %>%
    mutate(
      total_relaxation = A1 + A2
      # RelaxationPercentage = TotalRelaxation * 100
    )
}

# Get fitted data and residuals
data_fitted <- map_df(list_fitted, ~ .x$df_predict, .id = id)
data_residuals <- map_df(list_fitted, ~ .x$df_residuals, .id = id)

# extract single values
extract_values <- function(list, value) {
  var_sym <- sym(value)
  
  df <- map_df(list, ~ tibble(!!value := .x[[value]]), .id = id) %>%
    rename_with(~ paste0(value, "_", model_type), .cols = all_of(value))
  
  return(df)
}

data_r_squared <- extract_values(list_fitted, "r_squared")
data_aic <- extract_values(list_fitted, "aic")
data_bic <- extract_values(list_fitted, "bic")
data_rss <-  extract_values(list_fitted, "rss")
data_rmse <- extract_values(list_fitted, "rmse")


return(list(
  parameters = params_fitted,
  fitted_data = data_fitted,
  residuals = data_residuals,
  r_squared = data_r_squared,
  aic =  data_aic,
  bic = data_bic,
  rss = data_rss,
  rmse = data_rmse,
  n = nrow(df),
  model_fits = list_fitted,
  model_type = model_type
))

}
