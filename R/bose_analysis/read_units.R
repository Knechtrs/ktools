read_units <- function(df, unit_mode = "standard") {
  require(dplyr)
  require(stringr)
  require(units)
  
  # Extract the first row which contains unit strings
  unit_row <- df[1, ]
  
  # Remove the unit row from the data
  df_data <- df[-1, ]
  
  # Step 1: Convert character columns that look like numbers to numeric
  df_data <- df_data %>%
    mutate(across(
      where(is.character),
      ~ suppressWarnings({
        # Replace commas with dots to standardize decimal format
        maybe_numeric <- str_replace_all(.x, ",", ".")
        nums <- as.numeric(maybe_numeric)
        
        # Only convert to numeric if at least 95% of values are valid numbers
        if (sum(!is.na(nums)) / length(nums) > 0.95) nums else .x
      })
    ))
  
  # Step 2: Apply units to columns that are numeric and have valid unit strings
  for (col in names(df_data)) {
    # Clean and standardize the unit string
    unit_string <- clean_unit_names(trimws(unit_row[[col]]))
    
    # Only assign units if the column is numeric and the unit string is valid
    if (!is.na(unit_string) && unit_string != "" && is.numeric(df_data[[col]])) {
      tryCatch({
        df_data[[col]] <- set_units(df_data[[col]], unit_string, mode = unit_mode)
      }, error = function(e) {
        warning(sprintf("Skipping unit for column '%s': %s", col, e$message))
      })
    }
  }
  
  return(df_data)
}
