clean_unit_names <- function(unit_vec) {
  unit_vec <- trimws(unit_vec)  # Remove leading/trailing whitespace
  
  # Fix common abbreviations or typos
  unit_vec <- dplyr::recode(
    unit_vec,
    "Sec" = "s",         # seconds
    "sec" = "s",
    "Secs" = "s",
    "min" = "min",       # keep as-is
    "gr" = "g",          # grams
    "Gram" = "g",
    "Gramm" = "g",
    "mm." = "mm",        # remove trailing period
    "cm." = "cm",
    "°C" = "degC",       # degrees Celsius for units package
    .default = unit_vec  # Keep others unchanged
  )
  
  return(unit_vec)
}
