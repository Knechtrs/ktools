# 
# # Create a function to calculate linear regression for each group
# calc_regression <- function(data, xvar, yvar) {
#   # Convert column names to symbols
#   x <- rlang::ensym(xvar)
#   y <- rlang::ensym(yvar)
#   
#   # Create formula dynamically
#   formula <- as.formula(paste0(rlang::as_string(y), " ~ ", rlang::as_string(x)))
#   
#   # Fit model
#   model <- lm(formula, data = data)
#   
#   # Extract R² and p-value
#   r_squared <- summary(model)$r.squared
#   p_value <- summary(model)$coefficients[2, 4]
#   
#   # Return result as data.frame
#   return(data.frame(
#     R2 = round(r_squared, 2),
#     p_value = round(p_value, 3),
#     label = paste0(
#       "R² = ", round(r_squared, 2),
#       "\np = ", ifelse(p_value < 0.001, "< 0.001", round(p_value, 3))
#     )
#   ))
# }

calc_regression <- function(data, xvar, yvar,
                            multiline = TRUE,          # TRUE -> 2 lines, FALSE -> 1 line
                            sep_one = "  |  ",         # separator when multiline = FALSE
                            digits_r2 = 2, digits_p = 3
) {
  x <- rlang::ensym(xvar)
  y <- rlang::ensym(yvar)
  
  fml <- stats::as.formula(paste0(rlang::as_string(y), " ~ ", rlang::as_string(x)))
  fit <- stats::lm(fml, data = data)
  s   <- summary(fit)
  
  r2 <- round(s$r.squared, digits_r2)
  p  <- s$coefficients[2, 4]
  p_txt <- ifelse(is.na(p), NA_character_,
                  ifelse(p < 10^(-digits_p), paste0("< ", formatC(10^(-digits_p), format="f", digits = digits_p)),
                         formatC(p, format = "f", digits = digits_p)))
  
  # human-friendly label
  label <- if (multiline) {
    paste0("R² = ", r2, "\n", "p = ", p_txt)
  } else {
    paste0("R² = ", r2, sep_one, "p = ", p_txt)
  }

  data.frame(
    R2 = r2,
    p_value = p,
    label = label,
    stringsAsFactors = FALSE
  )
}
