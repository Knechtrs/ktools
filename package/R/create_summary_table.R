create_summary_table <- function(data, columns, group_columns) {
  
  if (!all(columns %in% names(data))) {
    stop("Some columns for summarizing not found in data.")
  }
  if (!all(group_columns %in% names(data))) {
    stop("Some group columns not found in data.")
  }
  
  data %>%
    group_by(across(all_of(group_columns))) %>%
    summarise(
      across(
        all_of(columns),
        list(
          Mean = ~ mean(.x, na.rm = TRUE),
          SD   = ~ sd(.x, na.rm = TRUE)
        ),
        .names = "{.col}__{.fn}"
      ),
      .groups = "drop"
    ) %>%
    pivot_longer(cols = -all_of(group_columns), names_to = "Variable", values_to = "Value") %>%
    separate(Variable, into = c("Variable", "Statistic"), sep = "__") %>%
    pivot_wider(names_from = Statistic, values_from = Value) %>%
    mutate(
      Mean_SD = case_when(
        abs(Mean) >= 100 ~ sprintf("%.0f \u00b1 %.0f", Mean, SD),
        abs(Mean) >= 10  ~ sprintf("%.1f \u00b1 %.1f", Mean, SD),
        TRUE             ~ sprintf("%.2f \u00b1 %.2f", Mean, SD)
      )
    )
}
