log_slice <- function(data, group_col, points = 100, min_index = 1) {
  # Inner function to generate log-spaced indices
  log_slice_indices <- function(n, points, min_index) {
    log_seq <- round(exp(seq(log(min_index), log(n), length.out = points)))
    unique(log_seq[log_seq <= n])
  }
  
  # Apply slicing per group
  data %>%
    group_by(across(all_of(group_col))) %>%
    group_modify(~ .x[log_slice_indices(nrow(.x), points, min_index), ]) %>%
    ungroup()
}
