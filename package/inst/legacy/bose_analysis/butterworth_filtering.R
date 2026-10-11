#---- apply butterworth filter ----#

butterworth_filtering <- function(
    df_data,
    load = "Load",    # name of column with load data (string)
    time = "Time",    # name of column with time data (string)
    id = "exp" # sample identifier
  ) {
  # Define Butterworth filter parameters
  order <- 2 #params$FilterParams$order     # e.g., 4
  cutoff_freq <- 0.1 # params$FilterParams$cutoff  # e.g., 10 Hz
  
  # # Get only stress-relaxation portion of the data
  # df_data <- df_data %>%
  #   group_by(exp) %>%
  #   mutate(
  #     max_time = .data[[time]][which.max(.data[[load]])]
  #   ) %>%
  #   dplyr::filter(.data[[time]] > max_time) %>%
  #   mutate(
  #     !!time := .data[[time]] - max_time  # reset time to zero at max load
  #   ) %>%
  #   ungroup()

# # quickly check data
# print(plot_load_time(data_StressRelax))

library(signal)

# Pad the signal at beginning and end to remove edge artifacts
pad_signal <- function(signal, pad_len = 100) {
  pad_start <- rep(signal[1], pad_len)
  pad_end <- rep(signal[length(signal)], pad_len)
  c(pad_start, signal, pad_end)
}

# Apply the filter with padding and trim the result
apply_butterworth <- function(signal, order, cutoff_freq, fs, pad_len = 100) {
  butter_filter <- butter(order, cutoff_freq, type = "low", fs = fs)
  padded_signal <- pad_signal(signal, pad_len)
  filtered <- filtfilt(butter_filter, padded_signal)
  filtered_trimmed <- filtered[(pad_len + 1):(length(filtered) - pad_len)]
  return(filtered_trimmed)
}

# Apply to data
df_data <- df_data %>%
  mutate(!!sym(load) := as.double(.data[[load]])) %>%
  group_by(!!sym(id)) %>%
  mutate(
    load_BF = apply_butterworth(.data[[load]], order, cutoff_freq, sampling_rate)
  ) %>%
  ungroup()

detach("package:signal", unload = TRUE)

#---- normalize to load ----#
df_data <- df_data %>%
  group_by(!!sym(id)) %>%
  mutate(load_norm = load_BF / first(load_BF)) %>%  # Normalize to load_BF
  # mutate(load_norm = load_BF / max(load_BF, na.rm = TRUE)) %>%  # Normalize to load_BF
  ungroup()

return(df_data)

}
