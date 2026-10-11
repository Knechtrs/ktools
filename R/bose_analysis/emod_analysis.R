emod_analysis <- function(
    df_data = df_data,
    id = "exp",
    load = "load",
    x_axis = "strain", # x-axis variable of Emod fit plot, alternative: disp
    y_axis = "stress", # x-axis variable of Emod fit plot, alternative: load
    time = "time",
    disp = "disp",
    groupvar = "Alginate",
    gel_height = params$GelProperties$Height,
    gel_r = params$GelProperties$Radius,
    emod_low = params$Emod_low,
    emod_high = params$Emod_high,
    FontSize = params$theme$FontSize
) {
  
  # browser()
  
  # If gel_height is a column name, use that column.
  # If it is numeric, create a constant column.
  if (is.character(gel_height) && gel_height %in% names(df_data)) {
    df_data <- df_data %>%
      mutate(.gel_height = .data[[gel_height]])
  } else if (is.numeric(gel_height) && length(gel_height) == 1) {
    df_data <- df_data %>%
      mutate(.gel_height = gel_height)
  } else {
    stop("gel_height must be either a numeric value or a valid column name.")
  }
  
  # Same for gel_r
  if (is.character(gel_r) && gel_r %in% names(df_data)) {
    df_data <- df_data %>%
      mutate(.gel_r = .data[[gel_r]])
  } else if (is.numeric(gel_r) && length(gel_r) == 1) {
    df_data <- df_data %>%
      mutate(.gel_r = gel_r)
  } else {
    stop("gel_r must be either a numeric value or a valid column name.")
  }
  
  #---- calculate stress and strain ----#
  data_emod <- df_data %>%
    group_by(!!sym(id)) %>%
    mutate(max_time = .data[[time]][which.max(.data[[load]])]) %>%
    filter(!!sym(time) < max_time) %>%
    mutate(
      !!sym(disp) := max(!!sym(disp)) - !!sym(disp),
      strain = !!sym(disp) / .gel_height,
      stress = (!!sym(load)*1e-3 * 9.81 / (pi * (.gel_r*1e-3)^2))/1e3 # convert load in g to kg and mm to m and divide by 1000 to get kPa
    ) %>%
    ungroup()
  
  #---- subset data for strain range ----#
  data_fit_range <- data_emod %>%
    filter(strain >= emod_low & strain <= emod_high)
  
  #---- plot E-modulus range ----#
  plot_EmodFit <- ggplot() +
    geom_line(data = data_emod , aes(x = !!sym(x_axis), y = !!sym(y_axis))) +
    geom_smooth(data = data_fit_range, aes(x = !!sym(x_axis), y = !!sym(y_axis)),
                color = "red", linewidth = 1.2, method = "lm", se = FALSE) +
    facet_wrap(reformulate(id), scales = "free_y") +
    theme_layout +
    theme_fontsize(FontSize) +
    labs(
      x = ifelse(x_axis == "strain", "Strain (%)", "Displacement (mm)"),
      y = ifelse(y_axis == "stress", "Stress (kPa)", "Load (g)")
    ) +
    ggtitle(
      paste0("E-modulus fit between ", emod_low * 100, "% - ", emod_high * 100, "% strain")
    )
  
  #---- calculate modulus ----#
  df_modulus <- data_fit_range %>%
    group_by(!!sym(id), !!sym(groupvar)) %>%
    nest() %>%
    mutate(
      model = map(data, ~ lm(stress ~ strain, data = .x)),
      Emod = map_dbl(model, ~ coef(.x)[["strain"]])
    ) %>%
    select(-data, -model) %>%
    ungroup()
  
  return(list(
    data = df_modulus,
    plot = plot_EmodFit
    ))
}
