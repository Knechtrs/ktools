
maxwell_plotting_function <- function(
    result1, # results from maxwell fitting function
    result2, # results from maxwell fitting function
    df = data_StressRelax, # data with original values
    time = "Time", #column name of data containing time values
    ft_var = "Stress_norm", # column name of data used for fitting
    color_label = "Hematoma", # name in legend for original data
    data_color = "#762A83",
    fit_label1 = "one element",
    fit_color1 =  "orange",
    fit_label2 = "two element",
    fit_color2 = "grey20",
    t_max = params$tMax, # time data cut-off value
    id = "exp" #sample identifier
    ) {
  if (is.null(result1)) return(NULL) # check that result is valid input

  model_type <- result$model_type # extract which model was used

  # Reduce number of points for plotting: faster plotting
  data_fitted_sliced <- result$fitted_data %>%
    group_by(!!sym(id)) %>%
    slice(seq(1, n(), by = floor(n()/100))) %>%
    ungroup()

  # Create parameter labels
  if (model_type == "one") {
    fitted_params_df <- result$parameters %>%
      mutate(label = sprintf("τ = %.1f, A = %.2f", tau, A)) %>%
      select(all_of(id), label)
  } else {
    fitted_params_df <- result$parameters %>%
      mutate(label = sprintf("τ1 = %.1f, τ2 = %.1f\nA1 = %.2f, A2 = %.2f", tau1, tau2, A1, A2)) %>%
      select(all_of(id), label)
  }

  # # Label positions
  # text_positions <- data_fitted_sliced %>%
  #   group_by(id) %>%
  #   summarize(
  #     Time = max(Time) * 0.5,
  #     Stress_predict = min(Stress_predict) + 0.1
  #   )
  #
  # label_positions <- fitted_params_df %>%
  #   left_join(text_positions, by = "id")
  
  # Add a dummy variable for legend grouping
  df$color_label <- color_label
  

  # Create plots
  plot_fitted <- ggplot() +
    # Change geom_path to include mapping for the legend
    geom_path(data = df,
              aes(x = !!sym(time), y = !!sym(fit_var), color = color_label),
              linewidth = 1.5) +
    geom_path(data = data_fitted_sliced,
              aes(x = !!sym(time), y = !!sym(fit_var), color = fit_labels),
              alpha = 1,
              linewidth = 1,
              linetype="11") +
    # geom_text(data = label_positions,
    #           aes(x = Time, y = Stress_predict, label = label),
    #           hjust = 0, vjust = 0, size = 3) +
    facet_wrap(reformulate(id), nrow = 2) +
    # Add scale_color_manual to control the colors and legend
    scale_color_manual(
      name = "", # no title
      values = setNames(c(data_color, fit_colors), c(color_label, fit_labels))
    ) +
    scale_x_continuous(limits=c(0,t_max), expand=expansion(mult=c(0.05,0))) +
    labs(
      x = "Time (s)",
      y = "Normalized stress"
    ) +
    theme_layout +
    theme_fontsize(FontSize)

#   plot_residuals <- ggplot(result$residuals, aes(x = Time, y = residuals)) +
#     geom_point(alpha = 0.5, size = 0.5) +
#     geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
#     facet_wrap(~ id, nrow = 2) +
#     labs(
#       title = paste(ifelse(model_type == "one", "One-Element", "Two-Element"), "Maxwell Model Residuals"),
#       x = "Time",
#       y = "Residuals"
#     ) +
#     theme_Layout +
#     theme_fontsize(8)
# 
#   return(list(
#     plot_fitted = plot_fitted,
#     plot_residuals = plot_residuals
#   ))
# }
# 

 }