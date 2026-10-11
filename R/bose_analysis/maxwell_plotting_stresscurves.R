maxwell_plotting_stresscurves <- function(
    result1 = list_maxwell_results_one,
    result2 = list_maxwell_results_two,
    which_models = c("one", "two", "both"),
    df = data_stressrelax,
    time = "Time",
    fit_var = "Load_norm",
    color_var = NULL,
    color_vec = Color.Gels,
    t_max = NULL,
    slice_it = FALSE,
    slice_points = 100,
    id = "alg_batch",
    facet_scale = "free",
    n_row = NULL,
    linewidth = 1
) {
  
  which_models <- match.arg(which_models)
  
  # optional time filtering
  if (!is.null(t_max)) {
    if (!is.null(result1)) {
      result1$fitted_data <- result1$fitted_data %>%
        dplyr::filter(.data[[time]] <= t_max)
    }
    
    if (!is.null(result2)) {
      result2$fitted_data <- result2$fitted_data %>%
        dplyr::filter(.data[[time]] <= t_max)
    }
    
    df <- df %>%
      dplyr::filter(.data[[time]] <= t_max)
  }
  
  fitted_data_all <- list()
  
  if (which_models %in% c("one", "both") && !is.null(result1)) {
    data1 <- result1$fitted_data %>%
      dplyr::mutate(
        Model = dplyr::recode(
          result1$model_type,
          "one" = "1-element",
          "two" = "2-element"
        )
      )
    fitted_data_all <- append(fitted_data_all, list(data1))
  }
  
  if (which_models %in% c("two", "both") && !is.null(result2)) {
    data2 <- result2$fitted_data %>%
      dplyr::mutate(
        Model = dplyr::recode(
          result2$model_type,
          "one" = "1-element",
          "two" = "2-element"
        )
      )
    fitted_data_all <- append(fitted_data_all, list(data2))
  }
  
  data_fitted_combined <- dplyr::bind_rows(fitted_data_all)
  
  # handle color variable
  if (is.null(color_var)) {
    df <- df %>% dplyr::mutate(.color_plot = "Data")
    color_var_plot <- ".color_plot"
    
    if (is.null(color_vec)) {
      color_vec <- c(Data = "black")
    }
  } else {
    color_var_plot <- color_var
    
    if (!(color_var %in% names(df))) {
      df <- df %>%
        dplyr::mutate(!!rlang::sym(color_var) := color_var)
    }
  }
  
  # optional slicing to reduce plot size
  if (slice_it) {
    df <- df %>%
      dplyr::group_by(.data[[id]]) %>%
      dplyr::slice(unique(round(seq(1, dplyr::n(), length.out = min(slice_points, dplyr::n()))))) %>%
      dplyr::ungroup()
    
    data_fitted_combined <- data_fitted_combined %>%
      dplyr::group_by(.data[[id]], .data[["Model"]]) %>%
      dplyr::slice(unique(round(seq(1, dplyr::n(), length.out = min(slice_points, dplyr::n()))))) %>%
      dplyr::ungroup()
  }
  
  plot_fitted <- ggplot2::ggplot() +
    ggplot2::geom_path(
      data = df,
      ggplot2::aes(
        x = .data[[time]],
        y = .data[[fit_var]],
        color = .data[[color_var_plot]]
      ),
      linewidth = linewidth * 2
    ) +
    ggplot2::geom_path(
      data = data_fitted_combined %>% dplyr::arrange(.data[[time]]),
      ggplot2::aes(
        x = .data[[time]],
        y = .data[[fit_var]],
        linetype = .data[["Model"]]
      ),
      color = "black",
      alpha = 0.7,
      linewidth = linewidth
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(.data[[id]]),
      nrow = n_row,
      scales = facet_scale
    ) +
    ggplot2::scale_color_manual(
      name = "",
      values = unlist(color_vec)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, NA),
      expand = ggplot2::expansion(mult = c(0.05, 0))
    ) +
    ggplot2::scale_linetype_manual(
      values = c("1-element" = 11, "2-element" = "solid")
    ) +
    ggplot2::labs(
      x = "Time (s)",
      y = "Normalized stress"
    ) +
    theme_layout +
    theme_fontsize(FontSize)
  
  return(plot_fitted)
}