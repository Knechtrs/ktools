fun_plotting_Histogram <- function(
    data,
    xvar,
    yvar1,
    ColorVar = NULL,
    GroupVar = NULL,
    quantile_lines = TRUE,
    Color = NULL,
    alpha_color = 1,
    alpha_fill = 1,
    LineVar = NULL,
    LineThickness = 1,
    LineCode = NULL,
    title = NULL,
    reverse_y = FALSE
) {
  if (reverse_y) {
    data <- data %>%
      mutate({{yvar1}} := forcats::fct_rev(as_factor({{yvar1}})))
  }
  
  # # Create explicit grouping variable that combines GroupVar with other factors
  has_group <- !rlang::quo_is_null(rlang::enquo(GroupVar))
  has_line  <- !rlang::quo_is_null(rlang::enquo(LineVar))
  
  if (has_group && has_line) {
    data <- data %>%
      dplyr::mutate(.group_var = interaction({{yvar1}}, {{ColorVar}}, {{GroupVar}}, {{LineVar}}, drop = TRUE))
  } else if (has_group) {
    data <- data %>%
      dplyr::mutate(.group_var = interaction({{yvar1}}, {{ColorVar}}, {{GroupVar}}, drop = TRUE))
  } else if (has_line) {
    data <- data %>%
      dplyr::mutate(.group_var = interaction({{yvar1}}, {{ColorVar}}, {{LineVar}}, drop = TRUE))
  } else {
    data <- data %>%
      dplyr::mutate(.group_var = interaction({{yvar1}}, {{ColorVar}}, drop = TRUE))
  }
  
  p <- ggplot(data, aes(
    x = {{xvar}},
    y = {{yvar1}},
    color = {{ColorVar}},
    linetype = {{LineVar}},
    group = .group_var  # Use the explicit grouping variable
  )) +
    ggridges::geom_density_ridges(
      aes(
        height = after_stat(ndensity)
          ),
      fill = NA,
      quantile_lines = quantile_lines,
      quantile_fun = median,
      alpha = alpha_fill,
      scale = 0.8,
      linewidth = LineThickness,
      key_glyph = "path"
    ) +
    scale_x_continuous(limits = c(NA, NA), expand = c(0, 0)) +
    labs(y = NULL, title = title)
  
  if (!is.null(Color)) {
    cols <- scales::alpha(unlist(Color), alpha_color)
    p <- p + scale_color_manual(values = cols)
  } else {
    p <- p + scale_color_brewer(palette = "Set2")
  }
  
  if (!is.null(LineCode)) {
    p <- p + scale_linetype_manual(values = unlist(LineCode))
  }
  
  return(p)
}