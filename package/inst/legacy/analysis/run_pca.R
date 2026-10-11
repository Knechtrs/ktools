run_pca <- function(
    df,
    scale_cols = NULL,  # Can be NULL or character vector of column names to group by for scaling
    center = TRUE, # center function in prcomps: sets mean = 0
    scale = FALSE, # scale function in prcomps: sets sd = 1
    color_cols = NULL, # metadata columns to be used for both coloring and annotation
    shape_col = NULL,  # column to map to shape
    fontsize = 8,
    fontsize_loading = fontsize, # adjust only loading fontsize
    pointsize = 3,
    color_values = NULL, # colors for color_cols values,
    shape_values = NULL, # manual shape values
    add_loadings = FALSE, # should loadings be shonw in plot?
    add_points = TRUE, # set FALSE if you only want to plot loadings
    sep_symbol = "_",
    add_group_means = FALSE, # add group means as bigger symbol
    group_mean_pointsize = pointsize*2,
    force = 1, # repulsion between overlapping text labels
    force_pull = 1 # attraction between a text label and its corresponding data point, 
) {
  
  # browser()
  
  # Optional scaling by group:
  if (!is.null(scale_cols)) {
    
    df <- df %>% 
      group_by(across(all_of(scale_cols))) %>%
      mutate(across(where(is.numeric), ~ scale(.x)[,1])) %>%
      ungroup()
  }
  
  # Select numeric columns for PCA
  df_numeric <- df %>% select(where(is.numeric))
  
  # NA check
  if( sum(is.na(df_numeric)) != 0) {
    stop( message("Number of NAs in numeric data: ", sum(is.na(df_numeric))))
  }
  
  #---- run pca ----#
  
  # Run PCA with scaling
  pca_result <- prcomp(df_numeric, center = center, scale. = scale)
  
  summary_output <- summary(pca_result)
  
  # Proportion of variance explained by each component
  var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
  
  #---- create ggplots ----#
  
  # Create a dataframe with PCA scores and original grouping variables
  pca_df <- data.frame(
    PC1 = pca_result$x[,1],
    PC2 = pca_result$x[,2])
  
  if (!is.null(color_cols)) {
    # Safely bind requested metadata columns (if any)
    pca_df <- bind_cols(
      pca_df,
      df %>% select(all_of(color_cols))
    )
  }
  
  # check color_cols input
  if (is.null(color_cols)) {
    stop("Please select at least one column name for colors")
  }
  
  if(length(color_cols) > 1) {
    pca_df <- pca_df %>%
      unite("color_col", all_of(color_cols), sep = sep_symbol, remove = FALSE)
  } else {
    pca_df <- pca_df %>% dplyr::rename("color_col" = color_cols)
  }
  
  if (!is.null(shape_col)) 
    pca_df <- bind_cols(pca_df, df %>% select(all_of(shape_col)))
  
  # calculate mean
  if(add_group_means) {
    group_means <- if (!is.null(shape_col)) {
      pca_df %>%
        group_by(color_col, !!sym(shape_col)) %>%
        summarise(PC1 = mean(PC1), PC2 = mean(PC2), .groups = "drop")
    } else {
      pca_df %>%
        group_by(color_col) %>%
        summarise(PC1 = mean(PC1), PC2 = mean(PC2), .groups = "drop")
    }
  }
  
  # Plot colored by different factors
  base_map  <- aes(PC1, PC2, color = color_col)
  shape_map <- if (!is.null(shape_col)) aes(shape = !!rlang::sym(shape_col)) else NULL
 ellips_map <- aes(group = ellips_col)
  
  plot <- ggplot(pca_df, mapping = base_map) +
    { if (add_points) 
      geom_point(mapping = shape_map, size = pointsize, alpha = 0.7)
      else NULL } +
    { if (isTRUE(add_group_means))
      geom_point(
        data = group_means,
        mapping = shape_map,
        size = group_mean_pointsize,
        stroke = 1.2
      )
      else NULL } +
    labs(
      x = paste0("PC1 (", round(var_explained[1]*100, 1), "%)"),
      y = paste0("PC2 (", round(var_explained[2]*100, 1), "%)")
    ) +
    theme_layout +
    theme_fontsize(fontsize) +
    theme(legend.title = element_blank())
  
  if (!is.null(color_values)) {
    plot <- plot + scale_color_manual(values = color_values, drop = FALSE)
  }
  if (!is.null(shape_values) && !is.null(shape_col)) {
    plot <- plot + scale_shape_manual(values = shape_values, drop = FALSE)
  }
  
  # optional: add loadings
  if (add_loadings) {
    # Get loadings
    loadings_df <- as.data.frame(pca_result$rotation[, 1:2])
    loadings_df$varname <- rownames(loadings_df)
    
    # Rescale arrows
    scale_factor <- max(abs(pca_df$PC1), abs(pca_df$PC2)) * 2
    loadings_df$PC1 <- loadings_df$PC1 * scale_factor
    loadings_df$PC2 <- loadings_df$PC2 * scale_factor
    
    # Add arrows and labels
    plot <- plot +
      geom_segment(
        data = loadings_df,
        aes(x = 0, y = 0, xend = PC1, yend = PC2),
        arrow = arrow(length = unit(0.2, "cm")),
        linewidth = 0.4,
        color = "grey60"
      ) +
      geom_text_repel(
        data = loadings_df,
        aes(x = PC1, y = PC2, label = varname),
        size = fontsize_loading * 0.3528,  # convert pt → mm
        color = "black",
        force = force,
        force_pull = force_pull
      )
  }
  
  return(list(
    df_numeric = df_numeric,
    summary_output = summary_output,
    pca_result = pca_result,
    plot = plot
  ))
  
}