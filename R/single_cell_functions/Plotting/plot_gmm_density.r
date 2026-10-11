### create function to plot gmm_density plot to visualize cut-off of module score  ###


plot_gmm_density <- function(seurat_obj, score_col, group_col) {
  df <- seurat_obj@meta.data
  
  # Calculate thresholds from score column
  mean_threshold <- mean(df[[score_col]], na.rm = TRUE)
  median_threshold <- median(df[[score_col]], na.rm = TRUE)
  
  ggplot(df, aes(x = !!sym(score_col), fill = !!sym(group_col))) +
    geom_density(alpha = 0.5) +
    geom_density(inehrit.aes = FALSE, aes(x = !!sym(score_col)), color = "black", fill = "black", alpha = 0.4) +
    geom_vline(xintercept = mean_threshold, color = "black", linetype = "dotted", size = 1) +
    geom_vline(xintercept = median_threshold, color = "black", linetype = "dotted", size = 1) +
    annotate("text", x = mean_threshold, y = Inf, label = "Mean", angle = 90, vjust = 1.5, color = "black") +
    annotate("text", x = median_threshold, y = Inf, label = "Median", angle = 90, vjust = -1, color = "black") +
    labs(x = score_col, y = "Density") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    theme_layout +
    theme_fontsize()
}