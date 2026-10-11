# Function to create a single volcano plot without labels
create_volcano_plot <- function(data, title, Pointsize=1, Fontsize=10, FC_Limit = 0.25, pValue_limit = 0.05) {
  suppressWarnings({
    
    # Add a column for color based on conditions
    data <- data %>%
      mutate(color = ifelse(abs(avg_log2FC) > FC_Limit & p_val_adj < pValue_limit, "black", "grey"))

    # Find y-min (or max of -log10(p_val_adj))
    yMin <- min(data$p_val_adj)
      
       # Replace 0 with a very small number if yMin is 0
    if (yMin == 0) {
      yMin <- 10^(-320)
    } else {
        yMin
    }
    
    # Create plot
    ggplot(data = data, aes(x = avg_log2FC, y = -log10(p_val_adj), color = color)) +
      geom_point(alpha = 0.7, size = Pointsize) +
      scale_color_identity() + # Uses the exact colors set in the 'color' column

      # Add horizontal and vertical threshold lines
      geom_hline(yintercept = -log10(pValue_limit), linetype = "dashed", color = "grey") +
      geom_vline(xintercept = c(-FC_Limit, FC_Limit), linetype = "dashed", color = "grey") +
      
      # Set axis limits and scales
      scale_y_continuous(limits = c(0, -log10(yMin)), expand = expansion(mult = c(0, 0.4)), oob = scales::oob_squish_infinite) +
      scale_x_continuous(limits = c(-3, 3)) +
      
      # Custom annotations
      annotate("text", x = -1.9, y = 1.3*(-log10(yMin)), label = "Fast", size = Fontsize/2.835, hjust = 0.5, color = "#4393C3") +
      annotate("text", x = 1.9, y = 1.3*(-log10(yMin)), label = "Slow", size = Fontsize/2.835, hjust = 0.5, color = "#D6604D") +

    # Add segments using annotate with a single y value
    annotate("segment", x = -2.6, xend = -1.2, y = 1.2*(-log10(yMin)), color = "#4393C3") +
    annotate("segment", x = 2.6, xend = 1.2, y = 1.2*(-log10(yMin)), color = "#D6604D") +      

      labs(y="-log10(adj. p value)", x= "log2 fold change") +
  
      # Add title
      ggtitle(title)  +
      theme_fontsize(Fontsize)
  })
  
}
