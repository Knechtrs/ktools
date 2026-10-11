roc_plotting <- function(roc_df, timepoint, gene, table_plot) {
    
  # Define color mapping
  donor_colors <- c(
    "A" = "#4DBBD5FF",
    "B" = "#00A087FF",
    "C" = "#3C5488FF"
  )

  # create roc plot
    roc_plot <- roc_df %>%
    ggplot(aes(x = FPR, y = TPR, color = donor)) +
      geom_line(size = 1.2) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0,1)) +
      scale_x_continuous(expand = expansion(mult = c(0, 0))) +
      scale_color_manual(values = donor_colors) +
      theme_fontsize(8) +
      theme_layout +
      theme(
          plot.title = element_text(hjust=0.5),
        legend.position = "none",
        axis.title = element_text(size = 8),
          # plot.margin = unit(c(0.05, 0.05, 0.05, 0.05), "npc")
      ) +
      labs(x = "False Positive Rate (FPR)", 
           y = "True Positive Rate (TPR)") +
    ggtitle(paste(gene, " ", timepoint))

    # get table
    auc_table <- roc_df %>%
        group_by(donor, AUC) %>%
        select(donor, AUC) %>%
        unique() %>%
        ungroup() %>%
        arrange(donor) 
    
  # Create the tableGrob with matched colors
   table_plot <- tableGrob(
      auc_table,
      rows = NULL,
      theme = ttheme_minimal(
        base_size = 10,
        core = list(
          fg_params = list(
            fontface = c(rep("bold", nrow(auc_table)), rep("plain", nrow(auc_table))),
            col = c(
              unname(donor_colors[as.character(auc_table$donor)]),
              rep("black", nrow(auc_table))
            )
          ),
          bg_params = list(fill = "transparent", col = NA)
        ),
        padding = unit(c(1, 1), "mm")
      )
    )

    # Combine plot and table
    Plot_ROC_curves <- roc_plot + 
      inset_element(
        table_plot, 
        left = 0.7, 
        right = 0.9, 
        bottom = 0.3, 
        top = 0.4, 
        align_to = "panel"
      )

    return(Plot_ROC_curves = Plot_ROC_curves)
    }