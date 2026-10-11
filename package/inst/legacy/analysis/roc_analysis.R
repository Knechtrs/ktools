#---- perform ROC anylsis for d1 and d2 on TopFeatures_d1 ----#

# create function
roc_analysis <- function(df, predict_var, response_var, donor_var) {
  
  required_pkgs <- c("ggsci", "pROC", "gridExtra", "patchwork")
  for (pkg in required_pkgs) {
    # require() will return FALSE if the package is not installed,
    # or if loading fails. We ask it to load quietly.
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      stop(sprintf(
        "Package '%s' is required but not installed or could not be loaded.\n",
        pkg
      ))
    }
  }
  
  # browser()
  
  # calculate roc 
  roc_results <- df %>%
    group_by(Donor) %>%
    group_modify(~ {
      roc_obj <- roc(.x[[response_var]], .x[[predict_var]])
      tibble(
        auc = as.numeric(roc_obj$auc),
        roc_obj = list(roc_obj)
      )
    })
  
  # only extract necessary data for lighter rds file
  roc_df <- roc_results %>% 
    select(!roc_obj)
  
  #---- create donor colors ----#

  # arrange according to auc value
  roc_df <- roc_df %>% 
    arrange(desc(auc)) %>%
    ungroup() %>%
    mutate(
      Donor_letter = factor(LETTERS[row_number()], levels = LETTERS[1:nrow(.)])
    )
  
  # Use only positions 5-9 (darkest colors)
  bg_darkest <- brewer.pal(9, "PuRd")[4:9]
  donor_colors <- setNames(
    colorRampPalette(rev(bg_darkest))(length(unique(roc_df[[donor_var]]))), 
    unique(roc_df[[donor_var]])
  )
  
  # get lightweight df for plot
  df_plot <- roc_results %>%
    mutate(
      roc_data = map(roc_obj, ~ tibble(
        FPR = 1 - .x$specificities,
        TPR = .x$sensitivities
      ))
    ) %>%
    select(-roc_obj) %>% 
    unnest(roc_data)
    
  # plot result
  roc_plot <- df_plot %>%
    ggplot(aes(x = FPR, y = TPR, color = Donor)) +
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
         y = "True Positive Rate (TPR)") 
    # ggtitle(paste(gene, " ", timepoint))

 
  
  auc_table <- tibble(roc_df %>% 
                        select(Donor_letter, auc) %>% 
                        mutate(auc = round(auc, 2)) %>% 
                        rename("Donor" = Donor_letter)
                      )
  
  
  # Create the tableGrob with matched colors
  table_plot <- tableGrob(
    auc_table,
    rows = NULL,
    theme = ttheme_minimal(
      base_size = 8,
      core = list(
        fg_params = list(
          fontface = c(rep("bold", nrow(auc_table)), rep("plain", nrow(auc_table))),
          col = c(donor_colors, rep("black", each = length(names(roc_df))))  # Color first column, black second
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
  
  return(list(
    Plot_ROC_curves = Plot_ROC_curves,
    roc_df = roc_df,
    roc_results = roc_results
    ))
    
}