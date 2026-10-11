create_KEGG_plots <- function(
    result_KEGG    
    ){
    # create paired plot
    Plot_mean <- plot_paired_points(
        result_KEGG$mean_expression,
        xvar= "gel",
        yvar = "Score",
        colorvar = "gel",
        connectVar1 = "donor",
        colors = Color.Gels,
        pointsize = 2,
        fontsize = 8,
        base_family = "" # use default font
        ) +
    labs(y = paste(result_KEGG$KEGG_Name_short, "(a.u.)"))   # Example title and labels

    # add stat to paired plot
    Plot_mean_stat <- add_stat_test_dodge(
        Plot_mean,
        yData = "Score",
        Group = "gel",
        test = "t.test",
        paired = TRUE,
        FontSize = 8)

    # get umap data
    df_result_KEGG <- get_umap_plot_data(result_KEGG$Score, color_meta_data = paste0(result_KEGG$KEGG_Name_short, "1"), color_meta_data_2 = "gel")

    # create umap plot
    Plot_umap <- dimplot_umap(
    df_result_KEGG,
    colorvar = paste0(result_KEGG$KEGG_Name_short, "1"),
    colors = brewer.pal(n = 9, name = "GnBu"),
    continuous_colors = TRUE,
    base_family =""
    ) +
    ggtitle(result_KEGG$KEGG_Name_short) +
    theme_fontsize(8, base_family = "") +
    theme_UMAP +
    theme(legend.position = "right",
         legend.direction = "vertical",
          legend.title = element_blank()
         )

    # create violing plot
    Plot_violin <- create_violin_plot(
        df = df_result_KEGG,
        xvar= "gel",
        yvar = paste0(result_KEGG$KEGG_Name_short, "1"),
        fillvar =  "gel",
        colors = Color.Gels,
        fontsize= 8,
        alpha = 1,
        add_points    = FALSE
    )

    list(
        Plot_mean_stat = Plot_mean_stat,
        Plot_umap = Plot_umap,
        Plot_violin = Plot_violin
        )
}