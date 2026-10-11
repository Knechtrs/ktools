dimplot_umap <- function(
    df,
    colorvar = "gel",
    colors = Color.Gels,
    continuous_colors = FALSE,
    pointsize = 0.1,
    fontsize = 8,
    facet = FALSE,
    facet_var = NULL,
    strip.position = "top",
    base_family = "Arial"
    ){
    
    # randomize points
    df <- df[sample(nrow(df)), ]

    # create plot
    p <- ggplot(df, aes(x = umap_1, y = umap_2, color = !!sym(colorvar))) +
        geom_point(size = pointsize, shape = 16) +
        theme_fontsize(fontsize, base_family) +
        theme_layout +
        theme_UMAP +
        guides(color = guide_legend(override.aes = list(size = 4))) +
        theme(
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.key.spacing.x = unit(20, "pt"),
            legend.key.spacing.y = unit(10, "pt"),
            legend.justification = "center",
            legend.box.spacing = unit(10, "pt")
        )

    if (continuous_colors) {
        p <- p +  scale_color_gradientn(colours = colors) +
        guides(color = guide_colorbar()
      )
    } else {
       p <- p +  scale_color_manual(values = colors)
    }

    if (facet) {
        p <- p + facet_wrap(reformulate(facet_var), strip.position = strip.position)
    }
    
    return(p)
}