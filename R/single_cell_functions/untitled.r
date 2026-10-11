function (seurat_obj, Title, KEGG_Pathway, FontSize = 7, PointSize = 1) 
{
    Pathway <- keggGet(KEGG_Pathway)[[1]]
    KEGG_Name <- Pathway$NAME
    KEGG_Name <- gsub(" - .*", "", KEGG_Name)
    KEGG_Name_short <- gsub(" .*", "", KEGG_Name)
    gene_list <- Pathway$GENE
    entrez_ids <- gene_list[seq(1, length(gene_list), by = 2)]
    descriptions <- gene_list[seq(2, length(gene_list), by = 2)]
    gene_symbols <- gsub(";.*", "", descriptions)
    Genes_KEGG <- data.frame(EntrezID = entrez_ids, GeneSymbol = gene_symbols, 
        stringsAsFactors = FALSE)
    Genes_KEGG_inSeurat <- base::intersect(Genes_KEGG$GeneSymbol, 
        rownames(seurat_obj))
    return(Genes_KEGG_inSeurat, KEGG_Name_short)
    Score <- AddModuleScore(object = seurat_obj, features = list(Genes_KEGG_inSeurat), 
        name = KEGG_Name_short)
    mean_expression <- Score@meta.data %>% group_by(donor, gel) %>% 
        summarize(Score = mean(.data[[paste0(KEGG_Name_short, 
            "1")]], na.rm = TRUE), .groups = "drop") %>% select(donor, 
        gel, Score)
    stat.test <- mean_expression %>% dplyr::arrange(donor) %>% 
        pairwise_t_test(Score ~ gel, paired = TRUE) %>% add_significance() %>% 
        add_xy_position(x = "gel", fun = "max") %>% mutate(yMax = y.position * 
        1.1)
    stat.test$p_formatted <- map(stat.test$p, format_pvalue)
    Plot <- local({
        Plot <- mean_expression %>% ggplot(aes(x = gel, y = Score, 
            color = gel)) + geom_line(aes(group = interaction(donor)), 
            color = "black") + geom_point(shape = 21, size = 2, 
            fill = "white") + scale_color_manual(values = Color.Gels) + 
            scale_y_continuous(limit = c(0, NA), expand = expansion(mult = c(0, 
                0.2))) + labs(y = paste(KEGG_Name_short, "(a.u.)")) + 
            theme_fontsize(FontSize) + theme_Layout + theme(plot.title = element_text(hjust = 0.5, 
            face = "bold", size = FontSize), legend.position = "none", 
            axis.title.x = element_blank()) + stat_pvalue_manual(stat.test, 
            y.position = "yMax", label = "{p_formatted}", vjust = -0.25, 
            tip.length = 0, linetype = "blank", size = FontSize/2.835)
    })
    Score@meta.data$PointShape <- "16"
    Plot_UMAP <- FeaturePlot(Score, features = paste0(KEGG_Name_short, 
        "1"), pt.size = PointSize, shape.by = "PointShape") + 
        theme_fontsize(FontSize) + theme_UMAP + scale_shape(guide = "none") + 
        theme(legend.key.width = unit(5, "pt"), legend.box.spacing = unit(10, 
            "pt")) + scale_colour_gradientn(colours = brewer.pal(n = 9, 
        name = "GnBu"), n.breaks = 3) + ggtitle(KEGG_Name_short)
    return(list(Plot = Plot, Plot_UMAP = Plot_UMAP, Metabo_GeneList = Genes_KEGG_inSeurat))
}