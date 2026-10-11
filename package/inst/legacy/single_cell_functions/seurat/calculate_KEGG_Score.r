calculate_KEGG_Score <- function(seurat_obj, Title = NULL, KEGG_Pathway, FontSize = 7, PointSize = 1) {
  # Load pathway information
  Pathway <- keggGet(KEGG_Pathway)[[1]]
  KEGG_Name <- gsub(" - .*", "", Pathway$NAME)
  KEGG_Name_short <- gsub(" .*", "", KEGG_Name)

  # Build gene list
  gene_list <- Pathway$GENE
  entrez_ids <- gene_list[seq(1, length(gene_list), by = 2)]
  descriptions <- gene_list[seq(2, length(gene_list), by = 2)]
  gene_symbols <- gsub(";.*", "", descriptions)
  Genes_KEGG <- data.frame(EntrezID = entrez_ids,
                           GeneSymbol = gene_symbols,
                           stringsAsFactors = FALSE)
  Genes_KEGG_inSeurat <- intersect(Genes_KEGG$GeneSymbol, rownames(seurat_obj))

  # Score and summarize
  Score <- AddModuleScore(object = seurat_obj,
                          features = list(Genes_KEGG_inSeurat),
                          name = KEGG_Name_short)

    mean_expression <- Score@meta.data %>%
        group_by(donor, gel) %>%
        dplyr::summarize(
          Score = mean(.data[[paste0(KEGG_Name_short, "1")]], na.rm = TRUE),
          .groups = "drop"
        )
    
    list(
        mean_expression = mean_expression,
        Score = Score,
        Genes_KEGG_inSeurat = Genes_KEGG_inSeurat,
        KEGG_Name_short = KEGG_Name_short
        )
}
