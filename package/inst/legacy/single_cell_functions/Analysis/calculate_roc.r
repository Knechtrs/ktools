# function that takes a list of genes, groups module score expresion of these genes into binary groups and check how well reference gene predicts the expression/ binary groups

# create ROC analysis function
calculate_roc <- function(seurat_obj, gene_set, reference_gene = "CD14") {

  # check if gene_set is not already binary. if TRUE create binary group
  if (length(gene_set) > 1) # check if column name or gene set string is input
      # length(unique(seurat_obj[[gene_set]])) == 2
   {
    
  # Remove reference gene from gene set if included
  gene_set_filtered <- setdiff(gene_set, reference_gene)
  
  # Compute module score on filtered object
  seurat_obj_filtered <- AddModuleScore(
    seurat_obj, 
    features = list(gene_set_filtered), 
    name = "Geneset_score"
  )
  
  # Unsupervised clustering to define binary groups: Module score high and low cells
    gmm_model_ModuleScore <- Mclust(seurat_obj_filtered@meta.data$Geneset_score1, G = 2)

  # plot density plot of GMM model: To check how well binary group identification worked
   plot_gmm_qc <- plotting_gmm_qc(seurat_obj_filtered@meta.data$Geneset_score1, gmm_model_ModuleScore)
         
  # add binary groups modulescore to seurat object meta data
    seurat_obj_filtered$gene_set_score_group <- factor(
      gmm_model_ModuleScore$classification,
      levels = c(1, 2),
      labels = c("low gene score", "high gene score")
    )
} else { 
  # Case 2: gene_set is already a binary vector per cell
    seurat_obj_filtered <- seurat_obj
    seurat_obj_filtered$gene_set_score_group <- seurat_obj[[gene_set]]
    plot_gmm_qc <- NULL  # no clustering needed
  }

  # Extract reference gene expression
    ref_gene_expression <- seurat_obj_filtered@assays$RNA$data[reference_gene, ]


  # Perform ROC analysis
    roc_obj <- roc(seurat_obj_filtered$gene_set_score_group, ref_gene_expression)

   # return objects
    return(list(
        seurat_obj_filtered = seurat_obj_filtered,
        plot_gmm_qc = plot_gmm_qc,
        roc_obj = roc_obj,
        auc_value = auc(roc_obj)
            )
        )
}