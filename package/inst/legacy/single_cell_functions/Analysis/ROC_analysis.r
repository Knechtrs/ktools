#---- perform ROC anylsis for d1 and d2 on TopFeatures_d1 ----#

library(ggsci)

# create function
ROC_analysis <- function(seurat_obj, reference_gene, GeneFeatures) {

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

  # filter out cells that have CD14 = 0 expression
    # get expression
    cd14_expression <- Seurat::GetAssayData(seurat_obj, slot = "data")["CD14", ]
    # Identify cells with CD14 > 0
    cells_to_keep <- names(cd14_expression)[cd14_expression > 0]
    # subest seurat_obj
    seurat_obj_filtered <- subset(seurat_obj, cells = cells_to_keep)    

    # perform roc analysis by donor
    seurat_obj_List <- SplitObject(seurat_obj_filtered, split.by = "donor")
        
    # apply perform_roc_analysis function
    Roc_Results <- purrr::map(seurat_obj_List, function(seurat_obj) {
      calculate_roc(
        seurat_obj, 
        gene_set = GeneFeatures,
        reference_gene = reference_gene
      )
    })

  # Build a data frame for plotting
    roc_df <- imap(Roc_Results, function(roc, donor) {
    data.frame(
      donor = factor(donor, levels= c("A", "B", "C")),
      threshold = roc$roc_obj$thresholds,
      TPR = roc$roc_obj$sensitivities,
      FPR = 1 - roc$roc_obj$specificities,
      AUC = round(roc$auc_value, 3)
        )
     }) %>% bind_rows()
    
  # Create AUC table with ordered donors
    auc_table <- roc_df %>%
      group_by(donor, AUC) %>%
      dplyr::slice(1) %>%  # Keep just one row per donor
      ungroup()
    
    auc_table <- tibble(
      Donor = sort(names(Roc_Results)),
      AUC = round(map_dbl(Roc_Results, ~.x$auc_value), 3)
    ) 

    return(
        list(
            roc_df     = roc_df,
            auc_table  = auc_table,
            Roc_Results       = Roc_Results,
            reference_gene = reference_gene
        )
    )

}