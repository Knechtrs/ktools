get_umap_plot_data <- function(
    seurat_obj,
    reduction = "umap",
    color_meta_data = "gel",
    color_meta_data_2  = NULL
) {
  df_umap <- Embeddings(seurat_obj, reduction = "umap") %>% 
    as.data.frame()

  df_umap[[ color_meta_data ]] <- seurat_obj@meta.data[[ color_meta_data ]]

  if (!is.null(color_meta_data_2)) {
    df_umap[[ color_meta_data_2 ]] <- seurat_obj@meta.data[[ color_meta_data_2 ]]
      }
    
  return(df_umap)
}