merge_seurat_objects <- function (seurat_list) 
{
    for (i in seq_along(seurat_list)) {
        seurat_list[[i]]$orig.ident <- names(seurat_list)[i]
    }
    merged_seurat <- merge(x = seurat_list[[1]], y = seurat_list[2:length(seurat_list)], 
        add.cell.ids = names(seurat_list))

    merged_seurat <- JoinLayers(merged_seurat)

    return(merged_seurat)
}
