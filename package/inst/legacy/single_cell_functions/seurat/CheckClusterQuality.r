CheckClusterQuality <- function (seurat_obj) 
{
    PlotDim_on(40, 40)
    
    Plots <- wrap_plots(
        FeaturePlot(seurat_obj, "nCount_RNA", label = T),
        VlnPlot(seurat_obj, "nCount_RNA", pt.size = 0), 
        FeaturePlot(seurat_obj, "nFeature_RNA", label = T),
        VlnPlot(seurat_obj,"nFeature_RNA", pt.size = 0),
        FeaturePlot(seurat_obj,"percent.mt", label = T),
        VlnPlot(seurat_obj, "percent.mt", pt.size = 0),
        FeaturePlot(seurat_obj, "percent.ribo", label = T),
        VlnPlot(seurat_obj, "percent.ribo", pt.size = 0),
            FeaturePlot(seurat_obj, "S.Score", label = T),
        VlnPlot(seurat_obj, "S.Score", pt.size = 0),
        nrow = 6) + 
    plot_layout(widths = c(1, 3)) & theme(legend.position = "none")
    
    return(Plots)
    
    PlotDim_off()
}
