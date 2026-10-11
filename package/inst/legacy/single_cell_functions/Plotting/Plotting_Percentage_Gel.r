Plotting_Percentage_Gel <- function(seurat_obj) {  
    seurat_obj@meta.data %>%
    ggplot(aes(x =binary_groups_Marker)) +
    geom_bar(aes(fill = gel), position = "fill") + 
    facet_wrap(~ timepoint) +
    scale_fill_manual(values= Color.Gels) +
    scale_y_continuous(limits=c(0,1), expand=c(0,0)) +
    theme_fontsize()+
    theme_layout +
    theme(legend.position ="bottom", legend.direction="horizontal", axis.title.x=element_blank(), legend.title=element_blank())
    }
