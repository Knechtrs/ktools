theme_UMAP <- theme(
                   legend.key.spacing.x=unit(20, "pt"),
                   legend.key.height = unit(10, "pt"),
                   legend.key.width = unit(5, "pt"),
                   plot.title=element_text(hjust=0.5),
                   axis.text=element_blank(), 
                   # axis.title=element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y=element_text(color="white"), # add label to have more space for figure label afterwards
                   axis.ticks=element_blank()
                 )