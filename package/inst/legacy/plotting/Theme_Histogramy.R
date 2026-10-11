Theme_Histogramy <- function(){ 
  theme(
    plot.background = element_rect(fill = "white"),
    panel.background = element_blank(),
    panel.border = element_rect(color = "black",fill=NA, linewidth = 0.5),
    panel.grid.major.y=element_line(color="black", linewidth = 0.1),
    panel.grid.major=element_line(color="grey90", linewidth=0.1),
    panel.grid.minor=element_line(color="grey90", linewidth=0.1),
    # axis.line=element_line(color="black"),
    axis.line = element_blank(),
    axis.text.x=element_text(angle=0, hjust=1, color="black"),
    axis.text.y= element_text(hjust=1,vjust=0, color="black"),
    axis.ticks.y=element_blank(),
    strip.background = element_blank(),  # Remove gray background
    strip.text = element_text(face = "bold",margin = margin(b = 3, l=10)),  # Make title bold of facet_wraps
    plot.title = element_text(hjust = 0.5, face = "bold"),
    # panel.grid.major.y = element_line(color = "black", linetype = "solid"),  # Add horizontal gridlines
    # panel.spacing = unit(0.8, "lines"), # controls don't make it too small. If smaller is desired, use geom_vline
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.key.size = unit(0.7, "lines"),  # Adjust the size of the legend key
    legend.spacing.y = unit(0.1, "lines"),  # Adjust the spacing between legend items
    plot.margin=unit(c(2,4,2,2),'mm'),#trouble => t,r,b,l)
  )}
