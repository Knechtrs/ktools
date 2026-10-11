theme_DotPlot <- theme(
                    plot.title=element_text(hjust=0.5),
                    axis.title=element_blank(),
                    axis.text.x = element_text(angle=45, hjust=1),
                    legend.box.spacing = unit(0, "pt"),
                    legend.justification = "center",
                    legend.position="top",
                    legend.direction="horizontal",
                    legend.title.position="top",
                    # legend.title = element_text(margin = margin(unit(c(t="100", b="100"), "pt"))),
                    # legend.text = element_text(margin = margin(unit(c(l=0.1, r=0.3, b=-100), "pt"))),
                    legend.key.spacing.x = unit(3, "pt"),
                    legend.key.width = unit(15, "pt"),       # Width of each legend key
                    legend.key.height = unit(0.2, "cm"), # Height of each legend key
                    legend.text = element_text(margin = margin(unit(c(l=1, r=1), "pt")))
                 )  