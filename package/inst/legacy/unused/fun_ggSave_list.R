fun_ggSave_list <- function (df, PlotName, w, h) 
{
    ggsave(paste0(folderName, "/", format(Sys.time(), "%Y-%m-%d_"), 
        PlotName, ".png"), df, width = w, height = h, dpi = 300, 
        units = "cm")
}
