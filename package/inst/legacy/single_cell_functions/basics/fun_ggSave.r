fun_ggSave <- function (df, w, h) 
{
    ggsave(paste0(folderName, "/", format(Sys.time(), "%Y-%m-%d_"), 
        deparse(substitute(df)), ".png"), df, width = w, height = h, 
        dpi = 300, units = "cm")
}
