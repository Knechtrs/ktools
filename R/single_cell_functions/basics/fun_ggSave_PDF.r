# Define function to save plots as PDF using cairo_pdf device
fun_ggSave_PDF <- function(df, w = 18.3, h) {
  # Create file path with date and plot name
  file_path <- paste0(folderName, "/", format(Sys.time(), "%Y-%m-%d_"), deparse(substitute(df)), ".pdf")
  
  # Save plot as PDF with cairo_pdf device to retain text and transparency
  ggsave(filename = file_path, plot = df, width = w, height = h, units = "cm", 
         scale = 1, device = cairo_pdf)
}