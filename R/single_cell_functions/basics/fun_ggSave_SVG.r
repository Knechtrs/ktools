fun_ggSave_SVG <- function(df, w = 18.3, h = 17) {
  # Create file path with date and plot name
  file_path <- paste0(folderName, "/", format(Sys.time(), "%Y-%m-%d_"), deparse(substitute(df)), ".svg")
  
  # Ensure the directory exists
  dir.create(folderName, showWarnings = FALSE, recursive = TRUE)
  
  # Export figure as SVG with Helvetica font
  svglite(file = file_path, width = w, height = h)
          # system_fonts = list(sans = "Arial"))  # Ensure Helvetica is used
  print(df)  # Print the plot to the SVG device
  dev.off()  # Close the SVG device
  
  message("Plot saved as SVG: ", file_path)
}