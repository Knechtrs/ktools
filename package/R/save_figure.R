save_figure <- function(
    plot,
    filename = NULL,
    width_cm = NA,
    height_cm = NA,
    folder = "Output/Plots",
    file_type = c(".rds", ".png", ".pdf", ".svg"),
    dpi = 600,
    scale = 1
) {
  # Create folder if missing
  if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
  
  if (is.null(filename)) filename <- deparse(substitute(plot))
  
  width_in <- if (!is.na(width_cm)) width_cm / 2.54 else NULL
  height_in <- if (!is.na(height_cm)) height_cm / 2.54 else NULL
  
  for (ft in file_type) {
    filepath <- file.path(folder, paste0(filename, ft))
    
    if (ft == ".rds") {
      saveRDS(plot, file = filepath)
    } else {
      if (ft == ".svg" && !requireNamespace("svglite", quietly = TRUE)) {
        stop("Package 'svglite' required for saving SVG files. Please install it.")
      }
      
      device_fun <- switch(
        ft,
        ".png" = "png",
        ".pdf" = if (capabilities("cairo")) cairo_pdf else "pdf",
        ".svg" = svglite::svglite
      )
      
      if (ft != ".svg") {
        ggsave(filepath, plot = plot, device = device_fun,
               width = width_in, height = height_in, dpi = dpi, scale = scale)
      } else {
        svglite::svglite(filepath, width = width_in, height = height_in, fix_text_size = FALSE)
        print(plot)
        invisible(dev.off())
      }
    }
  }
  
  invisible(file.path(folder, paste0(filename, file_type)))
}
