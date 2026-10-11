MyCorrPlot <- function(
    FontSize = 8,
    pch_size = 1,
    cl_size = 8,
    slot_cols = 2,  # number of columns this plot occupies in patchwork
    slot_rows = 1,  # number of rows this plot occupies
    layout_total_cols = 6, # total number of columns in patchwork
    layout_total_rows = 3, # total number of rows in patchwork
    total_width_cm = 18.3, # width of overall figure
    total_height_cm = 17, # height of overall figure
    dpi = 600,
    scale = 1, # scaling factor for the image
    format = "png", # select either png or svg
    output_dir = tempdir()
) {
  
  if (!requireNamespace("corrplot", quietly = TRUE)) stop("Package 'corrplot' is required.")
  if (!requireNamespace("cowplot", quietly = TRUE)) stop("Package 'cowplot' is required.")
  if (!requireNamespace("magick", quietly = TRUE)) stop("Package 'magick' is required.")
  if (format == "svg" && !requireNamespace("svglite", quietly = TRUE)) {
    stop("Package 'svglite' is required for SVG output.")
  }
  
  # Calculate slot size in cm
  slot_width <- total_width_cm * slot_cols / layout_total_cols
  slot_height <- total_height_cm * slot_rows / layout_total_rows
  
  # calculate effective fontsize
  FontSize_effective <- FontSize  / scale
  cl_size_effective <- cl_size  / scale
  
  # Prepare file path and open device
  if (format == "png") {
    tmpfile <- tempfile(fileext = ".png")
    png(tmpfile, width = slot_width, height = slot_height, units = "cm", res = dpi)
  } else if (format == "svg") {
    tmpfile <- file.path(output_dir, "Plot_corr.svg")
    svglite::svglite(tmpfile, width = slot_width / 2.54, height = slot_height / 2.54, fix_text_size = FALSE) # svglite needs inches
  } else {
    stop("Unsupported format. Use 'png' or 'svg'.")
  }
  
  corrplot::corrplot(
    cor_results$r,
    p.mat = cor_results$P,
    method = "circle",
    insig = "label_sig",
    sig.level = c(0.001, 0.01, 0.05),
    tl.col = "black",
    pch.col = "white",
    pch.cex = pch_size,
    tl.cex = FontSize_effective / 10,
    cl.cex = cl_size_effective / 10,
    cl.length = 5,
    order = "AOE",
    diag = FALSE,
    type = "lower",
    col = RColorBrewer::brewer.pal(n = 11, name = "PuOr"),
    mar = c(0, 0, 0, 0)   # <- bump bottom margin a touch if the title gets clipped
  )

  
  dev.off()

  if (format == "png") {
    # Return as ggplot-compatible image
    return(
      ggdraw() +
        theme_void() +
        draw_image(tmpfile,
                   x = 0.5, y = 0.5,
                   hjust = 0.5, vjust = 0.5,
                   width = scale, height = scale)  # Fill patchwork slot, centered
    )
  } else {
    # For SVG, just return the file path (you can read or open it externally)
    message("SVG saved to: ", tmpfile)
    return(tmpfile)
  }
}
