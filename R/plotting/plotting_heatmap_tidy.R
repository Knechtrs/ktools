plotting_heatmap_tidy <- function(
    df,
    row_var,
    col_var,
    value_var,
    BrewerColor = NULL, # default: "PuOr"
    rev_color = TRUE,
    ClusterRows = FALSE,
    ClusterColumns = FALSE,
    show_column_dend = FALSE,
    show_row_dend = FALSE,
    fontsize = 8,
    font_size_row = fontsize,
    font_size_col = fontsize,
    fontsize_anno = fontsize,
    col_angle = 90,
    legend_title = NULL,
    color_breaks = 11,
    auto_palette = TRUE, # automatically switches between diverging and sequential data
    symmetric_color_scale = FALSE, # should color legend be symetric?
    annotation_vars = NULL,  # optional tile annotations: make sure order is the same as in col_var or row_var!
    annotation_palettes = NULL, # color palette for annotation
    annotation_target = "column", # NEW: "column" or "row" - which axis to annotate
    row_order = NULL, # option to manually define row order
    col_order = NULL, # option to manually define col order
    show_legend = TRUE,
    show_annotation_title = TRUE
    
) {
  
  # browser()
  
  # Load required packages if not already loaded
  required_packages <- c("dplyr", "rlang", "tidyHeatmap", "ComplexHeatmap", "circlize", "grid", "RColorBrewer", "tidyr")
  for(pkg in required_packages) {
    if(!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("Package '", pkg, "' is needed for this function to work. Please install it."))
    }
  }
  
  # NULL coalescing operator definition
  `%||%` <- function(x, y) if (is.null(x)) y else x
  
  # Create strings from the NSE vars for safer evaluation
  row_var_str <- deparse(substitute(row_var))
  col_var_str <- deparse(substitute(col_var))
  value_var_str <- deparse(substitute(value_var))
  
  # Validate annotation_target parameter
  if (!annotation_target %in% c("column", "row")) {
    stop("annotation_target must be either 'column' or 'row'")
  }
  
  # Check if columns exist
  if (!value_var_str %in% names(df)) {
    stop(paste("Column", value_var_str, "not found in dataframe"))
  }
  if (!row_var_str %in% names(df)) {
    stop(paste("Column", row_var_str, "not found in dataframe"))
  }
  if (!col_var_str %in% names(df)) {
    stop(paste("Column", col_var_str, "not found in dataframe"))
  }
  
  # Get the numeric values for color scaling - use string names to be safer
  values_numeric <- df[[value_var_str]]
  value_min <- min(values_numeric, na.rm = TRUE)
  value_max <- max(values_numeric, na.rm = TRUE)
  
  # Optionally make scale symmetric around 0
  if (symmetric_color_scale) {
    max_abs <- max(abs(value_min), abs(value_max))
    value_min <- -max_abs
    value_max <- max_abs
  }
  
  # Set color palette based on data characteristics
  if (auto_palette) {
    if (value_min < 0 && value_max > 0) {
      BrewerColor <- BrewerColor %||% "PuOr"  # diverging
    } else {
      BrewerColor <- BrewerColor %||% "Blues" # sequential
    }
  } else {
    BrewerColor <- BrewerColor %||% "Blues"  # Default
  }
  
  # Create color ramp function
  if(rev_color) {
    my_color_ramp <- circlize::colorRamp2(
      breaks = seq(value_min, value_max, length.out = color_breaks),
      colors = grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(
        min(11, color_breaks), BrewerColor)))(color_breaks)
    )
  } else {
    my_color_ramp <- circlize::colorRamp2(
      breaks = seq(value_min, value_max, length.out = color_breaks),
      colors = grDevices::colorRampPalette(RColorBrewer::brewer.pal(
        min(11, color_breaks), BrewerColor))(color_breaks)
    )
  }

  # If annotations are requested, prepare the data first
  base_df <- df
  if (!is.null(annotation_vars)) {
    # Create a unique ID for joining back later
    base_df <- base_df %>% 
      dplyr::mutate(._row_id = dplyr::row_number())
    
    # Choose which variable to split based on annotation_target
    target_var_str <- if (annotation_target == "column") col_var_str else row_var_str
    
    # Extract metadata from the target variable
    metadata_df <- base_df %>%
      tidyr::separate_wider_delim(
        col = !!rlang::sym(target_var_str),
        delim = "_", 
        names = annotation_vars,
        too_few = "align_start",  # Handle IDs with fewer segments
        too_many = "drop"         # Handle IDs with more segments
      )
    
    # Join metadata back to the original dataframe
    for (annot_var in annotation_vars) {
      if (annot_var %in% colnames(metadata_df)) {
        base_df[[annot_var]] <- metadata_df[[annot_var]]
      }
    }
  }
  
  # If user supplies a manual order, coerce the column key to that factor order
  if (!is.null(col_order)) {
    base_df[[col_var_str]] <- factor(base_df[[col_var_str]], levels = col_order)
    ClusterColumns <- FALSE  # ensure clustering doesn't override manual order
  }
  
  
  # Create the main heatmap with explicit strings for more robust evaluation
  legend_breaks <- seq(
    floor(value_min),
    ceiling(value_max),
    by = 1
  )
  
  Plot <- tidyHeatmap::heatmap(
    base_df,
    .row = !!rlang::sym(row_var_str),
    .column = !!rlang::sym(col_var_str),
    .value = !!rlang::sym(value_var_str),
    column_title = NULL,
    row_title = NULL,
    scale = "none",
    row_order = row_order,
    column_order = col_order,
    cluster_rows = ClusterRows,
    row_dend_reorder = ClusterRows,
    show_row_dend = show_row_dend,
    cluster_columns = ClusterColumns,
    show_column_dend = show_column_dend,
    column_dend_reorder = ClusterColumns,
    col = my_color_ramp,
    row_names_gp = grid::gpar(fontsize = font_size_row),
    show_row_names = (font_size_row > 0),
    column_names_gp = grid::gpar(fontsize = font_size_col),
    show_column_names = (font_size_col > 0),
    column_names_rot = col_angle,
    show_heatmap_legend = show_legend,
    heatmap_legend_param = list(
      title = legend_title %||% value_var_str,
      title_position = "leftcenter-rot",
      at = legend_breaks,
      labels = legend_breaks,
      labels_gp = grid::gpar(fontsize = fontsize),
      title_gp = grid::gpar(fontsize = fontsize, fontface = "plain"),
      grid_width = grid::unit(1, "mm"),
      legend_height = grid::unit(18, "mm")
    )
  )
  

  # Add annotations to the plot based on annotation_target
  if (!is.null(annotation_vars)) {
    for (annot_var in annotation_vars) {
      if (annot_var %in% colnames(base_df)) {
        this_palette <- annotation_palettes[[annot_var]] %||% NULL
        
        # Enforce factor order before adding annotation
        if (is.factor(base_df[[annot_var]])) {
          base_df[[annot_var]] <- forcats::fct_inorder(base_df[[annot_var]])
        }
        
        # Plot <- tidyHeatmap::annotation_tile(
        #   Plot,
        #   !!rlang::sym(annot_var),
        #   palette = this_palette,
        #   size = grid::unit(fontsize*1.2, "pt"),   # height of the annotation stripe itself
        #   annotation_name_gp = grid::gpar(fontsize = fontsize_anno)
        #   # show_legend = show_legend,
        #   # # size of the legend keys (symbols)
        #   # simple_anno_size = grid::unit(fontsize_anno, "pt")
        # )
        
        Plot <- tidyHeatmap::annotation_tile(
          Plot,
          !!rlang::sym(annot_var),
          palette = this_palette,
          size = grid::unit(fontsize, "pt"),                  # height of the annotation strip
          annotation_name_gp = grid::gpar(fontsize = fontsize_anno),
          show_legend = show_legend,
          
          # size of the legend keys (colored squares)
          simple_anno_size = grid::unit(fontsize_anno, "pt"),
          
          # control annotation legend text + symbol size
          annotation_legend_param = list(
            labels_gp  = grid::gpar(fontsize = fontsize_anno),        # label text
            title_gp   = grid::gpar(fontsize = fontsize_anno),        # "Density", "Alginate"
            grid_width = grid::unit(fontsize_anno, "pt"),       # width of squares
            grid_height= grid::unit(fontsize_anno, "pt")        # height of squares
          )
        )
        
        
      }
    }
  }
  
  return(Plot)
}