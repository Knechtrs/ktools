# tidyheatmap takes df in pivot_longer format

Plotting_heatmap_tidy <- function(df, UniqueID, Row, Column, Value, TextSize) {
  library(dplyr)
  library(tidyr)
  library(circlize)
  library(grid)
  library(tidyHeatmap)
  library(RColorBrewer)
  
  # Capture the unquoted column names for row, column, and value
  Row <- enquo(Row)
  Column <- enquo(Column)
  Value <- enquo(Value)
  
  # Create a color ramp based on the numeric values in the Value column.
  # Use pull() to extract the numeric vector.
  my_values <- df %>% pull(!!Value)
  my_color_ramp <- colorRamp2(
    breaks = seq(-max(abs(my_values), na.rm = TRUE), max(abs(my_values), na.rm = TRUE), length = 11),
    colors = rev(brewer.pal(11, "PuOr"))
  )
  
  # Combine the columns specified in UniqueID into a new column "unique_id".
  # UniqueID is expected to be a character vector of column names.
  df <- df %>% unite("unique_id", !!!syms(UniqueID), remove = FALSE)
  
  # Create the heatmap using tidyHeatmap's heatmap() function.
  df %>%
    tidyHeatmap::heatmap(
      .row = !!Row,
      .column = !!Column,
      .value = !!Value,
      column_title = NULL,
      row_title = NULL,
      scale = "none",
      cluster_rows = FALSE,
      cluster_columns = TRUE,
      show_row_dend = FALSE,
      show_column_dend = FALSE,
      # row_dend_reorder = FALSE,
      # column_dend_reorder = FALSE,
      # row_order = sortedLevels_Markers,  # Ensure sortedLevels_Markers is defined in your environment.
      col = my_color_ramp,
      row_names_gp = grid::gpar(fontsize = TextSize),
      column_names_gp = grid::gpar(fontsize = TextSize),
      show_column_names = TRUE,
      heatmap_legend_param = list(
        title = "z-score",
        legend_gp = grid::gpar(fontsize = TextSize),
        labels_gp = grid::gpar(fontsize = TextSize),
        title_position = "topcenter",
        title_gp = grid::gpar(fontsize = TextSize, fontface = "plain"),
        direction = "vertical"
      ),
      annotation_legend_param = list(
        nrow= 1      
        )
    )
}
