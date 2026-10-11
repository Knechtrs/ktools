change_signif_fontsize <- function(plot, fontsize = 8, object = "p_value") {
  # search terms:
  if(object == "p_value") {
    search_cols <- c("p.signif", "p.adj", "p.adj.signif", "npcx")
  } else if(object == "npc") {
    search_cols <- c("npcx", "npcy")
  }
 
  # Initialize layer_index to NULL
  layer_index <- NULL
  found_coll <- NULL
  
  # Find the layer containing p.signif
  for (i in seq_along(plot$layers)) {
    
    layer_data <- plot$layers[[i]]$data
    
    # See which of the search_cols actually appears, if any
    hit <- intersect(search_cols, colnames(layer_data))
    if (length(hit) > 0) {
      layer_index <- i
      found_col <- hit[1]   # if multiple match, just take the first
      break
    }
  }
  
  if (is.null(layer_index)) {
    message("No layer with p.signif / p.adj / p.adj.signif found")
    return(plot)
  }
  
  # Find the layer containing p.signif
  # for (i in seq_along(plot$layers)) {
  #   
  #   layer_data <- plot$layers[[i]]$data
  #   
  #   # Check if "p.signif" exists in the column names
  #   if ("p.signif" %in% colnames(layer_data)) {
  #     layer_index <- i
  #     break  # Exit the loop once found
  #   }
  # }
  # 
  # # Check if we found a layer with p.signif
  # if (is.null(layer_index)) {
  #   message("No layer with p.signif found")
  #   return(plot)  # Return original plot unchanged
  # }
  # 
  message("Found p-values in layer ", layer_index)
  
  # Modify the font size in the appropriate parameter
  if(object == "p_value") {
    plot$layers[[layer_index]]$aes_params$label.size <- fontsize/2.54
  } else if(object == "npc") {
    plot$layers[[layer_index]]$aes_params$size <- fontsize/2.54
  }
  
  
  # Return the modified plot
  return(plot)
}