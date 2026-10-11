make_scientific_flextable <- function(df, caption = NULL, footer = NULL,
                                      font_family = "Arial",
                                      font_size = 11,
                                      digits = 3) {
  
  # Detect numeric columns
  num_cols <- which(sapply(df, is.numeric))
  
  ft <- flextable(df) %>%
    # General font and size
    fontsize(size = font_size, part = "all") %>%
    flextable::font(fontname = font_family, part = "all") %>% 
    
    # Header formatting
    bold(part = "header") %>%
    align(align = "center", part = "header") %>%
    bg(part = "header", bg = "white") %>%  # change to "grey90" if subtle shading preferred
    
    # Body alignment
    align(j = num_cols, align = "right", part = "body") %>%
    align(j = setdiff(seq_along(df), num_cols), align = "left", part = "body") %>%
    
    # Autofit content
    autofit()
  
  # Add caption if provided
  if (!is.null(caption)) {
    ft <- set_caption(ft, caption = caption)
  }
  
  # Add footer notes if provided
  if (!is.null(footer)) {
    ft <- add_footer_lines(ft, values = footer)
  }
  
  return(ft)
}
