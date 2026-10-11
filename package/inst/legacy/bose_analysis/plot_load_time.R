plot_load_time <- function(
    data=df_data,
    x = "time",
    y1 = "load",
    id = "exp",
    add_bf = FALSE,
    y2 = "load_BF",
    scales = "free",
    linewidth = 1
    ) {
  p <- data %>%
    ggplot() +
    geom_path(aes(x=!!sym(x), y=!!sym(y1)), linewidth=linewidth, alpha=0.7, color="black") + # use geom_path here, b.c connects points in the order they appear in the dataset. Geom_line by x-axis order
    facet_wrap(reformulate(id), scales = scales) +
    theme_layout +
    theme_fontsize(FontSize) +
    theme(strip.background = element_blank())
  
  if(add_bf) {
   p <- p +
     geom_path(aes(x=!!sym(x), y=!!sym(y2)), linewidth=linewidth*0.3, alpha=0.7, color="red")
  }
  
  return(p)
}