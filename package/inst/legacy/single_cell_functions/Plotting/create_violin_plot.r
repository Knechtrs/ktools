#' Create a violin plot with optional points overlay

# ' @param df           A data.frame or tibble
# ' @param xvar         Name of the categorical x‐axis variable (string)
# ' @param yvar         Name of the numeric y‐axis variable (string)
# ' @param fillvar      OPTIONAL: Name of a variable to map fill color to (string). 
# '                     If NULL, violin fill = xvar.
# ' @param add_points   Logical; if TRUE, adds jittered points on top
# ' @param point_size   Size of the overlaid points
# ' @param colors       A vector of fill colors (one per level of fillvar/xvar)
# ' @param palette      If `colors=NULL`, name of a RColorBrewer palette
# ' @param alpha        Opacity of the violins (0–1)
# ' @param title        Plot title (string)
# ' @param xlab         X‐axis label (string)
# ' @param ylab         Y‐axis label (string)
# ' @param fontsize     Base font size (numeric)
# ' @param base_family  Font family (string, e.g. “” for device default)
# ' @return             A ggplot object

#' create_violin_plot
create_violin_plot <- function(
  df,
  xvar,
  yvar,
  fillvar       = NULL,
  add_points    = TRUE,
  add_pointrange = TRUE,
  point_size    = 1.5,
  colors        = NULL,
  palette       = "Set2",
  alpha         = 0.7,
  title         = NULL,
  xlab          = NULL,
  ylab          = NULL,
  fontsize      = 8,
  base_family   = ""
) {
  # Build aes mapping with tidy eval
  mapping <- aes(
    x    = !!sym(xvar),
    y    = !!sym(yvar),
    fill = if (!is.null(fillvar)) !!sym(fillvar) else !!sym(xvar)
  )
  
  p <- ggplot(df, mapping) +
    geom_violin(alpha = alpha, color = "black", size = 0.3)

  if (add_pointrange) {
    p <- p + 
    stat_summary(
    fun.data = "mean_sdl",  fun.args = list(mult = 1), 
    geom = "pointrange", color = "white"
    )
  }
  
  if (add_points) {
    p <- p + geom_jitter(
      aes(
        x    = !!sym(xvar),
        y    = !!sym(yvar)
      ),
      width = 0.15,
      size  = point_size,
      alpha = 0.6,
      color = "black"
    )
  }
  
  # Apply fill scale
  if (!is.null(colors)) {
    p <- p + scale_fill_manual(values = colors)
  } else {
    p <- p + scale_fill_brewer(palette = palette)
  }
  
  # Labels & theme
  p <- p +
    labs(
      title = title %||% "",
      x     = xlab   %||% xvar,
      y     = ylab   %||% yvar
    ) +
    theme_layout +
    theme_fontsize(fontsize, base_family = base_family)
      
  p
}