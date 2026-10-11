theme_fontsize <- function(
    base_size   = 6,
    base_family = "sans"
) {
  # dynamically adapt margins
  margin_size <- max(5.5, base_size * 1.2)
  
  theme(
    # this first one sets the default for all text,
    # but since we override each element below we repeat family
    text         = element_text(family = base_family, size = base_size),
    axis.title   = element_text(family = base_family, size = base_size),
    axis.text    = element_text(family = base_family, size = base_size),
    plot.title   = element_text(family = base_family, size = base_size),
    strip.text   = element_text(family = base_family, size = base_size),
    legend.text  = element_text(family = base_family, size = base_size),
    legend.title = element_text(family = base_family, size = base_size),
    plot.margin  = margin(
      t = margin_size,
      r = margin_size,
      b = margin_size,
      l = margin_size
    )
  )
}
