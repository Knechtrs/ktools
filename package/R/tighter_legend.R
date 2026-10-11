tighter_legend <- function(
    position = "bottom",
    key_spacing_x = -2,
    key_spacing_y = -6,
    text_spacing_l = 0,
    text_spacing_r = 0
) {
    theme(
      legend.position = position,
      legend.box.margin = margin(t=0, b=0, unit = "pt"),
      legend.margin = margin(t = 0, b = 0, r = 0, l = 0, unit = "mm"),
      legend.text = element_text(
        margin = margin(r = text_spacing_r, l = text_spacing_l, unit = "pt")
      ),
      legend.key.spacing.y = unit(key_spacing_y, "pt"),
      legend.key.spacing.x = unit(key_spacing_x, "pt"),
      legend.box.spacing = unit(0, "pt"),
      legend.spacing.x = unit(0, "pt"),
      legend.spacing.y = unit(0, "pt")
    )
}
