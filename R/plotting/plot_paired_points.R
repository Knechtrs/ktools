plot_paired_points <- function(
    data,
    xvar = NULL,
    yvar,
    yaxis_trans = NULL, # e.g. ln or log
    fillvar = NULL, # variable for fill color
    colorvar = NULL, # variable for color. Shouldn't be NULL!
    shape_var = NULL, # define column for shape
    shape_values = c(21, 24), # shape symbols
    connectVar1, # variable for connecting: within dodge
    connectVar2 = NULL, # variable 2 for connecting: ensures not across group
    use_dodge = FALSE, # set TRUE, if dodging
    Group = NULL, # variable for dodge
    dodge_width = 0.5,
    colors = "grey",
    fill_colors = colors,
    ylimits = c(0, NA),
    expand_lower_y_mult = 0,
    fontsize = FontSize,
    pointsize = PointSize,
    linewidth = LineWidth,
    facet = NULL, # variable for faceting or set to NULL
    facet_scales = "free",
    nrow_facets = NULL,
    base_family = "Arial"
) {
  
  require(ggplot2)
  require(dplyr)
  require(rlang)
  
  # If xvar is NULL, use a constant "All"
  if (is.null(xvar)) {
    data <- data %>%
      mutate(x_dummy = "All")
    
    xvar <- "x_dummy"
  }
  
  # If colorvar is NULL, use constant
  if (is.null(colorvar)) {
    data <- data %>%
      mutate(color_dummy = "all")
    
    colorvar <- "color_dummy"
  }
  
  # If Group is NULL, use color variable
  if (is.null(Group)) {
    Group <- colorvar
  }
  
  # If fillvar is NULL, use constant
  if (is.null(fillvar)) {
    data <- data %>%
      mutate(fill_dummy = "all")
    
    fillvar <- "fill_dummy"
  }
  
  
  # X-axis levels
  x_values <- pull(data, !!sym(xvar))
  
  if (is.factor(x_values)) {
    x_levels <- levels(droplevels(x_values))
  } else {
    x_levels <- unique(as.character(x_values))
  }
  
  
  # Positioning
  if (use_dodge) {
    
    first_level <- levels(
      droplevels(
        as.factor(
          pull(data, !!sym(Group))
        )
      )
    )[1]
    
    data <- data %>%
      mutate(
        x_dodge = as.numeric(
          factor(
            as.character(!!sym(xvar)),
            levels = x_levels
          )
        ),
        
        x_dodge = ifelse(
          !!sym(Group) == first_level,
          x_dodge - dodge_width / 2,
          x_dodge + dodge_width / 2
        )
      )
    
    x_aes <- sym("x_dodge")
    
  } else {
    
    x_aes <- sym(xvar)
  }
  
  
  # Base plot
  p <- ggplot(
    data,
    aes(
      x = !!x_aes,
      y = !!sym(yvar),
      color = !!sym(colorvar),
      fill = !!sym(fillvar),
      group = !!sym(Group)
    )
  )
  
  
  # Connecting lines
  if (!is.null(connectVar2)) {
    
    p <- p +
      geom_line(
        aes(
          group = interaction(
            !!sym(connectVar1),
            !!sym(connectVar2)
          )
        ),
        color = "black",
        linewidth = linewidth * 0.5
      )
    
  } else {
    
    p <- p +
      geom_line(
        aes(
          group = !!sym(connectVar1)
        ),
        color = "black",
        linewidth = linewidth * 0.5
      )
  }
  
  
  # Points
  # No position_dodge() here:
  # when use_dodge = TRUE the x positions are already manually dodged
  # through x_dodge.
  if (!is.null(shape_var)) {
    
    # mapped shape
    if (!is.null(fillvar) && fillvar != "fill_dummy") {
      
      p <- p +
        geom_point(
          aes(
            shape = !!sym(shape_var)
          ),
          size = pointsize
        )
      
    } else {
      
      p <- p +
        geom_point(
          aes(
            shape = !!sym(shape_var)
          ),
          size = pointsize,
          fill = "white"
        )
    }
    
    p <- p +
      scale_shape_manual(
        values = shape_values
      )
    
  } else {
    
    # fixed shape
    if (!is.null(fillvar) && fillvar != "fill_dummy") {
      
      p <- p +
        geom_point(
          shape = 21,
          size = pointsize
        )
      
    } else {
      
      p <- p +
        geom_point(
          shape = 21,
          size = pointsize,
          fill = "white"
        )
    }
  }
  
  
  # X-axis
  if (use_dodge) {
    
    # x_dodge is numeric -> continuous scale
    # but show the original categorical labels
    p <- p +
        scale_x_continuous(
          breaks = seq_along(x_levels),
          labels = x_levels,
          expand = expansion(add = c(0.3, 0.3))
        )
    
  } else {
    
    p <- p +
      scale_x_discrete(
        limits = x_levels
      )
  }
  
  
  # Y-axis
  if (is.null(yaxis_trans)) {
    
    p <- p +
      scale_y_continuous(
        limits = ylimits,
        expand = expansion(
          mult = c(
            expand_lower_y_mult,
            0.05
          )
        )
      )
    
  } else {
    
    p <- p +
      scale_y_continuous(
        trans = yaxis_trans,
        limits = ylimits,
        expand = expansion(
          mult = c(
            expand_lower_y_mult,
            0.05
          )
        )
      )
  }
  
  
  # Theme
  p <- p +
    theme_layout +
    theme_fontsize(
      fontsize,
      base_family
    ) +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      plot.title = element_text(
        hjust = 0.5
      )
    )
  
  
  # Color scale
  if (!is.null(colorvar) && colorvar != "color_dummy") {
    
    p <- p +
      scale_color_manual(
        values = colors
      )
    
  } else {
    
    p <- p +
      scale_color_manual(
        values = c(
          "all" = "black"
        )
      )
  }
  
  
  # Fill scale
  if (!is.null(fillvar) && fillvar != "fill_dummy") {
    
    p <- p +
      scale_fill_manual(
        values = fill_colors
      )
  }
  
  
  # Facet
  if (!is.null(facet)) {
    
    if (!is_null(nrow_facets)) {
      
      p <- p +
        facet_wrap(
          facets = facet,
          scales = facet_scales,
          nrow = nrow_facets
        )
      
    } else {
      
      p <- p +
        facet_wrap(
          facets = facet,
          scales = facet_scales
        )
    }
  }
  
  
  return(p)
}