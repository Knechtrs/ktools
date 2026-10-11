plot_summary_points <- function(
    data, # data
    xvar = NULL, 
    yvar,
    shape_var = NULL, # variable for symbol shape
    fill_var = NULL, # fill variable for fill color
    color_var = NULL, # color variable for color color
    Group = NULL, # group for doging and summary calc
    use_dodge = FALSE,
    dodge_width = 0.5,
    jitter_width = 0.1,
    colors = "grey", # color vector for groups
    ylimits = c(0, NA),
    fontsize = getOption("ktools.fontsize", 7),
    pointsize = getOption("ktools.pointsize", 1),
    linewidth = getOption("ktools.linewidth", 1),
    facet = NULL,
    facet_scales = "fixed",
    facet_nrow = NULL,
    show_stat_summary = TRUE,
    error_type = c("se", "sd"),
    stat_summary_color = "black",
    stat_summary_width = 0.4,
    stat_summary_linewidth_factor = 3
) {
  
  # browser()



  data <- data %>% droplevels()
  
  # If xvar is NULL, use a constant "All"
  if (is.null(xvar)) {
    data <- data %>% mutate(x_dummy = "All")
    xvar <- "x_dummy"
  }
  
  # If fill_var or Group are NULL, use constant
  if (is.null(fill_var)) {
    data <- data %>% mutate(fill_dummy = "all")
    fill_var <- "fill_dummy"
  }
  
  # If color_var or Group are NULL, use constant
  if (is.null(color_var)) {
    data <- data %>% mutate(color_dummy = "all")
    color_var <- "color_dummy"
  }

  if (is.null(Group)) {
    Group <- fill_var
  }
  
  # Positioning
  dodge <- if (use_dodge) position_dodge(width = dodge_width) else position_identity()
  jitter_dodge <- if (use_dodge) {
    position_jitterdodge(jitter.width = jitter_width, dodge.width = dodge_width)
  } else {
    position_jitter(width = jitter_width)
  }
  
  # Build aesthetic mapping
  aes_args <- list(
    x     = sym(xvar),
    y     = sym(yvar),
    fill  = sym(fill_var),
    group = sym(Group)
  )
  
  ## add shape if shape_var != NULL
  if (!is.null(shape_var)) {
    aes_args$shape <- sym(shape_var)
  }
  
  # create geom_jitter layer with and without shape option
  if (is.null(shape_var)) {
    geom_layer <- geom_jitter(
      size = pointsize, shape = 21, alpha = 0.8, color = "black", 
      position = jitter_dodge
    )
  } else {
    geom_layer <- geom_jitter(
      size = pointsize, alpha = 0.8, color = "black", 
      position = jitter_dodge
    )
  }
  
  
  # Base plot
  p <- ggplot(data, do.call(aes, aes_args)) +
    geom_layer +
    scale_y_continuous(limits = ylimits, expand = expansion(mult = c(0, 0.05))) +
    theme_layout +
    theme_fontsize(fontsize) +
    theme(
      legend.position = "none",
      axis.title.x = element_blank(),
      plot.title = element_text(hjust = 0.5)
    )
  
  # add to the function arguments:
  #   error_type = c("se", "sd")
  
  error_type <- match.arg(error_type)
  error_fun <- switch(
    error_type,
    se = mean_se,
    # mean \u00b1 1 SD; computed directly because mean_sdl() needs Hmisc
    sd = function(x) {
      x <- x[!is.na(x)]
      m <- mean(x)
      s <- stats::sd(x)
      data.frame(y = m, ymin = m - s, ymax = m + s)
    }
  )
  
  if (show_stat_summary) {
    p <- p +
      stat_summary(
        fun.data = error_fun,
        geom = "errorbar",
        linewidth = linewidth / (stat_summary_linewidth_factor * 1.3),
        width = stat_summary_width / 2,
        color = stat_summary_color,
        position = dodge,
        show.legend = FALSE
      ) +
      stat_summary(
        fun = mean,
        geom = "crossbar",
        linewidth = linewidth / stat_summary_linewidth_factor,
        width = stat_summary_width,
        fatten = 2,
        color = stat_summary_color,
        position = dodge,
        show.legend = FALSE
      )
  }
  

  
  if (!is.null(fill_var) && fill_var != "fill_dummy") {
    n_groups <- length(unique(data[[fill_var]]))
    if (n_groups < 3) n_groups <- 3
    
    if (is.character(colors) && length(colors) == 1) {
      # --- Case 1: RColorBrewer palette ---
      if (colors %in% rownames(RColorBrewer::brewer.pal.info)) {
        pal_vals <- RColorBrewer::brewer.pal(min(max(n_groups, 3), 9), colors)
        
        # --- Case 2: pals palette ---
      } else if (colors %in% getNamespaceExports("pals")) {
        pal_fun <- get(colors, envir = asNamespace("pals"))
        pal_vals <- pal_fun(max(n_groups, 3))
        
        # Special handling for 'kelly' palette — remove black and white
        if (colors == "kelly") {
          pal_vals <- pal_fun(max(n_groups + 2, 3))
          pal_vals <- pal_vals[-(1:2)]
        }

        # --- Case 3: default: try to interpret as single color name ---
      } else {
        pal_vals <- rep(colors, n_groups)
      }
      
      p <- p + scale_fill_manual(values = pal_vals)
      
    } else {
      # --- Case 4: explicit vector of colors provided ---
      p <- p + scale_fill_manual(values = colors)
    }
  }

  # Only apply color scale if color_var was originally specified
  if (!is.null(color_var) && color_var != "color_dummy") {
    p <- p + scale_color_manual(values = colors)
  }

  # Only apply shape scale if shape_var was originally specified
  if (!is.null(shape_var)) {
    p <- p + scale_shape_manual(values = c(21, 22, 23, 24))
    }

  if (!is.null(facet)) {
    # if (!is.null(facet_nrow)) {
      p <- p + facet_wrap(
        facets = facet,
        scales = facet_scales,
        nrow = facet_nrow
      )
    } 
  
  # define legend symbols correctly. 
  p <- p + guides(
    fill  = guide_legend(override.aes = list(shape = 21, color = "black")),
    shape = guide_legend(override.aes = list(fill = "white", color = "black"))
  )
  
  return(p)
}
