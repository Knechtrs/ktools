add_stat_test_dodge <- function(
    plot,                          # ggplot object; its $data is used for the stats
    yData,                         # name (string) of the numeric column to test
    Group,                         # name (string) of the main x-axis grouping column
    Dodge = NULL,                  # name (string) of a dodged/subgrouping column, if any
    dodge_width = NULL,            # dodge width override; default = width of plot's dodge layer
    stat_group_by = NULL,          # name (string) of column to run separate tests within (e.g. facet-like grouping on x)
    test = "t.test",               # "t.test", "wilcox.test", or "permutation"
    Facet = NULL,                  # name (string) of the faceting column, if plot is faceted
    paired = FALSE,                # TRUE for paired comparisons (requires `id`)
    id = NULL,                     # name (string) of subject/sample ID column, used when paired = TRUE
    FontSize = 12,                 # font size (pt) for p-value labels
    yPosition = NULL,              # fixed y-position for all p-values; overrides automatic placement
    adjust_y_position = FALSE,     # TRUE to auto-space overlapping p-value labels
    staggered = FALSE,             # TRUE to stack p-values upward in sequence (needs adjust_y_position)
    alternate = FALSE,             # TRUE to alternate p-values between two heights (needs adjust_y_position)
    pval_spacing = 0.05,           # vertical spacing between staggered/alternated p-values, as fraction of y-range
    expand_y_0 = TRUE,             # (currently unused) intended to control y-axis expansion at 0
    lower_ylimit = 0,              # lower limit for the y-axis
    x_padding = NULL,              # extra horizontal padding added to the x-axis, if set
    pval_gap = 1,                  # multiplier applied to auto-computed p-value y-position
    expand_lower_y_mult = 0,       # y-axis expansion multiplier below lower_ylimit
    show_brackets = FALSE,         # TRUE to draw comparison brackets (line segments) above p-values
    bracket_dist = NULL,           # vertical spacing between stacked brackets; default 0.05 * facet y-range
    format_pvalue_dif = NULL,      # optional custom function(p) to format p-value labels
    top_align = FALSE,             # TRUE to align all p-values within a group/facet to the same (max) height
    show_effectsize = FALSE,       # TRUE to compute and attach an effect size column
    effectsize_type = "g",         # "g" = Hedges' g, "d" = Cohen's d
    method_sd = "z",               # SD method passed to effectsize::repeated_measures_d (paired only)
    remove_comp_g1 = NULL,         # group1 value of a specific pairwise comparison to drop
    remove_comp_g2 = NULL,         # group2 value of a specific pairwise comparison to drop (paired with remove_comp_g1)
    ref_group = NULL,              # reference group for all comparisons (passed to rstatix test as ref.group)
    exclude_group = NULL,          # level(s) of `Group` to exclude entirely before testing
    comparisons = NULL             # list of length-2 vectors, e.g. list(c("1% Alg","+ Col")); keeps ONLY these pairwise comparisons (order-independent), all others are dropped after testing
) {
  
  # Extract data from ggplot object
  df <- plot$data %>%
    droplevels()
  
  
  # ---- helper: rename columns ----
  
  rename_Col <- function(df, Name, NewName) {
    if (!is.null(Name)) {
      df <- df %>%
        dplyr::rename(!!NewName := !!rlang::sym(Name))
    }
    df
  }
  
  
  # ---- standardize column names ----
  
  df <- df %>%
    rename_Col(Name = yData, NewName = "yData_col") %>%
    rename_Col(Name = Facet, NewName = "Facet_col")
  
  
  # Group and Dodge are the same variable
  if (!is.null(Dodge) && !is.null(Group) && Group == Dodge) {
    
    df <- rename_Col(
      df,
      Name = Dodge,
      NewName = "Dodge_col"
    )
    
    df <- df %>%
      dplyr::mutate(Group_col = Dodge_col)
    
    df <- rename_Col(
      df,
      Name = stat_group_by,
      NewName = "stat_group_by"
    )
    
  } else {
    
    df <- df %>%
      rename_Col(Name = Dodge, NewName = "Dodge_col") %>%
      rename_Col(Name = Group, NewName = "Group_col")
  }
  
  
  # ---- exclude factor level(s) before testing ----
  #
  # NOTE: do NOT droplevels() here. Group_col's factor levels reflect
  # the actual x-axis positions in `plot`. If we renumber levels after
  # removing one, add_xy_position() will compute x positions relative
  # to the shrunken level set, misaligning every bracket/p-value with
  # the real (unfiltered) plot axis.
  
  if (!is.null(exclude_group)) {
    
    df <- df %>%
      dplyr::filter(
        !Group_col %in% exclude_group
      )
  }
  
  
  # paired ID
  if (!is.null(id) && paired) {
    df <- rename_Col(
      df,
      Name = id,
      NewName = "ID_Col"
    )
  }
  
  
  # ---- statistical test ----
  
  formula <- stats::as.formula(
    "yData_col ~ Group_col"
  )
  
  
  test_fun <- switch(
    test,
    "t.test" = rstatix::t_test,
    "wilcox.test" = rstatix::wilcox_test,
    "permutation" = perm_test_fun,
    stop(
      "Invalid test specified. Use 't.test', 'wilcox.test', or 'permutation'."
    )
  )
  
  
  # ---- grouping structure ----
  
  group_vars <- character()
  
  if (!is.null(Facet)) {
    group_vars <- c(
      group_vars,
      "Facet_col"
    )
  }
  
  if (!is.null(stat_group_by)) {
    
    group_vars <- c(
      group_vars,
      "stat_group_by"
    )
    
  } else if (
    !is.null(Dodge) &&
    !is.null(Group) &&
    Group != Dodge
  ) {
    
    group_vars <- c(
      group_vars,
      "Dodge_col"
    )
    
  } else if (
    !is.null(Dodge) &&
    is.null(Group)
  ) {
    
    group_vars <- c(
      group_vars,
      "Dodge_col"
    )
  }
  
  
  # ---- remove groups with n <= 2 ----
  
  if (!is.null(Group) && is.null(Dodge)) {
    
    tmp <- df %>%
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(group_vars)
        ),
        Group_col
      ) %>%
      dplyr::summarise(
        n = dplyr::n(),
        .groups = "drop"
      )
    
  } else {
    
    tmp <- df %>%
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(group_vars)
        )
      ) %>%
      dplyr::summarise(
        n = dplyr::n(),
        .groups = "drop"
      )
  }
  
  
  low_n <- tmp %>%
    dplyr::filter(n <= 2)
  
  
  if (nrow(low_n) > 0) {
    
    message(
      "Groups with n <= 2 were removed: ",
      paste(
        apply(
          low_n,
          1,
          paste,
          collapse = " "
        ),
        collapse = "; "
      )
    )
    
    
    if (!is.null(Group) && is.null(Dodge)) {
      
      df <- df %>%
        dplyr::inner_join(
          tmp %>%
            dplyr::filter(n > 2),
          by = c(
            group_vars,
            "Group_col"
          )
        )
      
    } else {
      
      df <- df %>%
        dplyr::inner_join(
          tmp %>%
            dplyr::filter(n > 2),
          by = group_vars
        )
    }
  }
  
  
  # ---- paired testing ----
  
  if (paired && !is.null(id)) {
    
    df <- df %>%
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(
            c(
              group_vars,
              "ID_Col"
            )
          )
        )
      ) %>%
      dplyr::filter(
        dplyr::n_distinct(Group_col) == 2
      ) %>%
      dplyr::ungroup()
    
    
    df <- df %>%
      dplyr::arrange(
        dplyr::across(
          dplyr::all_of(group_vars)
        ),
        ID_Col,
        Group_col
      )
  }
  
  
  # ---- run test ----
  
  if (length(group_vars) > 0) {
    
    stat.test <- df %>%
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(group_vars)
        )
      ) %>%
      test_fun(
        formula,
        paired = paired,
        ref.group = ref_group
      )
    
  } else {
    
    stat.test <- test_fun(
      df,
      formula,
      paired = paired,
      ref.group = ref_group
    )
  }
  
  
  # ---- keep only requested comparisons ----
  #
  # Filtered here (right after the test, before any xy-position math)
  # rather than by removing rows from `df`, so x-axis positions stay
  # aligned with the full, unfiltered plot. Each pair is matched
  # order-independently against group1/group2.
  
  if (!is.null(comparisons)) {
    
    comp_key <- function(g1, g2) {
      paste(
        pmin(as.character(g1), as.character(g2)),
        pmax(as.character(g1), as.character(g2))
      )
    }
    
    wanted_keys <- vapply(
      comparisons,
      function(pair) comp_key(pair[1], pair[2]),
      character(1)
    )
    
    stat.test <- stat.test %>%
      dplyr::filter(
        comp_key(group1, group2) %in% wanted_keys
      )
  }
  
  
  # ---- significance + xy position ----
  
  if (
    !is.null(stat_group_by) &&
    test != "permutation"
  ) {
    
    stat.test <- stat.test %>%
      rstatix::add_significance() %>%
      rstatix::add_xy_position(
        x = "stat_group_by",
        fun = "max",
        dodge = if (!is.null(dodge_width)) {
          dodge_width
        } else {
          plot$layers[[2]]$position$width
        }
      ) %>%
      dplyr::mutate(
        yMax = if (!is.null(yPosition)) {
          yPosition
        } else {
          y.position * pval_gap * 1.05
        },
        
        p_formatted = if (!is.null(format_pvalue_dif)) {
          format_pvalue_dif(p)
        } else {
          format_pvalue(p)
        }
      )
    
    
  } else if (
    test != "permutation" &&
    !is.null(Dodge)
  ) {
    
    stat.test <- stat.test %>%
      rstatix::add_significance() %>%
      rstatix::add_xy_position(
        x = "Group_col",
        fun = "max",
        group = "Dodge_col",
        dodge = if (!is.null(dodge_width)) {
          dodge_width
        } else {
          plot$layers[[2]]$position$width
        }
      ) %>%
      dplyr::mutate(
        yMax = if (!is.null(yPosition)) {
          yPosition
        } else {
          y.position * pval_gap * 1.05
        },
        
        p_formatted = if (!is.null(format_pvalue_dif)) {
          format_pvalue_dif(p)
        } else {
          format_pvalue(p)
        }
      )
    
    
  } else if (
    test != "permutation" &&
    is.null(Dodge)
  ) {
    
    stat.test <- stat.test %>%
      rstatix::add_significance() %>%
      rstatix::add_xy_position(
        fun = "max"
      ) %>%
      dplyr::mutate(
        yMax = if (!is.null(yPosition)) {
          yPosition
        } else {
          y.position * pval_gap * 1.05
        },
        
        p_formatted = if (!is.null(format_pvalue_dif)) {
          format_pvalue_dif(p)
        } else {
          format_pvalue(p)
        }
      )
  }
  
  
  # ---- permutation test positions ----
  
  if (test == "permutation") {
    
    stat.test <- stat.test %>%
      dplyr::mutate(
        xmin = 1,
        xmax = 2,
        yMax = purrr::map_dbl(
          data,
          ~ max(
            .x$yData_col,
            na.rm = TRUE
          )
        ) * 1.1,
        
        p_formatted = if (!is.null(format_pvalue_dif)) {
          format_pvalue_dif(p)
        } else {
          format_pvalue(p)
        }
      ) %>%
      dplyr::rename(
        data_nested = data
      )
    
    
    if (is.null(Dodge)) {
      
      x_levels <- levels(
        factor(
          plot$data[[Group]]
        )
      )
      
      stat.test <- stat.test %>%
        dplyr::mutate(
          xmin = match(
            group1,
            x_levels
          ),
          xmax = match(
            group2,
            x_levels
          )
        )
    }
    
    
    if (
      !is.null(Dodge) &&
      !is.null(stat_group_by)
    ) {
      
      dodge_w <- if (!is.null(dodge_width)) {
        dodge_width
      } else {
        plot$layers[[2]]$position$width
      }
      
      stat.test <- stat.test %>%
        dplyr::mutate(
          dodge_numeric = as.numeric(
            as.factor(
              stat_group_by
            )
          ),
          xmin = dodge_numeric - dodge_w / 2,
          xmax = dodge_numeric + dodge_w / 2
        ) %>%
        dplyr::select(
          -dodge_numeric
        )
    }
  }
  
  
  # ---- effect size ----
  
  if (
    show_effectsize &&
    test != "permutation"
  ) {
    
    if (effectsize_type == "d") {
      
      set_adjust <- FALSE
      
    } else if (effectsize_type == "g") {
      
      set_adjust <- TRUE
      
    } else {
      
      stop(
        "effectsize_type must be 'd' (Cohen's d) or 'g' (Hedges' g)."
      )
    }
    
    
    effectsize_tbl <- df %>%
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(group_vars)
        )
      ) %>%
      dplyr::group_modify(
        ~ {
          
          if (paired) {
            
            effectsize::repeated_measures_d(
              yData_col ~ Group_col | ID_Col,
              data = .x,
              ci = 0.95,
              method = method_sd,
              adjust = set_adjust
            )
            
          } else if (
            effectsize_type == "d"
          ) {
            
            effectsize::cohens_d(
              yData_col ~ Group_col,
              data = .x,
              ci = 0.95,
              hedges.correction = FALSE
            )
            
          } else {
            
            effectsize::hedges_g(
              yData_col ~ Group_col,
              data = .x,
              ci = 0.95
            )
          }
        }
      ) %>%
      dplyr::ungroup()
    
    
    stat.test <- stat.test %>%
      dplyr::left_join(
        effectsize_tbl,
        by = group_vars
      )
  }
  
  
  # ---- calculate base y position per facet ----
  #
  # IMPORTANT:
  # This now happens BEFORE staggering.
  
  if (
    !is.null(Facet) &&
    test != "permutation"
  ) {
    
    if (!is.null(stat_group_by)) {
      
      y_max_per_facet <- df %>%
        dplyr::group_by(
          Facet_col,
          stat_group_by
        ) %>%
        dplyr::summarise(
          y_max = max(
            yData_col,
            na.rm = TRUE
          ),
          .groups = "drop"
        )
      
      
      stat.test <- stat.test %>%
        dplyr::left_join(
          y_max_per_facet,
          by = c(
            "Facet_col",
            "stat_group_by"
          )
        ) %>%
        dplyr::mutate(
          yMax = if (!is.null(yPosition)) {
            yPosition
          } else {
            y_max * pval_gap * 1.05
          }
        ) %>%
        dplyr::select(
          -y_max
        )
      
    } else {
      
      y_max_per_facet <- df %>%
        dplyr::group_by(
          Facet_col
        ) %>%
        dplyr::summarise(
          y_max = max(
            yData_col,
            na.rm = TRUE
          ),
          .groups = "drop"
        )
      
      
      stat.test <- stat.test %>%
        dplyr::left_join(
          y_max_per_facet,
          by = "Facet_col"
        ) %>%
        dplyr::mutate(
          yMax = if (!is.null(yPosition)) {
            yPosition
          } else {
            y_max * pval_gap * 1.05
          }
        ) %>%
        dplyr::select(
          -y_max
        )
    }
  }
  
  
  # ---- adjust / stagger p-value positions ----
  #
  # FIX:
  # 1. Runs AFTER facet-specific y positions are calculated
  # 2. Uses xmin/xmax instead of nonexistent `x`
  # 3. Uses a separate spacing range per facet/group
  
  if (
    adjust_y_position &&
    nrow(stat.test) > 1
  ) {
    
    if (
      staggered &&
      alternate
    ) {
      
      stop(
        "Set only one of `staggered` or `alternate` to TRUE."
      )
    }
    
    
    # calculate local y-range for each statistical grouping unit
    if (length(group_vars) > 0) {
      
      y_range_lookup <- df %>%
        dplyr::group_by(
          dplyr::across(
            dplyr::all_of(group_vars)
          )
        ) %>%
        dplyr::summarise(
          .y_range = max(
            yData_col,
            na.rm = TRUE
          ) - lower_ylimit,
          .groups = "drop"
        )
      
      
      stat.test <- stat.test %>%
        dplyr::left_join(
          y_range_lookup,
          by = group_vars
        ) %>%
        dplyr::group_by(
          dplyr::across(
            dplyr::all_of(group_vars)
          )
        )
      
    } else {
      
      stat.test <- stat.test %>%
        dplyr::mutate(
          .y_range = max(
            df$yData_col,
            na.rm = TRUE
          ) - lower_ylimit
        )
    }
    
    
    if (alternate) {
      
      stat.test <- stat.test %>%
        dplyr::arrange(
          xmin,
          xmax,
          .by_group = TRUE
        ) %>%
        dplyr::mutate(
          yMax = max(
            yMax,
            na.rm = TRUE
          ) +
            (
              (dplyr::row_number() - 1) %% 2
            ) *
            pval_spacing *
            .y_range
        )
      
      
    } else if (staggered) {
      
      stat.test <- stat.test %>%
        dplyr::arrange(
          xmin,
          xmax,
          .by_group = TRUE
        ) %>%
        dplyr::mutate(
          yMax = max(
            yMax,
            na.rm = TRUE
          ) +
            (
              dplyr::row_number() - 1
            ) *
            pval_spacing *
            .y_range
        )
      
      
    } else {
      
      stat.test <- stat.test %>%
        dplyr::mutate(
          yMax = min(
            yMax,
            na.rm = TRUE
          ) +
            (
              rank(
                yMax,
                ties.method = "first"
              ) - 1
            ) *
            pval_spacing *
            .y_range
        )
    }
    
    
    stat.test <- stat.test %>%
      dplyr::ungroup() %>%
      dplyr::select(
        -.y_range
      )
  }
  
  
  # ---- top align ----
  
  if (
    top_align &&
    nrow(stat.test) > 0
  ) {
    
    if (length(group_vars) > 0) {
      
      stat.test <- stat.test %>%
        dplyr::group_by(
          dplyr::across(
            dplyr::all_of(group_vars)
          )
        ) %>%
        dplyr::mutate(
          yMax = max(
            yMax,
            na.rm = TRUE
          )
        ) %>%
        dplyr::ungroup()
      
    } else {
      
      top_val <- max(
        stat.test$yMax,
        na.rm = TRUE
      )
      
      stat.test <- stat.test %>%
        dplyr::mutate(
          yMax = top_val
        )
    }
  }
  
  
  # ---- extra facet offset ----
  
  if (!is.null(Facet)) {
    
    yMax_lookup <- stat.test %>%
      dplyr::reframe(
        yMax_bigger = max(
          yMax,
          na.rm = TRUE
        ),
        .by = "Facet_col"
      )
    
    
    stat.test <- stat.test %>%
      dplyr::left_join(
        yMax_lookup,
        by = "Facet_col"
      ) %>%
      dplyr::mutate(
        yMax = yMax +
          0.05 * yMax_bigger
      ) %>%
      dplyr::select(
        -yMax_bigger
      )
  }
  
  
  # ---- restore facet column name ----
  
  if (!is.null(Facet)) {
    
    stat.test <- stat.test %>%
      dplyr::rename(
        !!Facet := Facet_col
      )
  }
  
  
  # ---- restore stat_group_by name ----
  
  if (!is.null(stat_group_by)) {
    
    stat.test <- stat.test %>%
      dplyr::rename(
        !!stat_group_by :=
          !!rlang::sym("stat_group_by")
      )
  }
  
  
  # ---- remove selected comparison ----
  
  if (
    !is.null(remove_comp_g1) &&
    !is.null(remove_comp_g2)
  ) {
    
    stat.test <- stat.test %>%
      dplyr::filter(
        !(
          group1 == remove_comp_g1 &
            group2 == remove_comp_g2
        )
      )
  }
  
  
  # ---- add brackets / labels ----
  
  if (
    show_brackets &&
    !is.null(stat.test) &&
    nrow(stat.test) > 0
  ) {
    
    if (is.null(bracket_dist)) {
      bracket_dist <- 0.05
    }
    
    
    if (!is.null(Facet)) {
      
      bracket_y_range_lookup <- df %>%
        dplyr::group_by(Facet_col) %>%
        dplyr::summarise(
          bracket_y_range = diff(range(yData_col, na.rm = TRUE)),
          .groups = "drop"
        ) %>%
        dplyr::rename(!!Facet := Facet_col)
      
      stat.test <- stat.test %>%
        dplyr::left_join(bracket_y_range_lookup, by = Facet)
      
    } else {
      
      stat.test <- stat.test %>%
        dplyr::mutate(
          bracket_y_range = diff(range(plot$data[[yData]], na.rm = TRUE))
        )
    }
    
    
    stat.test <- stat.test %>%
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(Facet)
        )
      ) %>%
      dplyr::mutate(
        comp_rank = rank(
          yMax,
          ties.method = "first"
        ),
        
        y_bracket =
          yMax +
          (
            comp_rank - 1
          ) *
          bracket_dist *
          bracket_y_range,
        
        yMax =
          y_bracket +
          0.01 * bracket_y_range
      ) %>%
      dplyr::ungroup() %>%
      dplyr::select(
        -bracket_y_range
      )
    
    
    Plot_out <- plot +
      ggplot2::geom_segment(
        data = stat.test,
        ggplot2::aes(
          x = xmin,
          xend = xmax,
          y = y_bracket,
          yend = y_bracket
        ),
        inherit.aes = FALSE
      ) +
      ggpubr::stat_pvalue_manual(
        data = stat.test,
        label = "p_formatted",
        y.position = "yMax",
        xmin = "xmin",
        xmax = "xmax",
        vjust = -0.25,
        linetype = "blank",
        tip.length = 0,
        size = FontSize / 2.835,
        inherit.aes = FALSE
      )
    
    
  } else {
    
    linetype_val <- if (
      !is.null(Dodge) &&
      is.null(stat_group_by)
    ) {
      "solid"
    } else {
      "blank"
    }
    
    
    tip_length_val <- if (
      linetype_val == "solid"
    ) {
      0.05
    } else {
      0
    }
    
    
    Plot_out <- plot +
      ggpubr::stat_pvalue_manual(
        data = stat.test,
        label = "p_formatted",
        y.position = "yMax",
        xmin = "xmin",
        xmax = "xmax",
        inherit.aes = FALSE,
        vjust = -0.25,
        linetype = linetype_val,
        tip.length = tip_length_val,
        size = FontSize / 2.835
      )
  }
  
  
  # ---- axis expansion ----
  
  y_expand_mult <- dplyr::case_when(
    !is.null(Dodge) &&
      !is.null(Facet) ~ 0.30,
    
    !is.null(Dodge) &&
      is.null(Facet) ~ 0.40,
    
    is.null(Dodge) &&
      !is.null(Facet) ~ 0.25,
    
    TRUE ~ 0.20
  )
  
  
  x_scale <- Plot_out$scales$get_scales(
    "x"
  )
  
  
  if (
    !is.null(x_scale) &&
    !is.null(x_padding)
  ) {
    
    x_scale$expand <- ggplot2::expansion(
      add = c(
        x_padding,
        x_padding
      )
    )
  }
  
  
  Plot_out <- Plot_out +
    ggplot2::scale_y_continuous(
      limits = c(
        lower_ylimit,
        NA
      ),
      expand = ggplot2::expansion(
        mult = c(
          expand_lower_y_mult,
          y_expand_mult
        )
      )
    )
  
  
  return(
    list(
      Plot = Plot_out,
      data_stat = stat.test
    )
  )
}
