clean_plot <- function(p) {
  # 1. Drop stored layout and panel params
  p$layout       <- NULL
  if (!is.null(p$panel_params)) p$panel_params <- NULL

  # 2. Nuke the plot's enclosing environment
  p$plot_env     <- new.env(parent = emptyenv())

  # 3. Zap main aes quosure envs to baseenv() so base R functions remain available
  p$mapping      <- purrr::map(p$mapping, function(q) {
    if (rlang::is_quosure(q)) {
      rlang::quo_set_env(q, baseenv())
    } else {
      q
    }
  })

  # 4. For each layer, clear data/computed slots, reset geom/stat envs, zap mappings
  p$layers <- purrr::map(p$layers, function(l) {
    l$data                 <- ggplot2::waiver()
    l$computed_mapping     <- NULL
    l$computed_geom_params <- NULL
    l$computed_stat_params <- NULL
    l$geom_env             <- new.env(parent = emptyenv())
    l$stat_env             <- new.env(parent = emptyenv())
    if (!is.null(l$mapping)) {
      l$mapping <- purrr::map(l$mapping, function(q) {
        if (rlang::is_quosure(q)) {
          rlang::quo_set_env(q, baseenv())
        } else {
          q
        }
      })
    }
    l
  })

  p
}
