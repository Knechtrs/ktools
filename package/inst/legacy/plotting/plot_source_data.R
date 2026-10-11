# Returns a list of data frames: one per layer with distinct data
plot_source_data <- function(p, keep_all = FALSE) {
  if (is.data.frame(p)) return(list(p))
  if (is.list(p) && !inherits(p, "gg") && all(map_lgl(p, is.data.frame))) return(p)
  if (inherits(p, "patchwork")) stop("Pass the individual ggplots, not the patchwork")
  stopifnot(inherits(p, "ggplot"))
  
  facet_vars <- .vars_in(c(p$facet$params$facets,
                           p$facet$params$rows,
                           p$facet$params$cols))
  
  dfs <- map(p$layers, function(l) {
    d <- l$layer_data(p$data)          # resolves inherited / per-layer / function data
    if (!is.data.frame(d) || nrow(d) == 0) return(NULL)
    if (keep_all) return(as.data.frame(d))
    aes_map <- if (isTRUE(l$inherit.aes)) {
      modifyList(as.list(p$mapping), as.list(l$mapping))
    } else as.list(l$mapping)
    vars <- intersect(c(facet_vars, .vars_in(aes_map)), names(d))
    if (length(vars) == 0) return(NULL)
    as.data.frame(d[, vars, drop = FALSE])
  }) |> compact()
  
  dfs <- dfs[!duplicated(dfs)]          # layers sharing the same data -> one sheet
  if (length(dfs) > 1 && is.null(names(dfs))) names(dfs) <- paste0("layer", seq_along(dfs))
  dfs
}