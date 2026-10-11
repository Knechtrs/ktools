# ==============================================================================
# export_source_data.R
#
# Extract the underlying data from ggplot panels and write it to one xlsx file
# per figure (one sheet per panel), as required for Nature Communications
# source data.
#
# Features:
#   - Generic extraction from ggplot layers (keeps only mapped variables)
#   - Handles the .data pronoun (aes(x = .data[["Time"]]))
#   - Skips text/label and significance-bracket layers by default
#   - Accepts data frames / lists of data frames for non-ggplot panels
#   - Anonymises donor columns (case-insensitive; consistent project-wide key)
#   - Warns if real donor names remain anywhere in the output
#   - Relabels Fast/Slow, VLVG/MVG -> Low/High in values and column names
#
# Required packages (installed, not necessarily loaded): rlang, purrr, openxlsx
#
# Main entry point: export_source_data()
# ==============================================================================


# Null-default operator (base R >= 4.4 has it; defined here for older R)
`%||%` <- function(x, y) if (is.null(x)) y else x


# ------------------------------------------------------------------------------
# 1. Variable detection in aesthetic mappings and facets
# ------------------------------------------------------------------------------

# Column names referenced by one expression.
# Plain symbols (aes(x = Time)) are returned directly; the .data pronoun is
# resolved: .data$Time, .data[["Time"]] and .data[[var]] (var looked up in the
# environment where the plot was created).
.vars_in_expr <- function(e, env) {
  if (is.symbol(e)) return(as.character(e))
  if (!is.call(e)) return(character())
  
  fn <- e[[1]]
  if ((identical(fn, quote(`$`)) || identical(fn, quote(`[[`))) &&
      identical(e[[2]], quote(.data))) {
    idx <- e[[3]]
    if (identical(fn, quote(`$`))) return(as.character(idx))
    val <- tryCatch(eval(idx, env), error = function(err) NULL)
    return(if (is.character(val)) val else character())
  }
  
  # Any other call (e.g. load_norm * 100): recurse into the arguments,
  # skipping the function name itself
  unlist(lapply(as.list(e)[-1], .vars_in_expr, env = env))
}

# Variable names referenced in a list of aes mappings / facet specs (quosures)
.vars_in <- function(x) {
  unique(unlist(lapply(x, function(q) {
    if (rlang::is_quosure(q)) {
      .vars_in_expr(rlang::quo_get_expr(q), rlang::quo_get_env(q))
    } else {
      .vars_in_expr(q, globalenv())
    }
  })))
}


# ------------------------------------------------------------------------------
# 2. Data extraction from a single panel
# ------------------------------------------------------------------------------

# Returns a list of data frames: one per layer (and per sub-plot) with distinct data.
#
#   p          a ggplot, a patchwork, a data frame, or a list of data frames
#              (non-ggplot panels such as correlation matrices are passed through)
#   keep_all   TRUE = keep all columns of the layer data, not only mapped ones
#   keep_text  TRUE = also export text/label and significance-bracket layers
#   id_cols    identifier columns kept whenever present in a layer's data, even
#              if not mapped to an aesthetic (matched case-insensitively), so
#              paired / per-donor values stay traceable. Names are ignored here;
#              renaming happens in export_source_data()
#
# Patchworks: each sub-plot is extracted separately (plot1, plot2, ...;
# nested patchworks are handled recursively). A patchwork holding a single
# plot, e.g. from Seurat's DimPlot()/VlnPlot(), is treated like a plain ggplot.
#
# Note: layer data is taken *before* stats are computed, so summary layers
# (stat_summary, boxplots) export the raw individual values.
plot_source_data <- function(p, keep_all = FALSE, keep_text = FALSE,
                             id_cols = c("donor", "PatientLetter", "sample", "batch")) {
  
  # Pass-through for data frames and lists of data frames
  if (is.data.frame(p)) return(list(p))
  if (is.list(p) && !inherits(p, "gg") && all(purrr::map_lgl(p, is.data.frame))) return(p)
  
  if (inherits(p, "patchwork")) {
    # A patchwork object stores the earlier plots in p$patches$plots and is
    # itself the last plot (its own $data, $layers, $mapping)
    subplots <- c(p$patches$plots, list(p))
    n <- length(subplots)
    
    out <- lapply(seq_len(n), function(i) {
      if (i < n) plot_source_data(subplots[[i]], keep_all, keep_text, id_cols)   # may recurse
      else .ggplot_source_data(subplots[[i]], keep_all, keep_text, id_cols)      # the last plot
    })
    names(out) <- paste0("plot", seq_len(n))
    
    # Drop empty sub-plots (spacers, fillers, images without data)
    out <- out[lengths(out) > 0]
    if (length(out) == 0) return(list())
    if (length(out) == 1) return(out[[1]])   # single-plot patchwork
    
    # Name sheets plot1, plot2_layer1, plot2_layer2, ...
    out <- purrr::imap(out, function(dfs, nm) {
      names(dfs) <- if (length(dfs) == 1) nm else paste(nm, names(dfs), sep = "_")
      dfs
    })
    return(purrr::list_flatten(out, name_spec = "{inner}"))
  }
  
  stopifnot(inherits(p, "ggplot"))
  .ggplot_source_data(p, keep_all, keep_text, id_cols)
}

# Extraction from one ggplot (also used for the last plot of a patchwork)
.ggplot_source_data <- function(p, keep_all = FALSE, keep_text = FALSE,
                                id_cols = NULL) {
  
  # Facet variables are kept in every layer
  facet_vars <- .vars_in(c(p$facet$params$facets,
                           p$facet$params$rows,
                           p$facet$params$cols))
  
  # Paginated facets (ggforce::facet_wrap_paginate / facet_grid_paginate):
  # only the facets on the selected page are drawn, so only those are exported
  page_keys <- .paginated_keys(p, facet_vars)
  
  # Layers that are annotations rather than data: text labels, significance
  # brackets, and reference lines (geom_hline(yintercept = 0) etc.)
  skip_geoms <- c("GeomText", "GeomLabel", "GeomTextRepel", "GeomLabelRepel",
                  "GeomBracket", "GeomSignif",
                  "GeomHline", "GeomVline", "GeomAbline",
                  # image layers (cowplot::draw_image, ggimage, annotation_raster)
                  "GeomDrawGrob", "GeomImage", "GeomCustomAnn", "GeomRasterAnn",
                  # ggpp npc text (e.g. p-value labels placed by npcx/npcy)
                  "GeomTextNpc", "GeomLabelNpc")
  
  dfs <- purrr::map(p$layers, function(l) {
    if (!keep_text && inherits(l$geom, skip_geoms)) return(NULL)
    
    # Resolves inherited, per-layer and function-valued layer data
    d <- l$layer_data(p$data)
    if (!is.data.frame(d) || nrow(d) == 0) return(NULL)
    if (!is.null(page_keys)) {
      d <- .filter_to_page(d, page_keys)
      if (nrow(d) == 0) return(NULL)
    }
    if (keep_all) return(as.data.frame(d))
    
    # Layer mapping, combined with the global mapping if inherited
    aes_map <- if (isTRUE(l$inherit.aes)) {
      utils::modifyList(as.list(p$mapping), as.list(l$mapping))
    } else {
      as.list(l$mapping)
    }
    
    # Keep only columns that are actually mapped (or used for faceting);
    # after_stat() variables like count are not in d and drop out here
    # Plus identifier columns present in the data (case-insensitive)
    ids  <- names(d)[tolower(names(d)) %in% tolower(unname(id_cols))]
    vars <- intersect(unique(c(facet_vars, .vars_in(aes_map), ids)), names(d))
    if (length(vars) == 0) return(NULL)
    
    # Annotation layers (significance brackets, reference segments from
    # annotate()) only hold positions: x/xend/y/yend/xmin/xmax/... columns
    pos_re <- "^(x|xend|y|yend|xmin|xmax|ymin|ymax|[xy]_?bracket|[xy]_?label)$"
    if (all(grepl(pos_re, setdiff(vars, facet_vars)))) return(NULL)
    as.data.frame(d[, vars, drop = FALSE])
  }) |> purrr::compact()
  
  # Drop layers whose data is fully contained in another layer, e.g. a point
  # layer (gel, Score) that duplicates a line layer (gel, Score, donor).
  # Identical layers are exported once.
  dfs <- .drop_contained(dfs)
  if (length(dfs) > 1 && is.null(names(dfs))) names(dfs) <- paste0("layer", seq_along(dfs))
  dfs
}


# Facet values shown on the current page of a paginated facet, as a data frame
# of facet-variable combinations; NULL if the plot is not paginated
.paginated_keys <- function(p, facet_vars) {
  # Only ggforce's paginated facets; other facets (or other packages) may
  # carry a 'page' parameter with a different meaning
  if (!inherits(p$facet, c("FacetWrapPaginate", "FacetGridPaginate"))) return(NULL)
  
  pr <- p$facet$params
  if (length(facet_vars) == 0) return(NULL)
  
  # page/nrow/ncol may be stored as text (e.g. page = "2"); convert to numbers
  as_num <- function(v) {
    if (length(v) != 1) return(NA_real_)
    suppressWarnings(as.numeric(v))
  }
  pr$page <- as_num(pr$page)
  pr$nrow <- as_num(pr$nrow)
  pr$ncol <- as_num(pr$ncol)
  
  ok <- function(v) !is.na(v) && v >= 1
  if (!ok(pr$page) || !ok(pr$nrow) || !ok(pr$ncol)) {
    warning("Paginated facet without numeric page/nrow/ncol; exporting all facets",
            call. = FALSE)
    return(NULL)
  }
  
  # Layout of all panels, in page order; building the plot can emit harmless
  # scale warnings, which are silenced here
  lay <- tryCatch(
    suppressWarnings(ggplot2::ggplot_build(p))$layout$layout,
    error = function(e) NULL
  )
  if (is.null(lay)) {
    warning("Could not build paginated plot; exporting all facets", call. = FALSE)
    return(NULL)
  }
  lay  <- lay[order(as.integer(lay$PANEL)), , drop = FALSE]
  per  <- pr$nrow * pr$ncol
  idx  <- seq(per * (pr$page - 1) + 1, min(per * pr$page, nrow(lay)))
  cols <- intersect(facet_vars, names(lay))
  if (length(cols) == 0) return(NULL)
  unique(lay[idx, cols, drop = FALSE])
}

# Keep only rows whose facet values are on the page; layers without the facet
# variables (drawn in every panel) are left unchanged
.filter_to_page <- function(d, keys) {
  cols <- intersect(names(keys), names(d))
  if (length(cols) == 0) return(d)
  k_d <- do.call(paste, c(lapply(d[cols],    as.character), sep = "\r"))
  k_k <- do.call(paste, c(lapply(keys[cols], as.character), sep = "\r"))
  d[k_d %in% k_k, , drop = FALSE]
}

# TRUE if every column of `small` exists in `big` and holds the same values
# (row order ignored)
.is_contained <- function(small, big) {
  if (nrow(small) != nrow(big) || !all(names(small) %in% names(big))) return(FALSE)
  b  <- big[names(small)]
  o1 <- do.call(order, unname(as.list(small)))
  o2 <- do.call(order, unname(as.list(b)))
  isTRUE(all.equal(small[o1, , drop = FALSE], b[o2, , drop = FALSE],
                   check.attributes = FALSE))
}

# Remove data frames that are contained in another one in the list
.drop_contained <- function(dfs) {
  if (length(dfs) < 2) return(dfs)
  keep <- rep(TRUE, length(dfs))
  for (i in seq_along(dfs)) {
    for (j in seq_along(dfs)) {
      if (i == j || !keep[j]) next
      # for identical data frames keep the first one
      if (.is_contained(dfs[[i]], dfs[[j]]) &&
          (ncol(dfs[[i]]) < ncol(dfs[[j]]) || i > j)) {
        keep[i] <- FALSE
        break
      }
    }
  }
  dfs[keep]
}


# TRUE if a column holds file paths (e.g. image grids built from PNG paths).
# Paths can expose user names, institutions and folder structure, and are not
# data, so they never go into source data.
.is_path_col <- function(v) {
  if (!(is.character(v) || is.factor(v))) return(FALSE)
  x <- as.character(v)
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) return(FALSE)
  looks_like_path <-
    grepl("^(/|~/|[A-Za-z]:[/\\\\])", x) |
    grepl("[/\\\\]", x) & grepl("\\.(png|jpe?g|tiff?|bmp|gif|svg|pdf|czi|lif|nd2)$",
                                x, ignore.case = TRUE)
  all(looks_like_path)
}


# ------------------------------------------------------------------------------
# 3. Relabelling (e.g. Fast/Slow -> Low/High)
# ------------------------------------------------------------------------------

# Replace whole terms in strings. A term matches only if it is not directly
# preceded or followed by a letter, so "Fast_GMCSF" -> "Low_GMCSF" works,
# but "Breakfast" is untouched. Case-sensitive on purpose.
.replace_terms <- function(x, terms) {
  for (from in names(terms)) {
    x <- gsub(paste0("(?<![A-Za-z])", from, "(?![A-Za-z])"),
              terms[[from]], x, perl = TRUE)
  }
  x
}

# Apply a string function to character columns and factor levels
# (factor level order is preserved); other column types are untouched
.apply_to_text <- function(v, fun) {
  if (is.factor(v)) { levels(v) <- fun(levels(v)); v }
  else if (is.character(v)) fun(v)
  else v
}


# ------------------------------------------------------------------------------
# 4. Donor anonymisation
# ------------------------------------------------------------------------------

# Build a key from real donor names to letter codes (A..Z, then AA..ZZ).
# Create this ONCE for the whole project (e.g. from the sample metadata) and
# pass it to every export_source_data() call, so each donor has the same code
# in every figure. Keep the key private; it is the re-identification key.
#
#   keep_coded  values that already are letter codes (A, B, ..., AA) keep their
#               code, so data matches figures that already show these letters;
#               other donors get the next unused codes
make_donor_key <- function(donors, keep_coded = TRUE) {
  donors <- sort(unique(as.character(donors[!is.na(donors)])))
  codes  <- c(LETTERS, paste0(rep(LETTERS, each = 26), LETTERS))
  
  coded <- if (keep_coded) donors[donors %in% codes] else character()
  other <- setdiff(donors, coded)
  free  <- setdiff(codes, coded)
  if (length(other) > length(free)) stop("Too many donors for letter codes")
  
  c(stats::setNames(coded, coded),
    stats::setNames(free[seq_along(other)], other))
}

# Find donor columns in a data frame:
#   donor_cols     exact column names, matched case-insensitively
#                  ("donor" matches donor, Donor, DONOR)
#   donor_pattern  optional regex, case-insensitive (e.g. "^donor" also
#                  matches donor_id, Donor_name)
.find_donor_cols <- function(df, donor_cols = NULL, donor_pattern = NULL) {
  nm  <- names(df)
  hit <- tolower(nm) %in% tolower(donor_cols)
  if (!is.null(donor_pattern)) hit <- hit | grepl(donor_pattern, nm, ignore.case = TRUE)
  nm[hit]
}

# Replace donor names with their codes in the given columns.
# Stops with an error if a value has no code in the key, so a wrongly
# matched column (e.g. donor_age) fails loudly instead of silently.
.anonymise <- function(df, cols, key) {
  for (col in cols) {
    v <- df[[col]]
    if (is.factor(v)) v <- droplevels(v)
    vals <- as.character(v)
    
    missing <- setdiff(unique(vals[!is.na(vals)]), names(key))
    if (length(missing)) stop("Column '", col, "': no code in donor_key for: ",
                              paste(missing, collapse = ", "))
    
    if (is.factor(v)) levels(v) <- unname(key[levels(v)]) else v <- unname(key[vals])
    df[[col]] <- v
  }
  df
}

# Safety net: warn if any real donor name still appears anywhere in the output
# (column names or text values), e.g. inside a sample ID like "D12_rep1"
# or in a donor column that was not detected. Reports sheet AND column.
#
# Skipped: donors that already were codes (A -> A), and IDs shorter than
# min_nchar characters. Single letters (single-cell donors A/B/C) are
# pseudonyms, not identifying, and match too much (gene HLA-A, patient A).
# A name only matches as a whole token (not next to letters or digits).
.check_donor_leak <- function(sheets, key, min_nchar = 2) {
  real <- names(key)[names(key) != key & nchar(names(key)) >= min_nchar]
  if (length(real) == 0) return(invisible())
  
  pattern_for <- function(d) paste0("(?<![A-Za-z0-9])\\Q", d, "\\E(?![A-Za-z0-9])")
  hits_in <- function(txt) {
    real[vapply(real, function(d) any(grepl(pattern_for(d), txt, perl = TRUE)),
                logical(1))]
  }
  
  hits <- character()
  for (nm in names(sheets)) {
    df <- sheets[[nm]]
    h <- hits_in(names(df))
    if (length(h)) hits <- c(hits, paste0("  ", nm, " (column names): ",
                                          paste(h, collapse = ", ")))
    for (col in names(df)) {
      v <- df[[col]]
      if (!(is.character(v) || is.factor(v))) next
      h <- hits_in(unique(as.character(v)))
      if (length(h)) hits <- c(hits, paste0("  ", nm, " $ ", col, ": ",
                                            paste(h, collapse = ", ")))
    }
  }
  
  if (length(hits)) warning("Original donor names still present:\n",
                            paste(hits, collapse = "\n"), call. = FALSE)
}



# Make data frames safe to write to xlsx. Excel reports a corrupt file
# ("Removed Records: Cell information") for cells it cannot read:
#   - list columns (e.g. gene lists)       -> collapsed to "a, b, c"
#   - control characters (\x00-\x1F)       -> removed (invalid in xlsx XML)
#   - text longer than 32,767 characters   -> truncated, with a warning
.excel_safe <- function(sheets, max_chars = 32000) {
  bad_ctrl  <- "[\\x00-\\x08\\x0B\\x0C\\x0E-\\x1F]"
  truncated <- character()
  
  clean_text <- function(x) gsub(bad_ctrl, "", x, perl = TRUE)
  
  out <- lapply(names(sheets), function(nm) {
    df <- as.data.frame(sheets[[nm]])
    names(df) <- clean_text(names(df))
    for (col in names(df)) {
      v <- df[[col]]
      if (is.list(v)) {
        v <- vapply(v, function(e) paste(as.character(unlist(e)), collapse = ", "),
                    character(1))
      }
      if (is.factor(v)) v <- as.character(v)
      if (is.character(v)) {
        v <- clean_text(v)
        long <- !is.na(v) & nchar(v) > max_chars
        if (any(long)) {
          v[long] <- paste0(substr(v[long], 1, max_chars), " [truncated]")
          truncated <- c(truncated, paste0("  ", nm, " $ ", col, ": ", sum(long), " cell(s)"))
        }
      }
      df[[col]] <- v
    }
    df
  })
  names(out) <- names(sheets)
  
  if (length(truncated)) warning("Text longer than Excel's cell limit was truncated:\n",
                                 paste(truncated, collapse = "\n"), call. = FALSE)
  out
}


# Extract all panels into named data frames, with the final (Excel-safe)
# sheet names. Shared by export_source_data() and source_sheet_names().
.extract_sheets <- function(panels, id_cols) {
  sheets <- purrr::imap(panels, function(x, nm) {
    dfs <- plot_source_data(x, id_cols = unname(id_cols))
    if (length(dfs) == 1) rlang::set_names(dfs, nm)
    else rlang::set_names(dfs, paste(nm, names(dfs), sep = "_"))
  }) |> purrr::list_flatten(name_spec = "{inner}")
  
  # Excel sheet names are limited to 31 characters
  # (truncation can create duplicates -> make them unique: name_1, name_2, ...)
  names(sheets) <- make.unique(substr(names(sheets), 1, 28), sep = "_")
  sheets
}

# The sheet names export_source_data() will create for these panels, e.g. to
# list them in an index sheet that matches the workbook exactly
source_sheet_names <- function(panels,
                               id_cols = c("donor", "PatientLetter", "sample", "batch")) {
  names(.extract_sheets(panels, id_cols))
}

# ------------------------------------------------------------------------------
# 5. Main function
# ------------------------------------------------------------------------------

# Export source data for one figure.
#
#   panels         named list; names become sheet names (e.g. Fig_3b).
#                  Elements: ggplots, data frames, or lists of data frames.
#                  Panels with several data layers get one sheet per layer
#                  (Fig_1c_layer1, Fig_1c_layer2, ...).
#   file           output .xlsx path
#   donor_cols     donor column names, case-insensitive (default "donor")
#   donor_pattern  optional regex for donor columns, e.g. "^donor"
#   donor_key      named vector real name -> code from make_donor_key();
#                  if NULL, a key is built from this figure only (not
#                  consistent across figures; a message reminds you)
#   recode         named vector of terms to replace in values and column names;
#                  set to NULL to disable
#   id_cols        identifier columns kept even if not mapped in the plot
#                  (case-insensitive). Named elements are also renamed:
#                  c(id = "exp") keeps exp and calls it id; c(donor = "exp")
#                  additionally anonymises it as a donor column.
#                  Only add columns that are safe to publish.
#   drop_cols      regex of plotting-helper columns to remove from every sheet
#                  (default: *_dummy, x_dodge, and internal columns starting
#                  with "." such as .group_var, ._row_id); NULL keeps all
#
# Returns (invisibly) the cleaned sheets and the donor key used.
export_source_data <- function(panels, file,
                               donor_cols = "donor", donor_pattern = NULL,
                               donor_key = NULL,
                               recode = c(VLVG = "Low", MVG = "High",
                                          Fast = "Low", Slow = "High"),
                               id_cols = c("donor", "PatientLetter", "sample", "batch"),
                               drop_cols = "dummy$|^x_dodge$|^\\.") {
  
  # ---- Extract: one or more sheets per panel (final sheet names) ----
  sheets <- .extract_sheets(panels, id_cols)
  
  # ---- Rename identifier columns (named elements of id_cols) ----
  to_rename <- id_cols[nzchar(names(id_cols) %||% rep("", length(id_cols)))]
  if (length(to_rename)) {
    sheets <- lapply(sheets, function(df) {
      for (i in seq_along(to_rename)) {
        hit <- tolower(names(df)) == tolower(to_rename[[i]])
        if (any(hit) && !names(to_rename)[i] %in% names(df)) names(df)[hit] <- names(to_rename)[i]
      }
      df
    })
  }
  
  # ---- Remove plotting-helper columns ----
  if (!is.null(drop_cols)) {
    sheets <- lapply(sheets, function(df)
      df[, !grepl(drop_cols, names(df)), drop = FALSE])
  }
  
  # ---- Remove file paths ----
  # Path columns are always removed. If a sheet had paths and no numeric
  # column is left (an image panel), the whole sheet is dropped.
  dropped <- character()
  for (nm in names(sheets)) {
    df <- sheets[[nm]]
    is_path <- vapply(df, .is_path_col, logical(1))
    if (!any(is_path)) next
    df <- df[, !is_path, drop = FALSE]
    if (!any(vapply(df, is.numeric, logical(1)))) {
      sheets[[nm]] <- NULL
      dropped <- c(dropped, paste0("  ", nm, ": whole sheet (image panel)"))
    } else {
      sheets[[nm]] <- df
      dropped <- c(dropped, paste0("  ", nm, ": ",
                                   paste(names(is_path)[is_path], collapse = ", ")))
    }
  }
  if (length(dropped)) message("Removed file paths:\n", paste(dropped, collapse = "\n"))
  
  # ---- Donor anonymisation ----
  found <- lapply(sheets, .find_donor_cols,
                  donor_cols = donor_cols, donor_pattern = donor_pattern)
  
  if (any(lengths(found) > 0)) {
    if (is.null(donor_key)) {
      all_donors <- unlist(purrr::map2(sheets, found, function(df, cols)
        lapply(df[cols], as.character)))
      donor_key <- make_donor_key(all_donors)
      message("donor_key built from this figure only; pass a project-wide donor_key ",
              "so codes are consistent across figures.")
    }
    
    sheets <- purrr::map2(sheets, found, .anonymise, key = donor_key)
    
    # Report which columns were anonymised in which sheet
    rep <- found[lengths(found) > 0]
    message("Anonymised donor columns:\n",
            paste0("  ", names(rep), ": ",
                   vapply(rep, paste, "", collapse = ", "),
                   collapse = "\n"))
  } else if (length(donor_cols) || !is.null(donor_pattern)) {
    message("No donor columns found in any sheet.")
  }
  
  if (!is.null(donor_key)) .check_donor_leak(sheets, donor_key)
  
  # ---- Line breaks used for wrapping plot labels -> spaces ----
  unwrap <- function(x) gsub("\\s*\n\\s*", " ", x)
  sheets <- lapply(sheets, function(df) {
    names(df) <- unwrap(names(df))
    df[] <- lapply(df, .apply_to_text, fun = unwrap)
    df
  })
  
  # ---- Relabelling (values and column names) ----
  if (length(recode)) {
    sheets <- lapply(sheets, function(df) {
      names(df) <- .replace_terms(names(df), recode)
      df[] <- lapply(df, .apply_to_text,
                     fun = function(x) .replace_terms(x, recode))
      df
    })
  }
  
  # ---- Write ----
  # ---- Make cells valid for Excel ----
  sheets <- .excel_safe(sheets)
  
  # ---- Sheets too long for Excel -> separate compressed CSV ----
  # Excel holds at most 1,048,576 rows per sheet (incl. header); longer sheets
  # corrupt the workbook. Their data go to <file>_<sheet>.csv.gz next to the
  # workbook, and the sheet itself only says where to find them.
  max_rows <- 1048575
  for (nm in names(sheets)) {
    n <- nrow(sheets[[nm]])
    if (n <= max_rows) next
    csv_name <- paste0(tools::file_path_sans_ext(basename(file)), "_", nm, ".csv.gz")
    con <- gzfile(file.path(dirname(file), csv_name), "w")
    utils::write.csv(sheets[[nm]], con, row.names = FALSE)
    close(con)
    sheets[[nm]] <- data.frame(
      note = paste0("Too many rows for Excel (", format(n, big.mark = ","),
                    " rows). Data are provided in the separate file ", csv_name, ".")
    )
    message("Sheet ", nm, ": ", format(n, big.mark = ","), " rows -> ", csv_name)
  }
  
  openxlsx::write.xlsx(sheets, file, overwrite = TRUE)
  invisible(list(sheets = sheets, donor_key = donor_key))
}


# ------------------------------------------------------------------------------
# Example
# ------------------------------------------------------------------------------
# # Once per project (keep the key file private, do not deposit it):
# donor_key <- make_donor_key(sample_metadata$donor)
#
# # Per figure:
# export_source_data(
#   list(
#     Fig_3b = plot_emod_stat,
#     Fig_3c = plot_thalf_stat,
#     Fig_3j = plot_pca_gmcsf_lps
#   ),
#   file.path(output_dir_sourceData, "Fig3_sourceData.xlsx"),
#   donor_key = donor_key
# )


# ------------------------------------------------------------------------------
# 6. Helpers for figure scripts
# ------------------------------------------------------------------------------

# Source data for a panel that shows only part of the plot's data, e.g. one
# marker of a faceted / paginated plot:
#   panel_subset(Plot_CD80, marker %in% c("CD80", "CD86", "HLA-DR"))
# Returns a list of data frames, which export_source_data() passes through.
panel_subset <- function(p, ..., id_cols = c("donor", "PatientLetter", "sample", "batch")) {
  dfs <- plot_source_data(p, id_cols = unname(id_cols))
  out <- lapply(dfs, function(d) dplyr::filter(d, ...))
  
  # Guard against filters that match nothing (e.g. a misspelled sample name):
  # stop and show the available values of the filtered column(s)
  if (all(vapply(out, nrow, integer(1)) == 0)) {
    cols  <- all.vars(substitute(list(...)))
    avail <- unlist(lapply(dfs, function(d)
      lapply(intersect(cols, names(d)), function(cl)
        paste0(cl, ": ", paste(utils::head(unique(as.character(d[[cl]])), 20),
                               collapse = ", ")))))
    stop("panel_subset(): filter matched no rows.\nAvailable values:\n  ",
         paste(unique(avail), collapse = "\n  "), call. = FALSE)
  }
  out
}

# List donor values per panel and column (before anonymisation),
# e.g. as the starting point for the project-wide donor key
collect_donors <- function(panels, donor_cols = "donor", donor_pattern = NULL) {
  rows <- list()
  for (nm in names(panels)) {
    for (df in plot_source_data(panels[[nm]])) {
      for (col in .find_donor_cols(df, donor_cols, donor_pattern)) {
        rows[[length(rows) + 1]] <- data.frame(
          panel  = nm,
          column = col,
          donor  = unique(as.character(df[[col]]))
        )
      }
    }
  }
  dplyr::distinct(dplyr::bind_rows(rows))
}

# Give the layers of a panel meaningful names, used as sheet suffixes:
#   name_layers(Plot_stresscurves_example, c("measured", "model_fit"))
#   -> sheets Fig_1c_measured, Fig_1c_model_fit
# x can be a plot or the output of panel_subset(); the number of names must
# match the number of exported layers.
name_layers <- function(x, layer_names, ...) {
  dfs <- if (is.list(x) && !inherits(x, "gg")) x else plot_source_data(x, ...)
  if (length(dfs) != length(layer_names)) {
    stop("Panel has ", length(dfs), " data layer(s) but ", length(layer_names),
         " name(s) were given")
  }
  stats::setNames(dfs, layer_names)
}

# Prefix donor IDs of a panel, e.g. to keep imaging donor "A" apart from
# single-cell donor "A" before anonymisation:
#   prefix_donors(Plot_morph_nuc, "img_")  -> donors img_A, img_P, img_R
# Works for plots, patchworks, data frames and lists of data frames.
prefix_donors <- function(x, prefix, donor_cols = "donor", donor_pattern = NULL) {
  lapply(plot_source_data(x), function(d) {
    for (cl in .find_donor_cols(d, donor_cols, donor_pattern)) {
      v <- as.character(d[[cl]])
      d[[cl]] <- ifelse(is.na(v), NA, paste0(prefix, v))
    }
    d
  })
}

# Flatten nested lists of panels into one named list for export_source_data():
#   flatten_panels(list(amplitude = p1, tau = p2), "SF02")
#   -> list(SF02_amplitude = p1, SF02_tau = p2)
# Plots, patchworks, data frames and lists of data frames are treated as one
# panel each; other lists are expanded recursively.
flatten_panels <- function(x, name) {
  is_leaf <- is.data.frame(x) || inherits(x, "gg") ||
    (is.list(x) && length(x) > 0 && all(purrr::map_lgl(x, is.data.frame)))
  if (is_leaf) return(stats::setNames(list(x), name))
  if (!is.list(x) || length(x) == 0) return(list())
  
  child_names <- names(x)
  if (is.null(child_names)) child_names <- as.character(seq_along(x))
  child_names[!nzchar(child_names)] <- as.character(which(!nzchar(child_names)))
  
  purrr::map2(x, child_names, function(child, nm)
    flatten_panels(child, paste(name, nm, sep = "_"))) |>
    purrr::list_flatten(name_spec = "{inner}")
}

# Volcano data with gene names (Seurat stores them as rownames) and a logical
# significance column instead of the plotting colour
volcano_data <- function(p) {
  d <- p$data
  if (!"gene" %in% names(d)) d <- tibble::rownames_to_column(as.data.frame(d), "gene")
  if ("color" %in% names(d)) d <- dplyr::mutate(d, significant = color == "black", .keep = "unused")
  d
}


# ------------------------------------------------------------------------------
# 7. Project-wide donor key from two private tables
# ------------------------------------------------------------------------------
# Both files live in a PRIVATE folder (never in the repository or the deposited
# source data), as they contain the original donor IDs.
#
#   donor_codes.csv    one row per person:            donor, code
#   donor_aliases.csv  other IDs used for that person: prefix, alias, donor
#                      (e.g. img_ + A -> HIAL; empty prefix for plain aliases
#                      such as the typo ZIS -> ZISI)
#
# Returns the named vector original ID -> code used by export_source_data().
load_donor_key <- function(path) {
  read_chr <- function(f) utils::read.csv(file.path(path, f), colClasses = "character",
                                          na.strings = character(0))
  codes   <- read_chr("donor_codes.csv")
  aliases <- read_chr("donor_aliases.csv")
  
  dup <- codes$donor[duplicated(codes$donor)]
  if (length(dup)) stop("Duplicated donors in donor_codes.csv: ", paste(dup, collapse = ", "))
  
  unknown <- setdiff(aliases$donor, codes$donor)
  if (length(unknown)) stop("donor_aliases.csv refers to donors not in donor_codes.csv: ",
                            paste(unknown, collapse = ", "))
  
  alias_ids  <- paste0(aliases$prefix, aliases$alias)
  alias_code <- codes$code[match(aliases$donor, codes$donor)]
  
  key <- c(stats::setNames(codes$code, codes$donor),
           stats::setNames(alias_code, alias_ids))
  
  # The same ID must never point to two different codes
  clash <- names(which(tapply(key, names(key), function(x) length(unique(x)) > 1)))
  if (length(clash)) stop("IDs with conflicting codes: ", paste(clash, collapse = ", "))
  
  key[!duplicated(names(key))]
}
