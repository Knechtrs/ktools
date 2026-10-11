load_rds_objects <- function(path, pattern = "\\.rds$", verbose = TRUE, look_recursive = TRUE) {
  # Load required package
  if (!requireNamespace("tools", quietly = TRUE)) {
    stop("Package 'tools' is required but not installed.")
  }
  
  # List matching RDS files in path
  rds_files <- list.files(path, pattern = pattern, full.names = TRUE, recursive = look_recursive)
  
  if (length(rds_files) == 0) {
    warning("No RDS files found in path: ", path)
    return(invisible(NULL))
  }
  
  # Loop and assign
  for (f in rds_files) {
    varname <- tools::file_path_sans_ext(basename(f))
    assign(varname, readRDS(f), envir = .GlobalEnv)
    if (verbose) message("Loaded: ", varname)
  }
  
  invisible(NULL)
}
