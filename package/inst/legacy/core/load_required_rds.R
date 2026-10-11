# Automatically load required RDS files for a figure
load_required_rds <- function(path, plot_vec, suffix = "") {
  for (p in plot_vec) {
    file = file.path(path, paste0(p, ".rds"))
    if (file.exists(file)) {
      varname <- paste0(p, suffix)   # apply suffix here: helps if variable/plot name is already taken
      assign(varname, readRDS(file), envir = .GlobalEnv)
      message("Loaded: ", varname)
    }
  }
}

