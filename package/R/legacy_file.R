# Compatibility accessor for scripts from this installed, pinned package.
legacy_file <- function(path) {
  if (length(path) != 1L || is.na(path) || grepl("(^/|^[A-Za-z]:|\\\\|(^|/)\\.\\.(/|$))", path)) {
    stop("Supply a relative helper path within the package.")
  }
  result <- system.file("legacy", path, package = "ktools", mustWork = TRUE)
  result
}
