load_AllFunctions <- function (Folder) 
{
    files <- list.files(path = Folder, pattern = "\\.r$", full.names = TRUE)
    for (file in files) {
        source(file, local = .GlobalEnv)
        func_name <- sub("\\.r$", "", basename(file))
        cat("Loaded function:", func_name, "\n")
    }
}
