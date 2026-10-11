load_function <- function (file_path) 
{
    file_name <- basename(file_path)
    func_name <- str_replace(file_name, "\\.R$", "")
    
    source(file_path, local = .GlobalEnv)
    
    if (!exists(func_name, envir = .GlobalEnv)) {
        stop(paste0("No function named '", func_name, "' found in ", 
            file_path))
    }
    return(paste0("Function '", func_name, "' loaded from ", 
        file_path))
}
