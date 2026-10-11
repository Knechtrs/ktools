load_yaml <- function(
    default_file = here("config", "params.yaml"), 
    override_file = NULL
  ) {
  # Load default
  settings <- yaml::read_yaml(default_file)
  
  # If override exists, update defaults
  if (!is.null(override_file)) {
    override <- yaml::read_yaml(override_file)
    settings[names(override)] <- override
  }
  
  return(settings)
}