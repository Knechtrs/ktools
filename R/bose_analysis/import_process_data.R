
# get list with data files
list.csv <- list.files(path = here("data/raw"), pattern = "\\.csv|\\.CSV", recursive=FALSE, full.names=TRUE) 

# get names for data files
Names_list.csv <- list.csv

# Create clean names
Names_list.csv <- list.csv %>%
  basename() %>%                # remove full folder path
  str_remove("\\.csv$|\\.CSV$")  # remove .csv extension only

# create list with files
list_data <- list.csv %>%
  map(~ readr::read_csv(.x, show_col_types = FALSE)) %>%
  setNames(Names_list.csv) # add filename to the list items

# bind list to dataframe
df_data_raw <- list_data %>% 
  lapply(function(df) slice(df, -1)) %>% # remove first row with unit
  bind_rows(.id="exp") %>% 
  select(c("exp", "Elapsed Time", "Scan Time", "Disp", "Load 3"))

#---- create metadata columns ----#
metaData <-params$metaData

df_data <- df_data_raw %>%
  filter(!exp %in% params$RemoveSample) %>% 
  mutate(Alginate = str_extract(exp, "MVG|VLVG"),
         Batch = case_when(
             str_detect(exp, "VLVG") ~ str_extract(exp, "(?<=Ca_)\\w+"),
             str_detect(exp, "MVG")  ~ str_extract(exp, "(?<=500ul_)\\w+"),
             TRUE                    ~ NA_character_ # fallback if neither VLVG nor MVG
           )
         ) %>% 
  select(!"Scan Time") %>%
  mutate(across(c("Elapsed Time", "Disp", "Load 3"), as.double)) %>% 
  rename("Time" = "Elapsed Time",
         "Load" = "Load 3") %>%
  mutate( #Disp = Disp-min(Disp), # ensure Disp starts at 0 and not at gel height
    Load = Load *(-1), # ensure Load is positive
    exp = as.factor(exp))# ensure exp is factor for grouping

#---- check for NA in dataframe ----#
if (sum(is.na(df_data)) > 0) {
  
  # Remove rows containing any NA values
  df_data <- df_data %>% drop_na()
  
  # Print message
  cat("Missing values found and rows removed.\n")
  cat("Remaining NAs after cleaning:\n")
  print(colSums(is.na(df_data)))
} else {
  cat("No missing values found. No rows removed.\n")
}

#---- change order of alginate and re-label them ----#

df_data <- df_data %>% 
  dplyr::mutate(
    Alginate = Alginate %>%
      forcats::fct_recode("Slow" = "MVG", "Fast" = "VLVG") %>%
      forcats::fct_relevel("Fast", "Slow")
  )
