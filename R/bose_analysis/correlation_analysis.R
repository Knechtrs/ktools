
correlation_analysis <- function(
    df_meta = df_meta, # df with meta data from hematoma
    df_Emod  = df_modulus, # df from E-modulus analysis
    df_fit = list_maxwell_results_two$parameters, # df from maxwell fit analysis
    df_tHalf = df_tHalf, # df from tHalf analysis
    FontSize = FontSize,
    LineWidth = LineWidth
){
  
  # browser()

# get E-modulus and add column with patient letter id
  df_Emod <- df_Emod %>% 
  separate(exp, into = c("Patient", "PatientLetter"), sep = " ", remove = FALSE)
  
  df_fit <- df_fit %>% 
    separate(exp, into = c("Patient", "PatientLetter"), sep = " ", remove = FALSE)
  

# combine meta data with Maxwell fit params
data_corr <- df_fit %>% 
  separate(exp, into = c("Patient", "PatientLetter"), sep = " ", remove = FALSE) %>% 
  left_join(df_meta %>% select(PatientLetter, where(is.numeric)), by="PatientLetter") %>% 
  left_join(df_Emod %>% select(PatientLetter, Emod), by = "PatientLetter") %>% 
  left_join(df_tHalf %>% select(PatientLetter, Time), by = "PatientLetter") %>% 
  select(!c("Youngs")) %>% 
  rename("t1/2" = "Time",
         "tau 1" = "tau1",
         "tau 2" = "tau2",
         "Height" = "height",
         "Length" = "length",
         "Width" = "width",
         "A1 + A2" = "total_relaxation",
         "DPI" = "TimePoint") %>% # rename exp
  filter(!PatientLetter %in% c("Z")) %>% 
  mutate(across(where(~ inherits(.x, "units")), ~ drop_units(.x))) # drop units

### check for normality ##
# Create a Q-Q plot using ggplot2
QQ_Plot <- data_corr %>% 
  pivot_longer(cols = where(is.numeric),
               names_to = "parameter", 
               values_to = "value") %>% 
  ggplot(aes(sample = value)) +  # Use sample aesthetic for Q-Q plot
  stat_qq_line(color = "grey", lwd = LineWidth) + # Reference line
  stat_qq(size=PointSize) +  # Q-Q plot
  facet_wrap(~ parameter, scales="free") +
  labs(x="Theoretical quantiles", y="Sample quantiles") +
  theme_layout +
  theme_fontsize(FontSize) +
  ggtitle("Q-Q Plots") +
  theme(plot.title=element_text(hjust=.5, size=FontSize))
# =>  q-q plots suggest normalization for most parameters

  # Compute correlation matrix and p-values
  # Pearson: assumes normal distribution and linear relationship (sensitive to outliers)
  cor_results_Pearson <- rcorr(as.matrix(
    data_corr %>%
      select(where(is.numeric))
  ), type="pearson")
  
  # Spearman: assumes monotonic relationship (not necessarily linear)
  cor_results_Spearman <- rcorr(as.matrix(
    data_corr %>%
      select(where(is.numeric))
  ), type="spearman")
  
  # => both pearson and spearman show signif. corr between tau2 and timepoint. Use Pearson and linear regression 
  
  cor_results <- cor_results_Pearson
  
  return(list(
    "data_corr" = data_corr,
    "QQ_plot" = QQ_Plot,
    "cor_results" = cor_results
    # "Age_covariant_results" = fit_tau2
  ))

}
