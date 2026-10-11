
#---- Plot r squared plot to see which model worked better ----#+
maxwell_plot_rsquared <- function(
    df1 = list_maxwell_results_one$r_squared, # result from one element fit
    df2 = list_maxwell_results_two$r_squared, # result from two element fit
    id = "exp", # sample id 
    color1 = "#7F7F7F", # color one element
    color2 = "#BDBDBD" # color two element
    ){
  df_r_squared <- full_join(df1, df2, by=id)
  
  Plot_r_squared <- df_r_squared %>% 
    pivot_longer(cols= starts_with("r_squared"), names_to = "Model", values_to = "r_squared") %>% 
    mutate(Model = recode(Model,
                          "r_squared_one" = "1-element",
                          "r_squared_two" = "2-element")) %>% 
    plot_summary_points(
      data = .,
      xvar = "Model",
      yvar = "r_squared",
      fill_var = "Model",
      colors = c("1-element" = color1, "2-element" = color2),
      fontsize = FontSize
    )
  
  return(list(
    "df_r_squared" = df_r_squared,
    "Plot_r_squared" = Plot_r_squared
    ))
}
