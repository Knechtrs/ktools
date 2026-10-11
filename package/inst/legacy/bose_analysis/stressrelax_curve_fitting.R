stressrelax_curve_fitting <- function(
    df,
    time = Time, # column name of time data
    t_max = params$tMax # define max time for curve fitting: tau parameters are influenced by measurement time.
    
    )
  {

# shorten data to t_max:
df_short <- df %>% filter(time <= t_max)

# apply one element fit function to data 
list_fit_one <- analyze_maxwell_model(df_short, model_type = "one", t_max = t_max)

list_fit_one$plots$plot_fitted # check plots
list_fit_one$result$parameters # check fit parameters

# apply two element fit function to data 
list_fit_two <- analyze_maxwell_model(df_short, model_type = "two", t_max = t_max)

list_fit_two$plots$plot_fitted # check plots
list_fit_two$result$parameters # check fit parameters

# #---- create plots with 1 element and 2 element fit ----#
# 
# # Extract fitted data and add a column to distinguish models
# data_fitted_one <- list_fit_one$result$fitted_data %>%
#   mutate(model = "1 Element")
# 
# data_fitted_two <- list_fit_two$result$fitted_data %>%
#   mutate(model = "2 Element")
# 
# # combine into one dataframe:
# # Perform inner_join using purrr::reduce for multiple data frames
# df_comb_fitted <- list(df, data_fitted_one, data_fitted_two) %>% # us df and not df_short: allows to check for extrapolation
#   reduce(inner_join, by = c("exp", "time"), suffix = c(".one", ".two")) %>% 
#   select(exp, time, Load_norm, load_predict.one, load_predict.two) %>% 
#   mutate(Alginate = str_extract(exp, "MVG|VLVG"),
#          Batch = case_when(
#            str_detect(exp, "VLVG") ~ str_extract(exp, "(?<=Ca_)\\w+"),
#            str_detect(exp, "MVG")  ~ str_extract(exp, "(?<=500ul_)\\w+"),
#            TRUE                    ~ NA_character_ # fallback if neither VLVG nor MVG
#          )
#   ) %>% 
#   dplyr::mutate(
#     Alginate = Alginate %>%
#       forcats::fct_recode("Slow" = "MVG", "Fast" = "VLVG") %>%
#       forcats::fct_relevel("Fast", "Slow")
#   )
# 
# plot_fitted <- df_comb_fitted %>% 
#   ggplot() +
#   # Change geom_path to include mapping for the legend
#   geom_path(aes(x = time, y = Load_norm, color = Alginate), 
#             linewidth = 1.5) +
#   geom_path(aes(x = time, y = load_predict.one, color = "1 Element"), 
#             alpha = 1, 
#             linewidth = 1,
#             linetype="12") +
#   geom_path(aes(x = time, y = load_predict.two, color = "2 Element"), 
#             alpha = 1, 
#             linewidth = 1,
#             linetype="12") +
#   facet_wrap(~ exp, nrow=2) +
#   # Add scale_color_manual to control the colors and legend
#   scale_color_manual(name = "", # No title for the legend
#                      values = c("Fast" = Color.Gels$Fast,
#                                 "Slow" = Color.Gels$Slow,
#                                 "1 Element" = "black",
#                                 "2 Element" = "grey")) +
#   scale_x_continuous(limits=c(0,t_max), expand=expansion(mult=c(0.05,0))) +
#   labs(
#     x = "time (s)",
#     y = "Normalized stress"
#   ) +
#   theme_Layout +
#   theme_fontsize(FontSize)
# 
# #---- Plot r squared plot to see which model worked better ----#
# df_r_squared <- full_join(list_fit_one$result$r_squared, list_fit_two$result$r_squared, by="Patient_ID")
# 
# # get Alginate and batch column
# df_r_squared <- df_r_squared %>% 
#   rename("exp" = "Patient_ID") %>% 
#   mutate(Alginate = str_extract(exp, "MVG|VLVG"),
#        Batch = case_when(
#          str_detect(exp, "VLVG") ~ str_extract(exp, "(?<=Ca_)\\w+"),
#          str_detect(exp, "MVG")  ~ str_extract(exp, "(?<=500ul_)\\w+"),
#          TRUE                    ~ NA_character_ # fallback if neither VLVG nor MVG
#        )
#     ) %>% 
#   dplyr::mutate(
#     Alginate = Alginate %>%
#       forcats::fct_recode("Slow" = "MVG", "Fast" = "VLVG") %>%
#       forcats::fct_relevel("Fast", "Slow")
#   )
# 
# 
# # create plot
# Plot_rSquared <- plot_summary_points(
#   data = df_r_squared %>% 
#     pivot_longer(cols = starts_with("r_squared"), names_to = "Model", values_to = "r_squared") %>%
#     mutate(Model = recode(Model, "r_squared_one" = "1 Element", "r_squared_two" = "2 Element")),
#   xvar = "Model",
#   yvar = "r_squared",
#   fillvar = "Alginate",
#   Group = "Alginate",
#   use_dodge = TRUE,
#   colors = Color.Gels,
#   fontsize = FontSize
# ) +
#   labs(y= expression(R^2))
# 
# 
# #---- Plot tau1 and tau2 Plots ----#
# # add alginate and batch column
# df_tau <- list_fit_two$result$parameters %>% 
#   mutate(Alginate = str_extract(exp, "MVG|VLVG"),
#          Batch = case_when(
#            str_detect(exp, "VLVG") ~ str_extract(exp, "(?<=Ca_)\\w+"),
#            str_detect(exp, "MVG")  ~ str_extract(exp, "(?<=500ul_)\\w+"),
#            TRUE                    ~ NA_character_ # fallback if neither VLVG nor MVG
#          )
#   ) %>% 
#   dplyr::mutate(
#     Alginate = Alginate %>%
#       forcats::fct_recode("Slow" = "MVG", "Fast" = "VLVG") %>%
#       forcats::fct_relevel("Fast", "Slow")
#   )
# 
# # create plot
# Plot_tau_overview <- plot_summary_points(
#   data = df_tau %>% 
#     pivot_longer(cols = where(is.double), names_to = "Parameter", values_to = "Values"),
#   xvar = "Alginate",
#   yvar = "Values",
#   fillvar = "Alginate",
#   Group = "Alginate",
#   use_dodge = TRUE,
#   colors=Color.Gels,
#   fontsize = FontSize,
#   facet = ~Parameter,
#   facet_scales = "free_y"
# ) +
#   scale_y_continuous(limit=c(0,NA), expand=expansion(mult=c(0,0.1)))

}