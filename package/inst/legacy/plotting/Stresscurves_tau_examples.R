
plot_stresscurve_tau <- function(
    data = data_stressrelax, # df with stress data
    list_fitted_coef = list_fitted_coef, # coefficinets (tau1 and tau2 from fitting)
    Group = "PatientLetter", # unique identifier of sample. e.g patient id or fast/slow
    time_var = "time", # x_var
    facet_var = "PatientLetter", # for facet: for comb plot
    facet_scales = "free",
    color_var = "alginate", # for tau1 and tau2 plots
    colors = Color.Gels, # for tau1 and tau2 plots
    LineWidth = 1
    ) {
  
  # browser()


# define parameters:
sigma0 <- 1
# norm_den <- (A1 + A2)

coeffs <- list_fitted_coef %>% select(Group, A1, A2, tau1, tau2)

if(is.list(data)) {
  data <- dplyr::bind_rows(as.list(data), .id = "PatientLetter")
}


# create data frame with tau1 and tau2 and tau_comb time values
list_tau_data <- pmap(
  list(
    id = coeffs[[Group]],
    A1     = coeffs$A1,
    A2     = coeffs$A2,
    tau1   = coeffs$tau1,
    tau2   = coeffs$tau2
  ),
  function(id, A1, A2, tau1, tau2, sigma0 = 1) {
    df <- data %>% dplyr::filter(!!sym(Group) %in% id)
    t <- df[[time_var]]
    norm_den <- A1 + A2
     
    df %>%
      mutate(
        stress_tau1 = (A1 * exp(-t / tau1)) / norm_den,
        stress_tau2 = (A2 * exp(-t / tau2)) / norm_den,
        stress_tau_comb = 1 - (sigma0 * (A1 * (1 - exp(-t / tau1)) +
                                           A2 * (1 - exp(-t / tau2))))
        # stress_tau1 = (A1 * exp(-t / tau1)) / norm_den,
        # stress_tau2 = (A2 * exp(-t / tau2)) / norm_den,
        # stress_tau_comb = (A1 * exp(-t / tau1) + A2 * exp(-t / tau2)) / norm_den
      )
  }
)

# name list item with patient id
names(list_tau_data) <- coeffs[[Group]]

# flatten list to dataframe
df_tau_data <- bind_rows(list_tau_data)

# reduce file size of maxwell results for plotting. Otherwise rds file becomes huge
df_tau_data_sliced <- df_tau_data %>% log_slice(group_col = Group, points = 10^3)

# quick overview plot
Plot_stresscurves_comb <- df_tau_data_sliced %>% 
  pivot_longer(names_to = "Tau", values_to = "load_all", cols = c("load_norm", "stress_tau1", "stress_tau2", "stress_tau_comb")) %>% 
  ggplot(
    aes(x=!!sym(time_var), y= load_all, color =Tau)
  ) +
  geom_path() +
  facet_wrap(vars(!!sym(facet_var)), scales = facet_scales) +
  scale_color_manual(values = c(
    "load_norm" = "black",
    "stress_tau_comb" = "grey70",
    "stress_tau1" = "#44AA99",
    "stress_tau2" =  "#117733"
    )
  ) +
  scale_x_continuous(limits = c(0,3000), expand = expansion((c(0.05,0)))) +
  labs(x="Time (s)", y = "Normalized stress") +
  theme_layout +
  theme_fontsize(FontSize) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.spacing.x = unit(2, "lines"))

# create plot function
plotting_stresscurves_tau <- function(df, y_var) {

  tau_number <- as.numeric(str_extract(y_var, "\\d+"))   # extract the digits
  
   p <- df %>% 
    ggplot(
      aes(x=!!sym(time_var), y= !!sym(y_var), color = !!sym(color_var), group = !!sym(Group))
    ) +
    geom_path(linewidth = LineWidth) +
    # scale_color_manual(values = pals::kelly()[-(1:2)]) +
    # scale_color_manual(values = RColorBrewer::brewer.pal(8, "Set1")) +
    scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0.05, 0))) +
    labs(x="Time (s)", y = bquote("Normalized stress " * tau[.(tau_number)])) +# title = bquote(tau[.(tau_number)] * " : early phase stress relaxation")) +
    theme_layout +
    theme_fontsize(FontSize) +
    theme(plot.title = element_text(hjust = 0.5))
     
   # ----- Color scale handling -----
   if (!is.null(color_var)) {
     n_groups <- length(unique(df[[color_var]]))
     if (n_groups < 3) n_groups <- 3
     
     # Coerce list -> named character vector if needed
     if (is.list(colors)) colors <- unlist(colors, use.names = TRUE)
     
     pal_vals <- NULL
     
     if (is.character(colors) && length(colors) == 1) {
       # Case 1: RColorBrewer name
       if (colors %in% rownames(RColorBrewer::brewer.pal.info)) {
         pal_vals <- RColorBrewer::brewer.pal(min(max(n_groups, 3), 9), colors)
         
         # Case 2: pals palette name
       } else if (colors %in% ls("package:pals")) {
         pal_fun <- get(colors, envir = asNamespace("pals"))
         
         # if palette function accepts n, call with n; otherwise take full vector
         takes_n <- "n" %in% names(formals(pal_fun))
         pal_raw <- if (takes_n) pal_fun(max(n_groups, 3)) else pal_fun()
         
         # special-case Kelly: drop black & white
         if (identical(colors, "kelly")) {
           if (length(pal_raw) < (n_groups + 2)) {
             # if someone reduced kelly(), re-fetch full set
             pal_raw <- pals::kelly()
           }
           pal_raw <- pal_raw[-(1:2)]
         }
         
         pal_vals <- pal_raw[seq_len(min(length(pal_raw), n_groups))]
         
         # Case 3: single color name -> repeat
       } else {
         pal_vals <- rep(colors, n_groups)
       }
       
       p <- p + scale_color_manual(values = pal_vals)
       
     } else {
       # Case 4: explicit vector supplied
       p <- p + scale_color_manual(values = colors)
     }
   }
   
   return(p)
}

# create plots
Plot_stresscurve_tau1 <- plotting_stresscurves_tau(df_tau_data_sliced, "stress_tau1")

Plot_stresscurve_tau2 <- plotting_stresscurves_tau(df_tau_data_sliced, "stress_tau2")

return(list(
  Plot_stresscurves_comb = Plot_stresscurves_comb,
  Plot_stresscurve_tau1 = Plot_stresscurve_tau1,
  Plot_stresscurve_tau2 = Plot_stresscurve_tau2
  )
)

}
