### Plot gene expression by timepoint and gel: ###
## Plot significant genes over time (MVG vs VLVG) and check for consistency and effect size  ##

library(ggplot2)
library(dplyr)
library(patchwork)
library(purrr)
library(ggpubr)
library(rstatix)
library(plotrix)

# Define the plotting function without individual axis labels
plotting_expression <- function(df, Marker, FontSize=10, base_family = "") {

    # Paired t-tests per timepoint between gels
    stat.test <- df %>%
        filter(!timepoint %in% "d0") %>%
        arrange(donor, timepoint) %>%
        group_by(timepoint) %>%
        pairwise_t_test(MarkerExp ~ gel, paired = TRUE) %>% 
        add_significance() %>%
        add_xy_position(x = "timepoint", fun = "max", step.increase = 0.17) %>%
        mutate(
            yMax = y.position * 1.1,
            p_formatted = format_pvalue(p.adj),
            xmin = xmin + 1, # account for d0
            xmax = xmax +1 # account for d0
        )

    # Dodge positions for line plotting
    df <- df %>%
      mutate(Stimulus_Dodge = as.numeric(as.factor(timepoint))) %>%
      mutate(Stimulus_Dodge = case_when(
        timepoint == "d0" ~ as.numeric(as.factor(timepoint)),  # keep original
        gel %in% c("Fast", "VLVG") ~ Stimulus_Dodge - 0.25,
        TRUE ~ Stimulus_Dodge + 0.25
      ))

    # Plotting
    Plot <- df %>%
        ggplot(aes(x = timepoint, y = MarkerExp, color = gel, fill = gel)) +
        geom_line(aes(x = Stimulus_Dodge, group = interaction(donor, timepoint)), color = "black") +
        geom_point(aes(x = Stimulus_Dodge), shape = 21, size = 2, fill = "white") +
        scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.3))) +
        scale_x_discrete(limits = c("0h","24h", "48h")) +
        scale_color_manual(values = Color.Gels) +
        scale_fill_manual(values = Color.Gels) +
        labs(x = "Timepoint", y = "expr. (a.u.)") +
        theme_fontsize(FontSize, base_family = base_family) +
        theme_layout +
        theme(
            legend.position = "none",
            legend.direction = "horizontal",
            legend.title = element_blank(),
            legend.justification = "center",
            axis.title.x = element_blank(),
            plot.title = element_text(hjust = 0.5, size = 8)
        ) +
        ggtitle(Marker) +
        stat_pvalue_manual(
            inherit.aes = FALSE,
            data = stat.test,
            y.position = "yMax",
            label = "p_formatted",
            vjust = -0.25,
            tip.length = 0,
            linetype = "blank",
            size = FontSize / 2.835
        )

    return(Plot)
}