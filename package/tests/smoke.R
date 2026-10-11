library(ktools)
stopifnot(identical(format_pvalue_star(c(0.00001, 0.01, 0.8)), c("****", "*", "ns")))
data <- data.frame(group = rep(c("A", "B"), each = 5), id = rep(1:5, 2), value = c(1:5, 4:8))
summary <- create_summary_table(data, "value", "group")
stopifnot(nrow(summary) == 2L, all(summary$Mean == c(3, 6)))
plot <- plot_summary_points(data, xvar = "group", yvar = "value")
plot_se_data <- ggplot2::ggplot_build(plot)$data[[2L]]
plot_sd <- plot_summary_points(data, xvar = "group", yvar = "value", error_type = "sd")
plot_sd_data <- ggplot2::ggplot_build(plot_sd)$data[[2L]]
stopifnot(isTRUE(all.equal(plot_sd_data$ymax, c(3, 6) + sd(1:5))))
stopifnot(isTRUE(all.equal(plot_sd_data$ymin, c(3, 6) - sd(1:5))))
stopifnot(isTRUE(all.equal(plot_se_data$ymax, c(3, 6) + sd(1:5) / sqrt(5))))
paired <- plot_paired_points(data, xvar = "group", yvar = "value", connectVar1 = "id")
invisible(ggplot2::ggplot_build(paired))
annotated <- add_stat_test_dodge(plot, yData = "value", Group = "group")
stopifnot(is.list(annotated), "Plot" %in% names(annotated))
invisible(ggplot2::ggplot_build(annotated$Plot))
# paired t-test effect size: exact CI excludes 0 exactly when p < 0.05
paired_data <- data.frame(group = rep(c("A", "B"), each = 6), id = rep(1:6, 2),
                          value = c(1, 2, 3, 4, 5, 6, 1.5, 2.1, 3.9, 4.2, 5.8, 6.1))
paired_plot <- plot_paired_points(paired_data, xvar = "group", yvar = "value", connectVar1 = "id")
paired_stats <- add_stat_test_dodge(paired_plot, yData = "value", Group = "group", paired = TRUE,
                                    id = "id", show_effectsize = TRUE)$data_stat
stopifnot(nrow(paired_stats) == 1L, paired_stats$CI_low < paired_stats$d, paired_stats$d < paired_stats$CI_high)
stopifnot((paired_stats$p < 0.05) == (paired_stats$CI_low > 0 || paired_stats$CI_high < 0))
# lower_ylimit = NA: p-value label sits above negative data
negative <- transform(data, value = value - 10)
negative_stats <- add_stat_test_dodge(plot_summary_points(negative, xvar = "group", yvar = "value"),
                                      yData = "value", Group = "group", lower_ylimit = NA)$data_stat
stopifnot(all(negative_stats$y.position > max(negative$value)))
stopifnot(inherits(theme_layout + theme_fontsize(7, "Arial"), "theme"))
stopifnot(file.exists(legacy_file("bose_analysis/read_units.R")))
maxwell <- readLines(legacy_file("bose_analysis/maxwell_fitting_function.R"))
stopifnot(any(grepl("lower = c(1, 1, 0, 0)", maxwell, fixed = TRUE)))
for (script in list.files(system.file("legacy", package = "ktools"), pattern = "[.][Rr]$", recursive = TRUE, full.names = TRUE)) {
  parse(script)
}
stopifnot(inherits(try(legacy_file("../DESCRIPTION"), silent = TRUE), "try-error"))
