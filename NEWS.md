# ktools 0.1.1

- Helper scripts synced with the versions used for Knecht et al. (Nature Communications):
  lowercase file names matching analysis scripts, added source-data export helpers,
  effect size fix in add_stat_test_dodge(), error_type option in plot_summary_points().
- Maxwell fitting scripts: tau lower bound 1, tau1/tau2 reported in ascending order,
  parameters taken from the best start. New analyses should use maxwellfit
  (https://doi.org/10.5281/zenodo.23298133).

# ktools 0.1.0

- First installable package of the central helper collection.
- Common core, plotting, statistical annotation and theme helpers exported.
- Explicit package namespace imports and independent plotting defaults.
- All existing R scripts available as versioned compatibility resources.
- Original scripts remain in place for existing analyses.
