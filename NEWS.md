# ktools 0.1.2

- The installed package now matches the scripts in `R/`. Version 0.1.1 shipped
  out-of-date package copies:
  - `add_stat_test_dodge()` now includes the paired t-test effect-size fix (exact
    noncentral-t CI, so the CI excludes 0 exactly when p < 0.05) and supports
    `lower_ylimit = NA` for data that can be negative.
  - Legacy Maxwell scripts (`legacy_file("bose_analysis/...")`) now include the
    tau lower bound of 1, the tau1/tau2 ordering and the best-fit parameters;
    the unparseable `maxwell_fit_function.R` is removed.
  - Legacy snapshot updated with the lowercase file names and source-data helpers.
- `plot_summary_points(error_type = "sd")` computes mean ± SD directly; it
  previously needed the Hmisc package, which is not a dependency, and drew no
  error bars without it.
- Smoke tests check SD/SE bar values, the paired effect-size CI, p-value
  placement with negative data, and that all legacy scripts parse.

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
