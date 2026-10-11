# ktools 0.1.1

- Licensed under the MIT license (previously all rights reserved).
- Maxwell stress-relaxation fitting (`bose_analysis/`):
  - Two-element model now reports the slower relaxation process as `tau2`
    (swapping `tau1`/`A1` with `tau2`/`A2` where needed); fitted curves are
    unchanged because the model is symmetric under this relabeling.
  - Two-element tau lower bound raised from 0 to 1 s, matching the Methods.
  - `maxwell_fitting_function_general()` now reports the parameters of the
    best fit (`coef(best_fit)`) instead of the last attempted fit.
- Removed the unparseable, unused legacy copy `maxwell_fit_function.R`.

# ktools 0.1.0

- First installable package of the central helper collection.
- Common core, plotting, statistical annotation and theme helpers exported.
- Explicit package namespace imports and independent plotting defaults.
- All existing R scripts available as versioned compatibility resources.
- Original scripts remain in place for existing analyses.
