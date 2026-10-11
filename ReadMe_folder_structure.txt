# R Functions Library

This folder contains a curated set of reusable R functions used across multiple data analysis projects.
It is designed to be used together with the separate project template stored in `R_project_template`.

## Folder Structure

### Root: `R_functions/`

- `R_functions.Rproj` — RStudio project file for editing and testing global functions  
- `README.md` — This documentation  
- `R/` — Reusable function library, organized into subfolders:
  - `bose_analysis/` — Domain-specific tools for biomechanics
  - `config/` — Config and parameter readers (`load_yaml.R`)
  - `core/` — General-purpose utilities (`format_pvalue.R`, `fun_ggSave*.R`)
  - `plotting/` — ggplot helpers and plot generators
  - `single_cell_functions/` — Functions for single-cell RNA-seq workflows
  - `theme/` — Custom ggplot themes (`theme_fontsize.R`, `theme_layout.R`)
- `dev/` — Experimental scripts, drafts, or utilities in development

---

## Usage

- **In a project:**  
  Source functions manually or automatically (via `.Rprofile` in `R_project_template`):

  ```r
  global_fun_path <- "C:/Users/.../R_functions/R"
  sapply(list.files(global_fun_path, full.names = TRUE, pattern = "\\.R$", recursive = TRUE), source)