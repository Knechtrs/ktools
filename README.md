# R functions / ktools

Edit the canonical helper scripts under `R/`. Older projects may continue to
source those files. New projects install `ktools` from `package/` and pin its
version in their own `renv.lock`.

## Develop

1. Edit the original scripts under `R/`.
2. Run `python3 dev/build_package.py` from this repository.
3. The generator copies common helpers into `package/R/`, creates explicit
   namespace imports and documentation, and snapshots all `.R` files under
   `package/inst/legacy/`. Do not hand-edit generated package code.
4. Build and check before releasing:

   ```sh
   R CMD build package --no-build-vignettes --no-manual
   R CMD check ktools_*.tar.gz --no-manual
   ```

The package includes common core, plotting, statistical annotation and theme
helpers. Specialized scripts remain accessible through
`source(ktools::legacy_file("bose_analysis/read_units.R"))` from the installed
version. Such legacy scripts retain their original global-variable/package
requirements; they are not run automatically when ktools loads.

`plot_summary_points()` and `plot_paired_points()` now use
`getOption("ktools.fontsize", 7)`, `getOption("ktools.pointsize", 1)`, and
`getOption("ktools.linewidth", 1)` for defaults. Arguments can still override
these. The project template sets these options from its `params.yaml`.

## First release / later releases

The initial package version is 0.1.0. The project template also keeps a source
archive, so projects can restore their selected version without GitHub access.

For a release:

1. Choose a new version in `package/DESCRIPTION` (bug fix: patch; compatible new
   features: minor; breaking changes: major). For the first release use 0.1.0.
2. Regenerate, build, test, and review both canonical and generated changes.
3. Commit the reviewed source scripts, `package/`, and `dev/` to this repository.
4. Push that commit to GitHub using your normal Git workflow.
5. On https://github.com/Knechtrs/ktools/releases/new create a release with
   tag `v0.1.0` targeting that commit. Publish it as a normal release, not a draft
   or prerelease, and mark it as latest. Never move an already published tag.

New analysis projects query GitHub's latest published stable release, resolve
its tag to an exact commit, and store that package source in `renv/cellar/`.
They keep the version and commit in `renv.lock` and details/checksum in
`config/shared-functions.json`. Existing projects do not update on startup.
If GitHub is unavailable or there is no release, new projects keep the explicitly
reported bundled version. Private GitHub releases require GITHUB_PAT (or
GITHUB_TOKEN) with read access to this repository; keep tokens outside Git.

## Use in an analysis

```r
library(ktools)
packageVersion("ktools")
ktools::theme_fontsize(base_size = 7)
```

Commit the analysis project's `renv.lock`, `config/shared-functions.json`, and
`renv/cellar/` source archive. `renv::restore()` reinstalls the recorded version.
The source archive preserves the package even when GitHub is unavailable.

## Version naming

Use MAJOR.MINOR.PATCH in DESCRIPTION and vMAJOR.MINOR.PATCH as the Git tag.
Start at 0.1.0 while the interface is being established. Use 0.1.1 for compatible
fixes, 0.2.0 for new capabilities, and document any breaking changes clearly while
still below 1.0.0. Publish 1.0.0 once the public interface is stable. After that,
incompatible changes require 2.0.0, compatible features 1.1.0, and fixes 1.0.1.
A numerical bug fix can still change scientific results: review project results
before accepting any update. Never change the contents of a published version.
