"""Regenerate the installable package from the canonical scripts in ../R/."""
from pathlib import Path
import shutil
import re
import subprocess
import sys

root = Path(__file__).resolve().parents[1]
source_root = Path(sys.argv[1]).resolve() if len(sys.argv) > 1 else root
package = root / 'package'
package.mkdir(exist_ok=True)
selected = [
    'config/load_yaml.R', 'core/calc_regression.R', 'core/create_summary_table.R',
    'core/format_pvalue.R', 'core/format_pvalue_star.R', 'core/load_rds_objects.R',
    'core/log_slice.R', 'core/save_figure.R', 'theme/theme_fontsize.R',
    'theme/theme_layout.R', 'theme/make_scientific_flextable.R',
    'plotting/tighter_legend.R', 'plotting/plot_paired_points.R',
    'plotting/plot_summary_points.R', 'plotting/add_stat_test_dodge.R',
    'plotting/perm_test_fun.R',
]
for directory in [package / 'R', package / 'inst/legacy']:
    if directory.exists(): shutil.rmtree(directory)
    directory.mkdir(parents=True)
# Snapshot every script as a compatibility resource; none executes on package load.
for src in (source_root / 'R').rglob('*'):
    if not src.is_file() or src.suffix.lower() != '.r': continue
    if any(part.startswith('.') for part in src.relative_to(source_root / 'R').parts): continue
    dest = package / 'inst/legacy' / src.relative_to(source_root / 'R')
    dest.parent.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(src, dest)
for name in selected:
    text = (source_root / 'R' / name).read_text()
    for var, key, default in [('FontSize','fontsize',7),('PointSize','pointsize',1),('LineWidth','linewidth',1)]:
        text = re.sub(r'=\s*'+var+r'\b', f'= getOption("ktools.{key}", {default})', text)
    text = text.replace('ls("package:pals")', 'getNamespaceExports("pals")')
    # Namespace imports replace search-path attachment inside plotting functions.
    text = re.sub(r'^\s*require\((ggplot2|dplyr|rlang)\)\s*$', '', text, flags=re.M)
    text = text.replace('²', r'\u00b2').replace('±', r'\u00b1')
    (package / 'R' / Path(name).name).write_text(text.rstrip()+'\n')
(package / 'R/legacy_file.R').write_text('''# Compatibility accessor for scripts from this installed, pinned package.
legacy_file <- function(path) {
  if (length(path) != 1L || is.na(path) || grepl("(^/|^[A-Za-z]:|\\\\\\\\|(^|/)\\\\.\\\\.(/|$))", path)) {
    stop("Supply a relative helper path within the package.")
  }
  result <- system.file("legacy", path, package = "ktools", mustWork = TRUE)
  result
}
''')
subprocess.run(['Rscript','--vanilla',str(root / 'dev/generate_namespace.R'),str(package)],check=True)
print('Generated',package)
