## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
❯ On windows-x86_64-devel (r-devel)
  checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ''NULL''

❯ On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

❯ On ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

0 errors ✔ | 0 warnings ✔ | 3 notes ✖


> revdepcheck::revdep_check(num_workers = 4)
── INSTALL ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 2 versions ──
Installing CRAN version of SticsRFiles
installation des dépendances ‘pillar’, ‘tidyselect’, ‘vctrs’, ‘timechange’, ‘stringi’, ‘fansi’, ‘pkgconfig’, ‘purrr’, ‘cpp11’, ‘Rcpp’, ‘cli’, ‘crayon’, ‘curl’, ‘data.table’, ‘dplyr’, ‘lifecycle’, ‘lubridate’, ‘rlang’, ‘rstudioapi’, ‘stringr’, ‘tibble’, ‘tidyr’, ‘XML’, ‘xml2’, ‘xslt’

Installing DEV version of SticsRFiles
Installing 31 packages: rlang, cli, vctrs, stringi, magrittr, lifecycle, glue, withr, pkgconfig, utf8, fansi, tidyselect, tibble, R6, pillar, generics, cpp11, Rcpp, xml2, stringr, purrr, dplyr, timechange, xslt, XML, tidyr, rstudioapi, lubridate, data.table, curl, crayon
── CHECK ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── 0 packages ──
OK: 0
BROKEN: 0
Total time: <1 min
── REPORT ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
Writing summary to 'revdep/README.md'
Writing problems to 'revdep/problems.md'
Writing failures to 'revdep/failures.md'
Writing CRAN report to 'revdep/cran.md'
