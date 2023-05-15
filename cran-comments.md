## R CMD check results (first release)

## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
❯ On windows-x86_64-devel (r-devel)
  checking CRAN incoming feasibility ... [18s] NOTE
  
  New submission
  
  Non-FOSS package license (file LICENSE)
  
  Possibly misspelled words in DESCRIPTION:
    JavaSTICS (25:37)
    STICS (2:24, 24:57)
    fortran (26:34)
  Maintainer: 'Patrice Lecharpentier <patrice.lecharpentier@inrae.fr>'

❯ On windows-x86_64-devel (r-devel)
  checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ''NULL''

❯ On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

❯ On ubuntu-gcc-release (r-release)
  checking CRAN incoming feasibility ... [3s/16s] NOTE
  Maintainer: ‘Patrice Lecharpentier <patrice.lecharpentier@inrae.fr>’
  
  New submission
  
  Non-FOSS package license (file LICENSE)
  
  Possibly misspelled words in DESCRIPTION:
    fortran (26:34)
    JavaSTICS (25:37)
    STICS (2:24, 24:57)

❯ On ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

❯ On fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... [4s/23s] NOTE
  Maintainer: ‘Patrice Lecharpentier <patrice.lecharpentier@inrae.fr>’
  
  New submission
  
  Non-FOSS package license (file LICENSE)
  
  Possibly misspelled words in DESCRIPTION:
    JavaSTICS (25:37)
    STICS (2:24, 24:57)
    fortran (26:34)

❯ On fedora-clang-devel (r-devel)
  checking for future file timestamps ... NOTE
  unable to verify current time

0 errors ✔ | 0 warnings ✔ | 7 notes ✖


## Author's comments

* This is a new release.

* This is a first submission the package doesn't have any downstream dependencies yet.

* Words considered as misspelled in check results are software or language names.

* The package license is a CeCILL-C


