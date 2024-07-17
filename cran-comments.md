## R CMD check results
Duration: 1m 13.7s

❯ checking for unstated dependencies in examples ... OK
   WARNING
  ‘qpdf’ is needed for checks on size reduction of PDFs

0 errors ✔ | 1 warning ✖ | 0 notes ✔


## R check results on R-hub (00check.log files)

- windows: All R versions on GitHub Actions windows-latest
> All Done!
  Running 'testthat.R' [50s]
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... [30s] OK
* checking for non-standard things in the check directory ... OK
* checking for detritus in the temp directory ... OK
* DONE
Status: OK

- linux: All R versions on GitHub Actions ubuntu-latest
> All Done!
  Running ‘testthat.R’ [31s/31s]
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... [22s/20s] OK
* checking for non-standard things in the check directory ... OK
* checking for detritus in the temp directory ... OK
* DONE
Status: OK

- macos-arm64: All R versions on GitHub Actions macos-latest
> All Done!
  Running ‘testthat.R’ [21s/27s]
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes ... OK
* checking re-building of vignette outputs ... [16s/19s] OK
* checking for non-standard things in the check directory ... OK
* checking for detritus in the temp directory ... OK
* DONE
Status: OK

## revdepcheck results

We checked 0 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
