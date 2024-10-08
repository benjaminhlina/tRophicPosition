tRophicPosition v 0.8.1 (Release date: 2024-09-05)
==============

-  `extractIsotopeData()`, `generateTPData()`, `loadIsotopeData()`, `simulateTDF()`, and `TDF()`, all have the argument `seed` with a default set to `3`. CRAN does not allow `set.seed()` to be pre set. Found out about this issue with the initial releases of `{nichetools}`. The default of this being set to `3` has been changed to being a random number which is what CRAN wants. 

- Updated example in `fromParallelTP()` as it was missing a `)`. `seed` has been added to this example with it set to `3`.

- Updated example in `multiSpeciesTP()` to have `seed` argument in `generateTPData()` with `seed` set to `3`. 
 
- all examples have ben updated for those functions above with `seed` set to `3`. All documentation for these functions has been updated. 

- The file `generataIsotopeData.R` is blank in this repo and Benjamin Hlina's forked version. Is this suppose to be the case? 

- In the `Roach.R` documentation has the wrong URL.  It has changed to the following https://fishbase.mnhn.fr/summary/SpeciesSummary.php?ID=272&AT=roach and is currently up-to-date in the R and Rd file. 

- Removed `codecov.yml` file as this is no longer standard practice for code coverage. `test-coverage.yaml` has been added. This yaml, however, will fail as it will not be able find the secret token considering the token is invalid. The token can be added to the repo with the token name being CODECOV_TOKEN. To add the token go to the repo's setting -> secrets and variables -> actions -> new repository secret, then copy and paste the token from codecov's page for the repo under configuration -> general, and copy the token over, without the `CODECOV_TOKEN=`. 

- `R-CMD-check.yaml` has been updated. This yaml will pull and build jags for each OS and install `{rjags}` properly to check the package. 

- `pr-commands.yaml` has been added to make pull request checks easier.

- The `DESCRIPTION` file has been updated with the correct version of `{Roxygen}` and `{SIBER}` has been added into suggested pkgs as one of the vignettes uses it, the version number has been updated to `0.8.2`. 

- Vignettes have been updated to use `seed` argument properly so they are consistent with `seed` set to `3`.

- Package (this specific branch) has been added to Benjamin Hlina's [r-universe](https://benjaminhlina.r-universe.dev/tRophicPosition) as another way to build and access this package. 

tRophicPosition v 0.8.0 (Release date: 2022-12-11)
==============

* Updating CRAN version (archived recently due to email error). Small fixes.

tRophicPosition v 0.7.7 (Release date: 2019-04-05)
==============

* Updating CRAN version (archived recently). Small fix regarding examples.

tRophicPosition v 0.7.6 (Release date: 2018-06-27)
==============

* Fixed [issue 69](https://github.com/clquezada/tRophicPosition/issues/69) (parametricTP() when using only one baseline). This small fix changes current version of tRophicPosition to 0.7.6 only in GitHub. CRAN version will be updated soon.

tRophicPosition v0.7.5 (Release date: 2018-01-29)
==============

* Added the function extractPredictiveData() to perform a posterior predictive model-checking procedure.
* Implemented code coverage and unit test.
* Improved the code (to make it clearer, considering width and the assignment operator).
* Changed some arguments (consumer instead of species, group instead of community and others) in loadIsotopeData(), extractIsotopeData() and other functions (while maintaining old arguments as compatibility)
* Added a a procedure for checking errors on priors arguments in jagsBayesianModel and related functions
* Removed MCMCvis from the Short guide to tRophicPosition vignette, as that package is no longer in CRAN.

tRophicPosition v0.7.3 (Release date: 2017-10-12)
==============

* Minor details to fulfil CRAN checks

tRophicPosition v0.7.2 (Release date: 2017-10-12)
==============

* Fixed loadIsotopeData() when loading species without a community to iterate from.
* Added stable isotope data examples (Finnish Lakes and Roach)
* Improved credibilityIntervals(). Now it accepts legend position (for TP and alpha plots), a grouping variable (to plot groups with different colours), manual colours (scale_colour_manual) when using group_by, and labels for the x axis.

tRophicPosition v0.7.0 (Release date: 2017-06-11)
==============

First release version submitted to CRAN.

* List of capabilities
