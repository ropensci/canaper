canaper 1.0.1
===============

### DOCUMENTATION

Fix installation of dependencies when installing from r-universe repo (https://github.com/ropensci/canaper/commit/774d89f58af8c55dda178fff3d324b2e48b749fd)

Add warning for abundance data, since CANAPE analysis in canaper treats it the same as presence-absence data. Thanks to an anonymous reviewer for the suggestion (https://github.com/ropensci/canaper/commit/143cadee62a9bf3c5256e3d92240c6243bf803e4)

### BUG FIXES

Fix internal functions `count_higher()` and `count_lower()` that did not distinguish between `NA` and `NA_integer_`. Thanks to @hadley (Hadley Wickham) (https://github.com/ropensci/canaper/commit/ff6012d6b817d3ba8765d8c8b009e2818da2ab9f)

### OTHER

Remove tidyverse from vignettes and Suggests. Thanks to @hadley for pointing this out. (https://github.com/ropensci/canaper/commit/49aa94b418ad9e85f30e6f751651386674a367d8)

canaper 1.0.0
===============

### NEW FEATURES

* Add more palettes and pr_make_pal() to choose palettes (https://github.com/ropensci/canaper/commit/ec624d4c2e85051e4ee5c07143b407de96be7734)

* Add cpr_iter_sim() to help determine number of iterations needed for randomization algorithms (https://github.com/ropensci/canaper/commit/80a47782706816b8238c318ad596b9da47bfba76)

### DOCUMENTATION

* Add logo (https://github.com/ropensci/canaper/commit/1d4232c4597a6ae3167f85afe3efb3d82e462bcd)

canaper 0.0.3
===============

Includes improvements in response to review on ropensci (https://github.com/ropensci/software-review/issues/475)

### DOCUMENTATION

* Update README with comparisons to other packages (https://github.com/ropensci/canaper/commit/d8aaf8de5193166d52ab890d8267090ea2f8ef6a)

* Add link to CONTRIBUTING.md to README (https://github.com/ropensci/canaper/commit/d8aaf8de5193166d52ab890d8267090ea2f8ef6a)

* Change Depends: R (>= 3.5.0) to Depends: R (>= 4.1.0) in DESCRIPTION (https://github.com/ropensci/canaper/commit/0f334965fb5a3fe5034eb1bc682b5051548eddb1)

* Add @KlausVigo as reviewer to DESCRIPTION (https://github.com/ropensci/canaper/commit/6e9b3d55202e2294a6436280ca6b01db13d92974)

### BUG FIXES

* Delete redundant checks in internal functions (https://github.com/ropensci/canaper/commit/bccd0278dd3de5853680d9979ca26c1aa38a7fb3)

### TESTS

* Make skipping of extended tests default (https://github.com/joelnitta/canaper/blob/76c277490dcecd0f32df351c48c8f8c891674aad/tests/testthat/test-cpr_rand_test.R#L395)

* Decrease number of replicates so tests finish faster (https://github.com/ropensci/canaper/commit/12bbe36bf5516a2a63c1235c76234708c4c5432d, https://github.com/ropensci/canaper/commit/f3af4671d679020aeaa97b1a038b3b5a96633f42)

### OTHER

* Import functions from phyloregion instead of copying (https://github.com/ropensci/canaper/commit/7e8b24950912bfa2a5e10cf4dd9cfa8910d74e76)

canaper 0.0.2
===============

### BUG FIXES

* [Fix bug](https://github.com/ropensci/canaper/commit/acb40172c2d5ffb04e13519c6cad7c6d00fa451a) where `cpr_rand_test()` was failing to return identical results after `set.seed()` when parallelization is on

canaper 0.0.1 
===============

### NEW FEATURES

* [Add `cpr_rand_comm()` function](https://github.com/ropensci/canaper/commit/2f3318c1d62f401d7c99bdcc16c64194359c3fbe) for generating a random community

* Implement user-provided community matrix randomization functions

### BREAKING CHANGES

* [Use `vegan`](https://github.com/ropensci/canaper/commit/b0a2d47130398eb62e28eba4783057f781763645) for community matrix randomizations instead of `picante`


### BUG FIXES

* [Fix error](https://github.com/ropensci/canaper/commit/2f3318c1d62f401d7c99bdcc16c64194359c3fbe) in classification of endemism types with `cpr_classify_endem()`,
where super-endemic cells were previously being assigned if `pe_obs_p_upper`
**or** `pe_obs_alt_p_upper` were highly significant (P > 0.99), to requiring
**both** to be highly significant

* [Fix calculation](https://github.com/ropensci/canaper/commit/50ed6e975d14adba334f06a984d521ff69ed961c) of RPD, RPE where alternative tree had all branch lengths
converted to same value, to converting only non-zero branch lengths to same
value

canaper 0.0.0.9000 
===============

### NEW FEATURES

* Initial release on github
