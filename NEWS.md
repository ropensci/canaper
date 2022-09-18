canaper  0.0.3
===============

Includes improvements in response to review on ropensci (https://github.com/ropensci/software-review/issues/475)
### DOCUMENTATION

* Update README with comparisons to other packages (d8aaf8de5193166d52ab890d8267090ea2f8ef6a)

* Add link to CONTRIBUTING.md to README (d8aaf8de5193166d52ab890d8267090ea2f8ef6a)

* Change Depends: R (>= 3.5.0) to Depends: R (>= 4.1.0) in DESCRIPTION (0f334965fb5a3fe5034eb1bc682b5051548eddb1)

* Add @KlausVigo as reviewer to DESCRIPTION (6e9b3d55202e2294a6436280ca6b01db13d92974)

### BUG FIXES

* Delete redundant checks in internal functions (bccd0278dd3de5853680d9979ca26c1aa38a7fb3)

### TESTS

* Make skipping of extended tests default (https://github.com/joelnitta/canaper/blob/76c277490dcecd0f32df351c48c8f8c891674aad/tests/testthat/test-cpr_rand_test.R#L395)

* Decrease number of replicates so tests finish faster (12bbe36bf5516a2a63c1235c76234708c4c5432d, f3af4671d679020aeaa97b1a038b3b5a96633f42)

### OTHER

* Import functions from phyloregion instead of copying (7e8b24950912bfa2a5e10cf4dd9cfa8910d74e76)

canaper  0.0.2
===============

### BUG FIXES

* [Fix bug](https://github.com/ropensci/canaper/commit/acb40172c2d5ffb04e13519c6cad7c6d00fa451a) where `cpr_rand_test()` was failing to return identical results after `set.seed()` when parallelization is on

canaper  0.0.1 
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

canaper  0.0.0.9000 
===============

### NEW FEATURES

* Initial release on github
