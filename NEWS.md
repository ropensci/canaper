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
