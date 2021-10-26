canaper  0.0.1 
===============

### NEW FEATURES

* Add `cpr_rand_comm()` function for generating a random community
2f3318c1d62f401d7c99bdcc16c64194359c3fbe

* Implement user-provided community matrix randomization functions

### BREAKING CHANGES

* Use `vegan` for community matrix randomizations instead of `picante`
b0a2d47130398eb62e28eba4783057f781763645

### BUG FIXES

* Fix error in classification of endemism types with `cpr_classify_endem()`,
where super-endemic cells were previously being assigned if `pe_obs_p_upper`
**or** `pe_obs_alt_p_upper` were highly significant (P > 0.99), to requiring
**both** to be highly significant 2f3318c1d62f401d7c99bdcc16c64194359c3fbe

* Fix calculation of RPD, RPE where alternative tree had all branch lengths
converted to same value, to converting only non-zero branch lengths to same
value 50ed6e975d14adba334f06a984d521ff69ed961c

canaper  0.0.0.9000 
===============

### NEW FEATURES

* Initial release on github
