
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/master/examples>. -->

# canaper

<!-- badges: start -->
<!-- badges: end -->

The goal of canaper is to enable categorical analysis of neo- and
paleo-endemism (CANAPE) in R.

## Installation

You can install canaper from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("joelnitta/canaper")
```

## Example usage

These examples will use the dataset included with the `picante` package.

``` r
library(canaper)
library(picante)
#> Loading required package: ape
#> Loading required package: vegan
#> Loading required package: permute
#> Loading required package: lattice
#> This is vegan 2.5-7
#> Loading required package: nlme

data(phylocom)

# Example community matrix including 4 "clumped" communities, 
# one "even" community, and one "random" community
phylocom$sample
#>         sp1 sp10 sp11 sp12 sp13 sp14 sp15 sp17 sp18 sp19 sp2 sp20 sp21 sp22
#> clump1    1    0    0    0    0    0    0    0    0    0   1    0    0    0
#> clump2a   1    2    2    2    0    0    0    0    0    0   1    0    0    0
#> clump2b   1    0    0    0    0    0    0    2    2    2   1    2    0    0
#> clump4    1    1    0    0    0    0    0    2    2    0   1    0    0    0
#> even      1    0    0    0    1    0    0    1    0    0   0    0    1    0
#> random    0    0    0    1    0    4    2    3    0    0   1    0    0    1
#>         sp24 sp25 sp26 sp29 sp3 sp4 sp5 sp6 sp7 sp8 sp9
#> clump1     0    0    0    0   1   1   1   1   1   1   0
#> clump2a    0    0    0    0   1   1   0   0   0   0   2
#> clump2b    0    0    0    0   1   1   0   0   0   0   0
#> clump4     0    2    2    0   0   0   0   0   0   0   1
#> even       0    1    0    1   0   0   1   0   0   0   1
#> random     2    0    0    0   0   0   2   0   0   0   0

# Example phylogeny
phylocom$phy
#> 
#> Phylogenetic tree with 32 tips and 31 internal nodes.
#> 
#> Tip labels:
#>   sp1, sp2, sp3, sp4, sp5, sp6, ...
#> Node labels:
#>   A, B, C, D, E, F, ...
#> 
#> Rooted; includes branch lengths.
```

The main “workhorse” function of `canaper` is `cpr_rand_test()`, which
conducts a randomization test to determine if observed values of
phylogenetic diversity (PD) and phylogenetic endemism (PE) are
significantly different from random. It also calculates the same values
on an alternative phylogeny where all branch lengths have been set equal
(alternative PD, alternative PE) as well as the ratio of the original
value to the alternative value (relative PD, relative PE).

``` r
rand_test_results <- cpr_rand_test(phylocom$sample, phylocom$phy)
#> [1] "Dropping tips from the tree because they are not present in the community data:"
#> [1] "sp16" "sp23" "sp27" "sp28" "sp30" "sp31" "sp32"
```

`cpr_rand_test` produces **a lot** of columns (nine per metric), so
let’s just look at a subset of them:

``` r
rand_test_results[,1:9]
#>            pd_obs pd_rand_mean pd_rand_sd  pd_obs_z pd_obs_c_upper
#> clump1  0.3018868    0.4694340 0.03489188 -4.801896              0
#> clump2a 0.3207547    0.4688679 0.03417284 -4.334237              0
#> clump2b 0.3396226    0.4683019 0.03073875 -4.186223              0
#> clump4  0.4150943    0.4656604 0.03515881 -1.438218              6
#> even    0.5660377    0.4696226 0.03576872  2.695514            100
#> random  0.5094340    0.4647170 0.03722303  1.201326             80
#>         pd_obs_c_lower pd_obs_q pd_obs_p_upper pd_obs_p_lower
#> clump1             100      100           0.00           1.00
#> clump2a            100      100           0.00           1.00
#> clump2b            100      100           0.00           1.00
#> clump4              91      100           0.06           0.91
#> even                 0      100           1.00           0.00
#> random               4      100           0.80           0.04
```

This is a summary of the columns:

-   `*_obs`: Observed value
-   `*_obs_c_lower`: Count of times observed value was lower than random
    values
-   `*_obs_c_upper`: Count of times observed value was higher than
    random values
-   `*_obs_p_lower`: Percentage of times observed value was lower than
    random values
-   `*_obs_p_upper`: Percentage of times observed value was higher than
    random values
-   `*_obs_q`: Count of the non-NA random values used for comparison
-   `*_obs_z`: Standard effect size (z-score)
-   `*_rand_mean`: Mean of the random values
-   `*_rand_sd`: Standard deviation of the random values

The next step in CANAPE is to classify endemism types according to the
significance of PE, alternative PE, and relative PE. This adds a column
called `endem_type`.

``` r
canape_results <- cpr_classify_endem(rand_test_results)

canape_results[, "endem_type", drop = FALSE]
#>              endem_type
#> clump1  not significant
#> clump2a not significant
#> clump2b not significant
#> clump4  not significant
#> even              super
#> random            super
```

This data set is very small, so it doesn’t include all possible endemism
types. In total, they include:

-   `paleo`: paleoendemic
-   `neo`: neoendemic
-   `not significant` (what it says)
-   `mixed`: mixture of both paleo and neo
-   `super`: mixed and highly significant (*p* &lt; 0.01)

For a more complete example, please [see the
vignette](https://joelnitta.github.io/canaper/articles/canape.html)

## References

Mishler, B., Knerr, N., González-Orozco, C. *et al*. Phylogenetic
measures of biodiversity and neo- and paleo-endemism in Australian
*Acacia*. *Nat Commun* 5, 4473 (2014).
<https://doi.org/10.1038/ncomms5473>
