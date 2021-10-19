
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/master/examples>. -->

# canaper

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/359280907.svg)](https://zenodo.org/badge/latestdoi/359280907)
[![R-CMD-check](https://github.com/joelnitta/canaper/workflows/R-CMD-check/badge.svg)](https://github.com/joelnitta/canaper/actions)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Codecov test
coverage](https://codecov.io/gh/joelnitta/canaper/branch/main/graph/badge.svg)](https://codecov.io/gh/joelnitta/canaper?branch=main)
<!-- badges: end -->

The goal of canaper is to enable [categorical analysis of neo- and
paleo-endemism (CANAPE)](https://doi.org/10.1038/ncomms5473) in **R**.
This is the first implementation in **R** of CANAPE, which was
previously only available in
[Biodiverse](http://shawnlaffan.github.io/biodiverse/).

## Important note

**This package is in early development.** There may be major, breaking
changes to functionality in the near future. If you use this package, I
highly recommend using a package manager like
[renv](https://rstudio.github.io/renv/articles/renv.html) so that later
updates won’t break your code.

## Installation

You can install canaper from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("joelnitta/canaper")
```

## Example usage

These examples will use the dataset included with the `picante` package.
The dataset includes a community (site x species) matrix and a
pylogenetic tree.

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
set.seed(071421)
rand_test_results <- cpr_rand_test(phylocom$sample, phylocom$phy)
#> [1] "Dropping tips from the tree because they are not present in the community data:"
#> [1] "sp16" "sp23" "sp27" "sp28" "sp30" "sp31" "sp32"
```

`cpr_rand_test` produces **a lot** of columns (nine per metric), so
let’s just look at a subset of them:

``` r
rand_test_results[,1:9]
#>            pd_obs pd_rand_mean pd_rand_sd  pd_obs_z pd_obs_c_upper
#> clump1  0.3018868    0.4679245 0.02850761 -5.824331              0
#> clump2a 0.3207547    0.4703774 0.03130385 -4.779688              0
#> clump2b 0.3396226    0.4624528 0.03133141 -3.920353              0
#> clump4  0.4150943    0.4688679 0.03551447 -1.514132              4
#> even    0.5660377    0.4692453 0.03836478  2.522951            100
#> random  0.5094340    0.4698113 0.03086949  1.283553             85
#>         pd_obs_c_lower pd_obs_q pd_obs_p_upper pd_obs_p_lower
#> clump1             100      100           0.00           1.00
#> clump2a            100      100           0.00           1.00
#> clump2b            100      100           0.00           1.00
#> clump4              88      100           0.04           0.88
#> even                 0      100           1.00           0.00
#> random               7      100           0.85           0.07
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
#> even              mixed
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

## Other information

Poster at [Botany 2021](https://2021.botanyconference.org/)

-   [PDF](https://github.com/joelnitta/botany_poster_2021/raw/main/canaper.pdf)
-   [Video](https://www.youtube.com/watch?v=LXn_ziEli_Q)

## Citing this package

If you use this package, please cite it! Here is an example:

    Nitta JH, Iwasaki W. (2021) canaper: Categorical analysis of neo- and paleo-endemism in R. doi: 10.5281/zenodo.5094032

The example DOI above is for the overall package.

Here is the latest DOI, which you should use if you are using the latest
version of the package:

[![DOI](https://zenodo.org/badge/359280907.svg)](https://zenodo.org/badge/latestdoi/359280907)

You can find DOIs for older versions by viewing the “Releases” menu on
the right.

## Papers citing `canaper`

-   Nitta *et al.* “Spatial phylogenetics of Japanese ferns: Patterns,
    processes, and implications for conservation”. bioRxiv
    <https://doi.org/10.1101/2021.08.26.457744>

## Licenses

-   Code: [MIT](LICENSE.md)
-   Example datasets (`acacia`, `biod_example`): [GNU General Public
    License v3.0](data-raw/LICENSE.txt)

## References

Mishler, B., Knerr, N., González-Orozco, C. *et al*. Phylogenetic
measures of biodiversity and neo- and paleo-endemism in Australian
*Acacia*. *Nat Commun* 5, 4473 (2014).
<https://doi.org/10.1038/ncomms5473>
