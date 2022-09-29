
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/master/examples>. -->

# canaper <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/359280907.svg)](https://zenodo.org/badge/latestdoi/359280907)
[![R-CMD-check](https://github.com/ropensci/canaper/workflows/R-CMD-check/badge.svg)](https://github.com/ropensci/canaper/actions)
[![Project Status:
active.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Codecov test
coverage](https://codecov.io/gh/ropensci/canaper/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ropensci/canaper?branch=main)
[![ropensci
review](https://badges.ropensci.org/475_status.svg)](https://github.com/ropensci/software-review/issues/475)
![runiverse](https://ropensci.r-universe.dev/badges/canaper)
<!-- badges: end -->

The goal of `canaper` is to enable [categorical analysis of neo- and
paleo-endemism (CANAPE)](https://doi.org/10.1038/ncomms5473) in **R**.

## Installation

`canaper` is not currently available on CRAN.

You can install `canaper` from [r-universe](https://r-universe.dev)
with:

``` r
install.packages("canaper", repos = "https://ropensci.r-universe.dev")
```

or, you can install `canaper` from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("ropensci/canaper")
```

## Example usage

These examples use the dataset from
[Phylocom](https://phylodiversity.net/phylocom/). The dataset includes a
community (site x species) matrix and a phylogenetic tree.

``` r
library(canaper)

data(phylocom)

# Example community matrix including 4 "clumped" communities,
# one "even" community, and one "random" community
phylocom$comm
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
rand_test_results <- cpr_rand_test(
  phylocom$comm, phylocom$phy,
  null_model = "swap"
)
#> Warning in match_phylo_comm(phy = phy, comm = comm): Dropping tips from the tree because they are not present in the community data: 
#>  sp16, sp23, sp27, sp28, sp30, sp31, sp32
```

`cpr_rand_test` produces **a lot** of columns (nine per metric), so
let’s just look at a subset of them:

``` r
rand_test_results[, 1:9]
#>            pd_obs pd_rand_mean pd_rand_sd  pd_obs_z pd_obs_c_upper
#> clump1  0.3018868    0.4692453 0.03214267 -5.206739              0
#> clump2a 0.3207547    0.4762264 0.03263836 -4.763465              0
#> clump2b 0.3396226    0.4681132 0.03462444 -3.710978              0
#> clump4  0.4150943    0.4667925 0.03180131 -1.625660              3
#> even    0.5660377    0.4660377 0.03501739  2.855724            100
#> random  0.5094340    0.4733962 0.03070539  1.173662             79
#>         pd_obs_c_lower pd_obs_q pd_obs_p_upper pd_obs_p_lower
#> clump1             100      100           0.00           1.00
#> clump2a            100      100           0.00           1.00
#> clump2b            100      100           0.00           1.00
#> clump4              91      100           0.03           0.91
#> even                 0      100           1.00           0.00
#> random               6      100           0.79           0.06
```

This is a summary of the columns:

- `*_obs`: Observed value
- `*_obs_c_lower`: Count of times observed value was lower than random
  values
- `*_obs_c_upper`: Count of times observed value was higher than random
  values
- `*_obs_p_lower`: Percentage of times observed value was lower than
  random values
- `*_obs_p_upper`: Percentage of times observed value was higher than
  random values
- `*_obs_q`: Count of the non-NA random values used for comparison
- `*_obs_z`: Standard effect size (z-score)
- `*_rand_mean`: Mean of the random values
- `*_rand_sd`: Standard deviation of the random values

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
#> random            mixed
```

This data set is very small, so it doesn’t include all possible endemism
types. In total, they include:

- `paleo`: paleoendemic
- `neo`: neoendemic
- `not significant` (what it says)
- `mixed`: mixture of both paleo and neo
- `super`: mixed and highly significant (*p* \< 0.01)

For a more complete example, please [see the
vignette](https://docs.ropensci.org/canaper/articles/canape.html)

## Comparsion with other software

Several other R packages are available to calculate diversity metrics
for ecological communities. The non-exhaustive summary below focuses on
alpha diversity metrics in comparison with `canaper`, and is **not** a
comprehensive description of each package.

- [PhyloMeasures](https://doi.org/10.1111/ecog.01814): Calculates
  phylogenetic community diversity metrics including MPD, MNTD, PD,
  phylosor, and unifrac. Null models for matrix randomization include
  `uniform`, `frequency.by.richness`, and `sequential`.
- [phyloregion](https://github.com/darunabas/phyloregion): Calculates PD
  but not MPD or MNTD. Implements sparse matrix encoding to increase
  computing speed, which is used by `canaper`. Null models for matrix
  randomization include `tipshuffle`, `rowwise`, and `colwise`. Also
  performs regionalization based on taxonomic or phylogenetic beta
  diversity.
- [picante](https://github.com/skembel/picante): Calculates MPD, MNTD,
  PD, etc. Null models for community matrix randomization include
  `frequency`, `richness`, `independentswap`, and `trialswap`.
- [vegan](https://github.com/vegandevs/vegan): Performs a large range of
  mostly non-phylogenetic diversity analyses. Includes the largest
  selection of null models (\> 20), according to data type (binary
  vs. quantitative). `canaper` uses `vegan` to randomize community
  matrices.
- [biodiverse](http://shawnlaffan.github.io/biodiverse/): Not an R
  package, but software written in perl with a GUI. Performs all of the
  calculations needed for CANAPE, and many other metrics (\> 300).
  Includes `rand_structured` null model as well as spatially structured
  null models. None of these null models are currently available in any
  R packages AFAIK, except for `independentswap`.

## Other information

Poster at [Botany 2021](https://2021.botanyconference.org/)

- [PDF](https://github.com/joelnitta/botany_poster_2021/raw/main/canaper.pdf)
- [Video](https://www.youtube.com/watch?v=LXn_ziEli_Q)

## Citing this package

If you use this package, please cite it! Here is an example:

- Nitta JH, Laffan SW, Mishler BD, Iwasaki W. (2021) canaper:
  Categorical analysis of neo- and paleo-endemism in R. doi:
  10.5281/zenodo.5094032

The example DOI above is for the overall package.

Here is the latest DOI, which you should use if you are using the latest
version of the package:

[![DOI](https://zenodo.org/badge/359280907.svg)](https://zenodo.org/badge/latestdoi/359280907)

You can find DOIs for older versions by viewing the “Releases” menu on
the right.

## Papers citing `canaper`

- Ellepola *et al.* 2022. “The role of climate and islands in species
  diversification and reproductive-mode evolution of Old World tree
  frogs”. *Communications Biology* 5, 347
  <https://doi.org/10.1038/s42003-022-03292-1>
- Lu *et al.* 2022 “A comprehensive evaluation of flowering plant
  diversity and conservation priority for national park planning in
  China”. *Fundamental Research*
  <https://doi.org/10.1016/j.fmre.2022.08.008>
- Nitta *et al.* 2022 “Spatial phylogenetics of Japanese ferns:
  Patterns, processes, and implications for conservation”. *American
  Journal of Botany* 109, 727-745 <https://doi.org/10.1002/ajb2.1848>

## Contributing and code of conduct

Contributions to `canaper` are welcome! For more information, please see
[CONTRIBUTING.md](.github/CONTRIBUTING.md)

Please note that this package is released with a [Contributor Code of
Conduct](https://ropensci.org/code-of-conduct/). By contributing to this
project, you agree to abide by its terms.

## Licenses

- Code: [MIT](LICENSE.md)
- Example datasets
  - `acacia`, `biod_example`: [GNU General Public License
    v3.0](https://github.com/ropensci/canaper/blob/main/data-raw/LICENSE-gpl-3.txt)
  - `phylocom`:
    [BSD-3-Clause](https://github.com/ropensci/canaper/blob/main/data-raw/LICENSE-bsd3.txt)

## References

Mishler, B., Knerr, N., González-Orozco, C. *et al*. Phylogenetic
measures of biodiversity and neo- and paleo-endemism in Australian
*Acacia*. *Nat Commun* 5, 4473 (2014).
<https://doi.org/10.1038/ncomms5473>
