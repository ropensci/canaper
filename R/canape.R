
#' Calculate diversity metrics for a single random community
#'
#' The independent swap method of Gotelli (2000) is used, which randomizes
#' the community matrix while maintaining species occurrence frequency and
#' sample species richness.
#'
#' For description of metrics available, see run_ses_analysis()
#'
#' @param comm Input community matrix in data.frame format (communities as rows,
#' species as columns, with row names and column names)
#' @param phy Input phylogeny with total branch length scaled to 1
#' @param phy_alt Alternative phylogeny where all branches are of equal length, scaled to 1
#' @param n_iterations Number of iterations to use when shuffling random community
#' @param metrics Character vector; names of metrics to calculate
#'
#' @return List of vectors. Each vector is a biodiversity metric measured on the
#' random community, in the same order as the rows in the input community.
#'
#' @examples
#' \dontrun{
#' library(picante)
#' data(phylocom)
#' phy <- phylocom$phy
#' comm <- phylocom$sample
#' subsetted_data <- picante::match.phylo.comm(phy = phy, comm = comm)
#' phy <- subsetted_data[["phy"]]
#' comm <- subsetted_data[["comm"]]
#' phy_alt <- phy
#' phy_alt$edge.length <- rep(x = 1, times = length(phy_alt$edge.length))
#' phy_alt$edge.length <- phy_alt$edge.length / sum(phy_alt$edge.length)
#' phy$edge.length <- phy$edge.length / sum(phy$edge.length)
#' calc_biodiv_random(comm, phy, phy_alt, "independentswap", metrics = "pd")
#' }
#' @keywords internal
calc_biodiv_random <- function(comm, phy, phy_alt,
                               null_model = c("frequency", "richness", "independentswap", "trialswap"),
                               n_iterations = 1000, metrics) {
  assertthat::assert_that(assertthat::is.string(null_model))
  assertthat::assert_that(
    null_model %in% c("frequency", "richness", "independentswap", "trialswap"),
    msg = "'null_model' must be one of 'frequency', 'richness', 'independentswap', or 'trialswap'"
  )

  # Make sure names match between community and tree
  assertthat::assert_that(isTRUE(
    all.equal(sort(phy$tip.label), sort(colnames(comm)))
  ))

  assertthat::assert_that(isTRUE(
    all.equal(sort(phy_alt$tip.label), sort(colnames(comm)))
  ))

  # Make sure phylogeny has been rescaled to total branch length of 1 for RPE or RFD
  if (any(metrics %in% c("rpe", "rpd"))) assertthat::assert_that(isTRUE(all.equal(sum(phy$edge.length), 1)))
  if (any(metrics %in% c("rpe", "rpd"))) assertthat::assert_that(isTRUE(all.equal(sum(phy_alt$edge.length), 1)))

  # Convert comm to sparse matrix format for phyloregions
  comm_sparse <- phyloregion::dense2sparse(comm)

  # Generate random community
  random_comm <- picante::randomizeMatrix(comm, null.model = null_model, iterations = n_iterations)
  random_comm_sparse <- phyloregion::dense2sparse(random_comm)

  # Calculate statistics for random community
  # - set up null vectors first
  pd <- NULL
  pd_alt <- NULL
  rpd <- NULL
  pe <- NULL
  pe_alt <- NULL
  rpe <- NULL

  # - calculate selected metrics
  if ("pd" %in% metrics) pd <- phyloregion::PD(random_comm_sparse, phy)
  if ("rpd" %in% metrics) {
    pd_alt <- phyloregion::PD(random_comm_sparse, phy_alt)
    rpd <- pd / pd_alt
  }
  if ("pe" %in% metrics) pe <- phyloregion::phylo_endemism(random_comm_sparse, phy, weighted = TRUE)
  if ("rpe" %in% metrics) {
    pe_alt <- phyloregion::phylo_endemism(random_comm_sparse, phy_alt, weighted = TRUE)
    rpe <- pe / pe_alt
  }

  # Output results, only keep non-NULL results
  purrr::compact(
    list(
      pd = pd,
      pd_alt = pd_alt,
      rpd = rpd,
      pe = pe,
      pe_alt = pe_alt,
      rpe = rpe
    )
  )
}

#' Extract standard effect size (and other related statistics) for a single
#' diversity metric given random values and observed values of the metric
#'
#' @param random_vals List of list of vectors. Each list of vectors is a biodiversity metric measured on a
#' random community, in the same order as the rows in the input community.
#' @param obs_vals Observed values of the biodiversity metric
#' @param metric Name of the metric ("mpd", "mntd", "mpd_morph", "mntd_morph", "pd", "pe", or "rpe")
#'
#' @return Tibble
#' @examples
#' \dontrun{
#' library(picante)
#' data(phylocom)
#' phy <- phylocom$phy
#' comm <- phylocom$sample
#' subsetted_data <- picante::match.phylo.comm(phy = phy, comm = comm)
#' phy <- subsetted_data[["phy"]]
#' comm <- subsetted_data[["comm"]]
#' phy_alt <- phy
#' phy_alt$edge.length <- rep(x = 1, times = length(phy_alt$edge.length))
#' phy_alt$edge.length <- phy_alt$edge.length / sum(phy_alt$edge.length)
#' phy$edge.length <- phy$edge.length / sum(phy$edge.length)
#' random_vals <-
#'   purrr::map(
#'     1:100,
#'     ~ calc_biodiv_random(comm, phy, phy_alt, "independentswap", 1000, metrics = "pe")
#'   )
#' comm_sparse <- phyloregion::dense2sparse(comm)
#' pe_obs <- phyloregion::phylo_endemism(comm_sparse, phy, weighted = TRUE)
#' get_ses(random_vals, pe_obs, "pe")
#' }
#' @autoglobal
#' @keywords internal
get_ses <- function(random_vals, obs_vals, metric) {
  assertthat::assert_that(assertthat::is.string(metric))

  assertthat::assert_that(
    all(metric %in% c("pd", "pd_alt", "rpd", "pe", "pe_alt", "rpe")),
    msg = "Biodiversity metrics may only be selected from 'pd', 'rpd', 'pe', or 'rpe'"
  )

  random_vals_trans <- purrr::transpose(random_vals)

  results <-
    tibble::tibble(
      random_values = purrr::map(
        purrr::transpose(random_vals_trans[[metric]]),
        purrr::as_vector
      ),
      obs_val = obs_vals
    )

  results <- dplyr::transmute(
    results,
    obs = obs_val,
    # Calculate SES
    rand_mean = purrr::map_dbl(random_values, ~ mean(., na.rm = TRUE)),
    rand_sd = purrr::map_dbl(random_values, ~ sd(., na.rm = TRUE)),
    obs_z = (obs_val - rand_mean) / rand_sd,
    # Count number of times observed value is higher than random values
    obs_c_upper = purrr::map2_dbl(.x = obs_val, .y = random_values, ~ count_higher(.x, .y)),
    # Count number of times observed value is lower than random values
    obs_c_lower = purrr::map2_dbl(.x = obs_val, .y = random_values, ~ count_lower(.x, .y)),
    # Count the number of non-NA random values used for comparison
    obs_q = purrr::map_dbl(random_values, ~ length(magrittr::extract(., !is.na(.)))),
    # Calculate p-value for upper tail
    obs_p_upper = obs_c_upper / obs_q,
    # Calculate p-value for lower tail
    obs_p_lower = obs_c_lower / obs_q
  )

  colnames(results) <- paste(metric, colnames(results), sep = "_")

  results
}

#' Run a randomization analysis for one or more biodiversity metrics
#'
#' The observed value of the biodiversity metric(s) will be calculated, then
#' compared against a set of random communities. Various statistics are calculated
#' from the comparison (see **Value** below).
#'
#' The biodiversity metrics available for analysis include:
#' - `pd`: Phylogenetic diversity (Faith 1992)
#' - `rpd`: Relative phylogenetic diversity (Mishler et al 2014)
#' - `pe`: Phylogenetic endemism (Rosauer et al 2009)
#' - `rpe`: Relative phylogenetic endemism (Mishler et al 2014)
#'
#' The default method for generating random communities is the independent swap
#' method of Gotelli (2000), which randomizes the community matrix while maintaining
#' species occurrence frequency and sample species richness.
#'
#' @param comm Dataframe; input community matrix with communities as rows
#' and species as columns, including row names and column names.
#' @param phy List of class `phylo`; input phylogeny.
#' @param null_model Name of null model to use. Must choose from `frequency`, `richness`,
#' `independentswap`, or `trialswap.` For details, see [picante::randomizeMatrix()].
#' @param n_reps Number of random communities to replicate.
#' @param n_iterations Number of iterations to use when swapping occurrences to
#' generate each random community; only used if `null_model` is 'independentswap'
#' or 'trialswap'.
#' @param metrics Character vector; names of biodiversity metrics to calculate.
#' May include one or more of: `pd`, `rpd`, `pe`, `rpe`.
#'
#' @return Dataframe. For each of the biodiversity metrics, the following 9 columns
#' will be produced:
#' - `*_obs`: Observed value
#' - `*_obs_c_lower`: Count of times observed value was lower than random values
#' - `*_obs_c_upper`: Count of times observed value was higher than random values
#' - `*_obs_p_lower`: Percentage of times observed value was lower than random values
#' - `*_obs_p_upper`: Percentage of times observed value was higher than random values
#' - `*_obs_q`: Count of the non-NA random values used for comparison
#' - `*_obs_z`: Standard effect size (z-score)
#' - `*_rand_mean`: Mean of the random values
#' - `*_rand_sd`: Standard deviation of the random values
#'
#' So if you included `pd` in `metrics`, the output columns would include `pd_obs`,
#' `pd_obs_c_lower`, etc...
#'
#' @source Faith DP (1992) Conservation evaluation and phylogenetic diversity.
#'  Biological Conservation, 61:1–10. <https://doi.org/10.1016/0006-3207(92)91201-3>
#' @source Gotelli, N.J. (2000) Null Model Analysis of Species Co-Occurrence
#' Patterns. Ecology, 81: 2606-2621. <https://doi.org/10.1890/0012-9658(2000)081[2606:NMAOSC]2.0.CO;2>
#' @source Rosauer, D., Laffan, S.W., Crisp, M.D., Donnellan, S.C. and Cook, L.G. (2009)
#' Phylogenetic endemism: a new approach for identifying geographical concentrations of
#' evolutionary history. Molecular Ecology, 18: 4061-4072. https://doi.org/10.1111/j.1365-294X.2009.04311.x
#' @source Mishler, B., Knerr, N., González-Orozco, C. et al.  (2014) Phylogenetic measures
#' of biodiversity and neo- and paleo-endemism in Australian Acacia.
#' Nat Commun, 5: 4473. <https://doi.org/10.1038/ncomms5473>
#'
#' @examples
#' library(picante)
#' data(phylocom)
#' cpr_rand_test(phylocom$sample, phylocom$phy, metrics = "pd")
#' @export
cpr_rand_test <- function(comm, phy, null_model = "independentswap", n_reps = 100, n_iterations = 10000, metrics = c("pd", "rpd", "pe", "rpe")) {

  # Check input
  assertthat::assert_that(inherits(comm, "data.frame"),
    msg = "'comm' must be of class 'data.frame'")
  assertthat::assert_that(
    is.list(phy) && inherits(phy, "phylo"),
    msg = "'phy' must be a list of class 'phylo'")
  assertthat::assert_that(assertthat::is.string(null_model))
  assertthat::assert_that(
    null_model %in% c("frequency", "richness", "independentswap", "trialswap"),
    msg = "'null_model' must be one of 'frequency', 'richness', 'independentswap', or 'trialswap'"
  )
  assertthat::assert_that(assertthat::is.number(n_reps))
  assertthat::assert_that(assertthat::is.number(n_iterations))
  assertthat::assert_that(is.character(metrics))
  assertthat::assert_that(
    metrics %in% c("pd", "rpd", "pe", "rpe"),
    msg = "'metrics' may only include 'pd', 'rpd', 'pe', or 'rpe'"
  )

  # Match tips of tree and column names of community data frame:
  # Use only taxa that are in common between phylogeny and community
  subsetted_data <- picante::match.phylo.comm(phy = phy, comm = comm)
  phy <- subsetted_data[["phy"]]
  comm <- subsetted_data[["comm"]]

  assertthat::assert_that(
    isTRUE(
      all.equal(
        sort(colnames(comm)),
        sort(phy$tip.label)
      )
    ),
    msg = "Tip names don't match between community and phylogeny"
  )

  # Make alternative tree with equal branch lengths
  phy_alt <- phy
  phy_alt$edge.length <- rep(x = 1, times = length(phy_alt$edge.length))
  # rescale so total phy length is 1
  phy_alt$edge.length <- phy_alt$edge.length / sum(phy_alt$edge.length)
  # rescale original phy so total length is 1
  phy$edge.length <- phy$edge.length / sum(phy$edge.length)

  # Make sparse community df
  comm_sparse <- phyloregion::dense2sparse(comm)

  # Calculate biodiversity metrics for random communities
  # set up a progress bar
  # pb <- progress::progress_bar$new(total = 100)
  pb <- progressr::progressor(steps = n_reps)

  random_vals <-
    furrr::future_map(
      1:n_reps,
      ~ {
        pb()
        calc_biodiv_random(
          comm, phy, phy_alt,
          null_model = null_model,
          n_iterations = n_iterations,
          metrics = metrics
        )
      },
      .options = furrr::furrr_options(seed = TRUE)
    )

  # Calculate biodiversity metrics for observed community
  # - set up null vectors first
  ses_pd <- NULL
  ses_pd_alt <- NULL
  ses_rpd <- NULL
  ses_pe <- NULL
  ses_pe_alt <- NULL
  ses_rpe <- NULL

  # - calculate selected metrics
  if ("pd" %in% metrics) {
    pd_obs <- phyloregion::PD(comm_sparse, phy)
    ses_pd <- get_ses(random_vals, pd_obs, "pd")
  }

  if ("rpd" %in% metrics) {
    pd_alt_obs <- phyloregion::PD(comm_sparse, phy_alt)
    ses_pd_alt <- get_ses(random_vals, pd_alt_obs, "pd_alt")
    rpd_obs <- pd_obs / pd_alt_obs
    ses_rpd <- get_ses(random_vals, rpd_obs, "rpd")
  }

  if ("pe" %in% metrics) {
    pe_obs <- phyloregion::phylo_endemism(comm_sparse, phy, weighted = TRUE)
    ses_pe <- get_ses(random_vals, pe_obs, "pe")
  }

  if ("rpe" %in% metrics) {
    pe_alt_obs <- phyloregion::phylo_endemism(comm_sparse, phy_alt, weighted = TRUE)
    ses_pe_alt <- get_ses(random_vals, pe_alt_obs, "pe_alt")
    rpe_obs <- pe_obs / pe_alt_obs
    ses_rpe <- get_ses(random_vals, rpe_obs, "rpe")
  }

  # Combine results
  results <- dplyr::bind_cols(
    ses_pd,
    ses_pd_alt,
    ses_rpd,
    ses_pe,
    ses_pe_alt,
    ses_rpe
  )

  results <- dplyr::mutate(results, site = rownames(comm))

  tibble::column_to_rownames(results, "site")
}

#' Classify phylogenetic endemism
#'
#' Given the results of [cpr_rand_test()], classifies phylogenetic endemism according to
#' CANAPE scheme of Mishler 2014.
#'
#' For a summary of the classification scheme, see:
#' <http://biodiverse-analysis-software.blogspot.com/2014/11/canape-categorical-analysis-of-palaeo.html>
#'
#' @param df Input data frame. Must have the following columns:
#' - `pe_obs_p_upper`: Upper *p*-value comparing observed phylogenetic endemism to random values
#' - `pe_alt_obs_p_upper`: Upper *p*-value comparing observed phylogenetic endemism on alternate tree to random values
#' - `rpe_obs_p_upper`: Upper *p*-value comparing observed relative phylogenetic endemism to random values
#'
#' @return Dataframe with column `endem_type` (character) added. Values of `endem_type` type
#' include `paleo` (paleoendemic), `neo` (neoendemic), `not significant` (what it says), `mixed` (mixed endemism),
#' and `super` (super-endemic; both `pe_obs` and `pe_obs_alt` are highly significant).
#'
#' @source Mishler, B., Knerr, N., González-Orozco, C. et al.  (2014) Phylogenetic measures
#' of biodiversity and neo- and paleo-endemism in Australian Acacia.
#' Nat Commun, 5: 4473. <https://doi.org/10.1038/ncomms5473>
#'
#' @examples
#' library(picante)
#' data(phylocom)
#' rand_test <- cpr_rand_test(phylocom$sample, phylocom$phy, metrics = c("pe", "rpe"))
#' cpr_classify_endem(rand_test)
#' @export
cpr_classify_endem <- function(df) {
  dplyr::mutate(
    df,
    # Categorize endemism by CANAPE scheme
    # (here, PE_orig = pe_obs_p, PE_alt = pe_alt_obs_p, and RPE = rpe_obs_p)
    #
    # 1)    If either PE_orig or PE_alt are significantly high then we look for palaeo or neo endemism
    #   a)    If RPE is significantly high then we have palaeo-endemism
    #         (PE_orig is consistently higher than PE_alt across the random realisations)
    #   b)    Else if RPE is significantly low then we have neo-endemism
    #         (PE_orig is consistently lower than PE_alt across the random realisations)
    #     c)    Else we have mixed age endemism in which case
    #        i)    If both PE_orig and PE_alt are highly significant (p<0.01) then we
    #              have super endemism (high in both palaeo and neo)
    #        ii)   Else we have mixed (some mixture of palaeo, neo and non endemic)
    # 2)    Else if neither PE_orig or PE_alt are significantly high then we have a non-endemic cell
    endem_type = dplyr::case_when(
      (pe_obs_p_upper >= 0.95 | pe_alt_obs_p_upper >= 0.95) & rpe_obs_p_upper >= 0.975 ~ "paleo",
      (pe_obs_p_upper >= 0.95 | pe_alt_obs_p_upper >= 0.95) & rpe_obs_p_lower >= 0.975 ~ "neo",
      pe_obs_p_upper >= 0.99 | pe_alt_obs_p_upper >= 0.99 ~ "super",
      pe_obs_p_upper >= 0.95 | pe_alt_obs_p_upper >= 0.95 ~ "mixed",
      TRUE ~ "not significant"
    )
  )
}

#' Classify statistical significance
#'
#' Given the results of [cpr_rand_test()], classifies statistical significance
#' of a biodiversity metric. The null hypothesis is that observed value does not
#' lie in the extreme of the random values.
#'
#' @details  For metrics like `pe`, you probably want to consider a one-sided
#' hypothesis testing values in the upper extreme (i.e., we are interested in
#' areas that have higher than expected endemism). For this, you would set
#' `one_sided = TRUE, upper = TRUE`. For metrics like `pd`, you
#' probably want to consider a two-sided hypothesis (i.e., we are interested in
#' areas that are either more diverse or less than diverse than expected at
#' random). For this, set `one_sided = FALSE`.
#'
#' @param df  Input data frame.
#' @param metric Selected metric to classify significance. May choose from
#' `pd` (phylogenetic diversity), `rpd` (relative phylogenetic diversity),
#' `pe` (phylogenentic endemism), `rpe` (relative phylogenetic endemism).
#' @param one_sided Logical; is the null hypothesis one-sided? If `TRUE`, values
#' will be classified as significant if they are in **either** the top 5% **or**
#' bottom 5%. If `FALSE`, values will be classified as significant if they
#' are in the top 2.5% or bottom 2.5%, combined.
#' @param upper Logical; only applies if `one_sided` is `TRUE`. If `TRUE`,
#' values in the top 5% will be classified as significant. If `FALSE`, values
#' in the bottom 5% will be classified as significant.
#'
#' @return Dataframe with column added for stastistical significance of the
#' selected metric. The new column name is the name of the metric with
#' `_signif` appendend. The new column is a character that may contain the
#' following values, depending on the null hypothesis:
#' - `< 0.01`, `< 0.025`, `> 0.99`, `> 0.99`, `not significant` (two-sided)
#' - `< 0.01`, `< 0.05`, `> 0.99`, `> 0.95`, `not significant` (one-sided)
#'
#' @examples
#' library(picante)
#' data(phylocom)
#' rand_test <- cpr_rand_test(phylocom$sample, phylocom$phy, metrics = "pd")
#' cpr_classify_signif(rand_test, "pd")
#' @export
cpr_classify_signif <- function(df, metric, one_sided = FALSE, upper = FALSE) {
  assertthat::assert_that(
    all(metric %in% c("pd", "pd_alt", "rpd", "pe", "pe_alt", "rpe")),
    msg = "Biodiversity metrics may only be selected from 'pd', 'rpd', 'pe', or 'rpe'"
  )

  df[[paste0(metric, "_obs_p_lower")]]

  if (!isTRUE(one_sided)) {
    signif <- dplyr::case_when(
      df[[paste0(metric, "_obs_p_lower")]] > 0.99 ~ "< 0.01",
      df[[paste0(metric, "_obs_p_lower")]] > 0.975 ~ "< 0.025",
      df[[paste0(metric, "_obs_p_upper")]] > 0.99 ~ "> 0.99",
      df[[paste0(metric, "_obs_p_upper")]] > 0.975 ~ "> 0.975",
      TRUE ~ "not significant"
    )
  } else {
    if (isTRUE(upper)) {
      signif <- dplyr::case_when(
        df[[paste0(metric, "_obs_p_upper")]] > 0.99 ~ "> 0.99",
        df[[paste0(metric, "_obs_p_upper")]] > 0.95 ~ "> 0.95",
        TRUE ~ "not significant"
      )
    } else {
      signif <- dplyr::case_when(
        df[[paste0(metric, "_obs_p_lower")]] > 0.99 ~ "< 0.01",
        df[[paste0(metric, "_obs_p_lower")]] > 0.95 ~ "< 0.05",
        TRUE ~ "not significant"
      )
    }
  }

  df[[paste0(metric, "_signif")]] <- signif

  df
}
