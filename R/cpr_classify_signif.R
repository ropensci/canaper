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
    isTRUE(all(metric %in% c("pd", "pd_alt", "rpd", "pe", "pe_alt", "rpe"))),
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
