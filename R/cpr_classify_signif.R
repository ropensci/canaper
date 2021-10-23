#' Classify statistical significance
#'
#' Given the results of [cpr_rand_test()], classifies statistical significance
#' of a biodiversity metric. The null hypothesis is that observed value does not
#' lie in the extreme of the random values.
#'
#' @srrstats {G1.3} defines terminology:
#' @details  For metrics like `pe`, you probably want to consider a one-sided
#'   hypothesis testing values in the upper extreme (i.e., we are interested in
#'   areas that have higher than expected endemism). For this, you would set
#'   `one_sided = TRUE, upper = TRUE`. For metrics like `pd`, you probably want
#'   to consider a two-sided hypothesis (i.e., we are interested in areas that
#'   are either more diverse or less than diverse than expected at random). For
#'   this, set `one_sided = FALSE`.
#'
#' @srrstats {G2.0a, G2.1a, G2.3b} Documents expectations on lengths, types of vector
#'   inputs, case-sensitivity
#' @param df  Input data frame.
#' @param metric Character vector of length 1; selected metric to classify
#'   significance. May choose from `pd` (phylogenetic diversity), `rpd`
#'   (relative phylogenetic diversity), `pe` (phylogenentic endemism), `rpe`
#'   (relative phylogenetic endemism) (case-sensitive).
#' @param one_sided Logical vector of length 1; is the null hypothesis
#'   one-sided? If `TRUE`, values will be classified as significant if they are
#'   in **either** the top 5% **or** bottom 5%. If `FALSE`, values will be
#'   classified as significant if they are in the top 2.5% or bottom 2.5%,
#'   combined.
#' @param upper Logical vector of length 1; only applies if `one_sided` is
#'   `TRUE`. If `TRUE`, values in the top 5% will be classified as significant.
#'   If `FALSE`, values in the bottom 5% will be classified as significant.
#'
#' @return Dataframe with column added for stastistical significance of the
#'   selected metric. The new column name is the name of the metric with
#'   `_signif` appendend. The new column is a character that may contain the
#'   following values, depending on the null hypothesis:
#'   - `< 0.01`, `< 0.025`, `> 0.975`, `> 0.99`, `not significant` (two-sided)
#'   - `< 0.01`, `< 0.05`, `> 0.99`, `> 0.95`, `not significant` (one-sided)
#'
#' @examples
#' set.seed(12345)
#' data(phylocom)
#' rand_test <- cpr_rand_test(
#'   phylocom$sample, phylocom$phy,
#'   null_model = "curveball", metrics = "pd")
#' cpr_classify_signif(rand_test, "pd")
#'
#' @srrstats {G1.4} uses roxygen
#'
#' @export
cpr_classify_signif <- function(df, metric, one_sided = FALSE, upper = FALSE) {

  # Check input
  #' @srrstats {G2.1, G2.6} Check input types and lengths
  #' @srrstats {G2.0, G2.2} assert input length is 1
  assertthat::assert_that(assertthat::is.string(metric))
  assertthat::assert_that(assertthat::noNA(metric))
  #' @srrstats {G2.3, G2.3a} # univariate char input
  assertthat::assert_that(
    metric %in% c("pd", "rpd", "pe", "rpe"),
    msg = "Biodiversity metrics may only be selected from 'pd', 'rpd', 'pe', or 'rpe'"
  )
  assertthat::assert_that(
    inherits(df, "data.frame"),
    msg = "'df' must be of class 'data.frame'")
  assertthat::assert_that(
    isTRUE(paste0(metric, "_obs_p_upper") %in% colnames(df)),
    msg = "'df' does not include percentage of times observed value was higher than random values"
  )
  assertthat::assert_that(
    isTRUE(paste0(metric, "_obs_p_lower") %in% colnames(df)),
    msg = "'df' does not include percentage of times observed value was lower than random values"
  )
  assertthat::assert_that(
    isTRUE(all(df[[paste0(metric, "_obs_p_lower")]] <= 1)),
    msg = "Values for percentage of times observed value was lower than random values should be between 0 and 1, inclusive"
  )
  assertthat::assert_that(
    isTRUE(all(df[[paste0(metric, "_obs_p_lower")]] >= 0)),
    msg = "Values for percentage of times observed value was lower than random values should be between 0 and 1, inclusive"
  )
  assertthat::assert_that(
    isTRUE(all(df[[paste0(metric, "_obs_p_upper")]] <= 1)),
    msg = "Values for percentage of times observed value was higher than random values should be between 0 and 1, inclusive"
  )
  assertthat::assert_that(
    isTRUE(all(df[[paste0(metric, "_obs_p_upper")]] >= 0)),
    msg = "Values for percentage of times observed value was higher than random values should be between 0 and 1, inclusive"
  )
  assertthat::assert_that(assertthat::is.flag(one_sided))
  assertthat::assert_that(assertthat::is.flag(upper))

  #' @srrstats {G3.0} use appropriate tolerances for approximate equality (see utils.R)
  if (!isTRUE(one_sided)) {
    signif <- dplyr::case_when(
      is.na(df[[paste0(metric, "_obs_p_lower")]]) ~ NA_character_,
      is.na(df[[paste0(metric, "_obs_p_upper")]]) ~ NA_character_,
      df[[paste0(metric, "_obs_p_lower")]] %greater% 0.99 ~ "< 0.01",
      df[[paste0(metric, "_obs_p_lower")]] %greater% 0.975 ~ "< 0.025",
      df[[paste0(metric, "_obs_p_upper")]] %greater% 0.99 ~ "> 0.99",
      df[[paste0(metric, "_obs_p_upper")]] %greater% 0.975 ~ "> 0.975",
      TRUE ~ "not significant"
    )
  } else {
    if (isTRUE(upper)) {
      signif <- dplyr::case_when(
        is.na(df[[paste0(metric, "_obs_p_upper")]]) ~ NA_character_,
        df[[paste0(metric, "_obs_p_upper")]] %greater% 0.99 ~ "> 0.99",
        df[[paste0(metric, "_obs_p_upper")]] %greater% 0.95 ~ "> 0.95",
        TRUE ~ "not significant"
      )
    } else {
      signif <- dplyr::case_when(
        is.na(df[[paste0(metric, "_obs_p_lower")]]) ~ NA_character_,
        df[[paste0(metric, "_obs_p_lower")]] %greater% 0.99 ~ "< 0.01",
        df[[paste0(metric, "_obs_p_lower")]] %greater% 0.95 ~ "< 0.05",
        TRUE ~ "not significant"
      )
    }
  }

  df[[paste0(metric, "_signif")]] <- signif

  df
}
