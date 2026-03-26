#' Convert parquet data frame to randomization list format
#'
#' Internal helper function to convert parquet data (long format) back to
#' the list structure used by cpr_rand_test()
#'
#' @param rand_df Data frame from parquet with columns: site, replicate,
#'   and metric columns
#' @param sites Character vector of site names in expected order
#' @param n_reps Number of replicates
#'
#' @return List of lists, where each element corresponds to one replicate
#'   and contains named vectors of metric values
#'
#' @noRd
parquet_to_rand_list <- function(rand_df, sites, n_reps) {
  # Get metric column names (exclude site and replicate)
  metric_cols <- setdiff(colnames(rand_df), c("site", "replicate"))

  # Convert to list format expected by cpr_rand_test
  rand_list <- lapply(seq_len(n_reps), function(rep_num) {
    # Extract data for this replicate
    rep_data <- rand_df[rand_df$replicate == rep_num, ]

    # Ensure sites are in correct order
    rep_data <- rep_data[match(sites, rep_data$site), ]

    # Extract metrics into named list
    metric_list <- lapply(metric_cols, function(metric) {
      vals <- rep_data[[metric]]
      names(vals) <- rep_data$site # Set site names on the vector
      vals
    })
    names(metric_list) <- metric_cols

    metric_list
  })

  rand_list
}
