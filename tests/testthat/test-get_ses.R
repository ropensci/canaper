# Make objects used across multiple tests ----

phy <- biod_example$phy
comm <- biod_example$comm
phy_alt <- phy
phy_alt$edge.length <- rep(x = 1, times = length(phy_alt$edge.length))
phy_alt$edge.length <- phy_alt$edge.length / sum(phy_alt$edge.length)
phy$edge.length <- phy$edge.length / sum(phy$edge.length)
random_vals <-
  purrr::map(
    1:100,
    ~ calc_biodiv_random(comm, phy, phy_alt, "curveball", 1000L, metrics = "pe")
  )
comm_sparse <- dense2sparse(comm)
pe_obs <- phylo_endemism(comm_sparse, phy, weighted = TRUE)

# Run tests ----

test_that("Input is valid", {
  expect_error(
    get_ses(random_vals, pe_obs, "pf"),
    "Biodiversity metrics may only be selected from 'pd', 'pd_alt', 'rpd', 'pe', 'pe_alt', or 'rpe'"
  )
  expect_error(
    get_ses(random_vals, purrr::set_names(pe_obs, ~ stringr::str_replace_all(., "0", "1")), "pe"),
    "Names don't match between 'obs_vals' and 'random_vals'"
  )
  expect_error(
    get_ses(random_vals, purrr::set_names(pe_obs, NULL), "pe"),
    "'obs_vals' must be named"
  )
  expect_error(
    get_ses(random_vals, pe_obs, "pd"),
    "Observed metric missing from random values"
  )
})

# Output should be a data.frame with 9 columns, with specific
# names and all of type "double"
test_that("Output is formatted as expected", {
  expect_s3_class(
    get_ses(random_vals, pe_obs, "pe"),
    "data.frame"
  )
  expect_length(
    get_ses(random_vals, pe_obs, "pe"),
    9
  )
  expect_named(
    get_ses(random_vals, pe_obs, "pe"),
    c(
      "pe_obs", "pe_rand_mean", "pe_rand_sd", "pe_obs_z", "pe_obs_c_upper",
      "pe_obs_c_lower", "pe_obs_q", "pe_obs_p_upper", "pe_obs_p_lower"
    )
  )
  for (i in 1:9) {
    expect_type(
      get_ses(random_vals, pe_obs, "pe")[[i]],
      "double"
    )
  }
})

# Cleanup ----

rm(phy, comm, phy_alt, random_vals, comm_sparse, pe_obs)
