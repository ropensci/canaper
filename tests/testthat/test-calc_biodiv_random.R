# Make objects used across multiple tests ----

phy <- biod_example$phy
comm <- biod_example$comm
phy_alt <- phy
phy_alt$edge.length <- rep(x = 1, times = length(phy_alt$edge.length)) # nolint
phy_alt$edge.length <- phy_alt$edge.length / sum(phy_alt$edge.length) # nolint
phy$edge.length <- phy$edge.length / sum(phy$edge.length) # nolint

# Run tests ----

test_that("Random seeds work", {
  set.seed(12345)
  res1 <- calc_biodiv_random(comm, phy, phy_alt, "curveball", 100L)
  set.seed(67890)
  res2 <- calc_biodiv_random(comm, phy, phy_alt, "curveball", 100L)
  # Should be able to pass seed as an argument
  set.seed(42)
  res3 <- calc_biodiv_random(
    comm, phy, phy_alt, "curveball", 100L,
    seed = 12345
  )
  res4 <- calc_biodiv_random(
    comm, phy, phy_alt, "curveball", 100L,
    seed = 67890
  )
  # Different seeds should give different results
  expect_false(isTRUE(all.equal(res1, res2)))
  expect_false(isTRUE(all.equal(res3, res4)))
  # Same seeds should give same results
  expect_true(isTRUE(all.equal(res1, res3)))
  expect_true(isTRUE(all.equal(res2, res4)))
})

test_that("Random seeds work in parallel", {
  # If run without sourcing files in R/, get error:
  # Error in calc_biodiv_random(comm, phy, phy_alt, "curveball", 100L) :
  # 	could not find function "calc_biodiv_random"

  # Change back to sequential when done (including on failure)
  on.exit(future::plan(future::sequential), add = TRUE)

  # Set future resolution to parallelized, with 2 workers
  future::plan(future::multisession, workers = 2)

  set.seed(12345)
  res1 <- future.apply::future_lapply(
    1:2,
    function(x) {
      calc_biodiv_random(comm, phy, phy_alt, "curveball", 100L)
    },
    future.seed = TRUE
  )
  set.seed(67890)
  res2 <- future.apply::future_lapply(
    1:2,
    function(x) {
      calc_biodiv_random(comm, phy, phy_alt, "curveball", 100L)
    },
    future.seed = TRUE
  )
  # Should be able to pass seed as an argument
  set.seed(12345)
  res3 <- future.apply::future_lapply(
    1:2,
    function(x) {
      calc_biodiv_random(comm, phy, phy_alt, "curveball", 100L)
    },
    future.seed = TRUE
  )
  # Different seeds should give different results
  expect_false(isTRUE(all.equal(res1, res2)))
  # Same seeds should give same results
  expect_true(isTRUE(all.equal(res1, res3)))

  # Change back to sequential
  future::plan(future::sequential)
})

test_that("Output is formatted as expected", {
  #' @srrstats {G5.3} check that output has no missing values
  expect_true(
    all(
      assertr::not_na(
        unlist(calc_biodiv_random(comm, phy, phy_alt, "swap", 1L))
      )
    )
  )
})

test_that("Selected metric shows up in output", {
  expect_named(calc_biodiv_random(
    comm, phy, phy_alt, "swap", 10L,
    metrics = "pd"
  ), "pd")
  expect_named(calc_biodiv_random(
    comm, phy, phy_alt, "swap", 10L,
    metrics = "pe"
  ), "pe")
  expect_named(calc_biodiv_random(
    comm, phy, phy_alt, "swap", 10L,
    metrics = "rpd"
  ),
  c("pd", "pd_alt", "rpd"),
  ignore.order = TRUE
  )
  expect_named(calc_biodiv_random(
    comm, phy, phy_alt, "swap", 10L,
    metrics = "rpe"
  ),
  c("pe", "pe_alt", "rpe"),
  ignore.order = TRUE
  )
  expect_named(
    calc_biodiv_random(
      comm, phy, phy_alt, "swap", 10L,
      metrics = c("rpd", "rpe")
    ),
    c("pd", "pd_alt", "rpd", "pe", "pe_alt", "rpe"),
    ignore.order = TRUE
  )
  expect_named(
    calc_biodiv_random(
      comm, phy, phy_alt, "swap", 10L,
      metrics = c("pd", "pe")
    ),
    c("pd", "pe"),
    ignore.order = TRUE
  )
  expect_named(
    calc_biodiv_random(
      comm, phy, phy_alt, "swap", 10L,
      metrics = c("pd", "pe", "pd_alt")
    ),
    c("pd", "pe", "pd_alt"),
    ignore.order = TRUE
  )
})

# Cleanup ----

rm(phy, comm, phy_alt)
