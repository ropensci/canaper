library(picante)
data(phylocom)
rand_test <- cpr_rand_test(phylocom$sample, phylocom$phy, metrics = c("pe", "rpe"))
dummy_data <- data.frame(
	pe_obs_p_upper =     c(0.99, 0.10, 0.999, 0.999, 0.10, 0.10, NA),
	pe_alt_obs_p_upper = c(0.10, 0.99, 0.999, 0.10,  0.10, 0.10, 0.10),
	rpe_obs_p_upper =    c(0.99, 0.10, 0.10,  0.10,  0.10, 0.99, 0.99),
	rpe_obs_p_lower =    c(0.10, 0.99, 0.10,  0.10,  0.10, 0.10, 0.10)
)
dummy_res <- cpr_classify_endem(dummy_data)
data_non_num_1 <- dummy_data
data_non_num_2 <- dummy_data
data_non_num_3 <- dummy_data
data_non_num_4 <- dummy_data
data_non_num_1[1,1] <- "a"
data_non_num_2[1,2] <- "a"
data_non_num_3[1,3] <- "a"
data_non_num_4[1,4] <- "a"

#' @srrstats {G5.2, G5.2a, G5.2b} tests failure if input is not valid and checks warning messages
test_that("Input is valid", {
	expect_error(
		cpr_classify_endem(1),
		"'df' must be of class 'data.frame'"
	)
	expect_error(
		cpr_classify_endem(rand_test[,1:2]),
		"'df' must include the following columns: 'pe_obs_p_upper', 'pe_alt_obs_p_upper', 'rpe_obs_p_upper'"
	)
	expect_error(
		cpr_classify_endem(data_non_num_1),
		"Column 'pe_obs_p_upper' of 'df' must be numeric"
	)
	expect_error(
		cpr_classify_endem(data_non_num_2),
		"Column 'pe_alt_obs_p_upper' of 'df' must be numeric"
	)
	expect_error(
		cpr_classify_endem(data_non_num_3),
		"Column 'rpe_obs_p_upper' of 'df' must be numeric"
	)
	expect_error(
		cpr_classify_endem(data_non_num_4),
		"Column 'rpe_obs_p_lower' of 'df' must be numeric"
	)
})

#' @srrstats {G5.4a, G5.5} Correctness tests on trivial examples for new implementations
test_that("Calculations work", {
	set.seed(123)
  expect_equal(dummy_res$endem_type[[1]], "paleo")
  expect_equal(dummy_res$endem_type[[2]], "neo")
  expect_equal(dummy_res$endem_type[[3]], "super")
  expect_equal(dummy_res$endem_type[[4]], "mixed")
  expect_equal(dummy_res$endem_type[[5]], "not significant")
  expect_equal(dummy_res$endem_type[[6]], "not significant")
  expect_equal(dummy_res$endem_type[[7]], NA_character_)
})

test_that("Output is formatted as expected", {
	expect_s3_class(
		cpr_classify_endem(rand_test),
		"data.frame")
	expect_type(
		cpr_classify_endem(rand_test)[["endem_type"]],
		"character"
	)
	expect_true(
		isTRUE(
			all(
				cpr_classify_endem(rand_test)[["endem_type"]] %in%
					c("paleo", "neo", "super", "mixed", "not significant")
			)
		)
	)
})

