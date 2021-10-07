library(picante)
data(phylocom)
rand_test <- cpr_rand_test(phylocom$sample, phylocom$phy, metrics = c("pe", "rpe"))

test_that("Input is valid", {
	expect_error(
		cpr_classify_endem(1),
		"'df' must be of class 'data.frame'"
	)
	expect_error(
		cpr_classify_endem(rand_test[,1:2]),
		"'df' must include the following columns: 'pe_obs_p_upper', 'pe_alt_obs_p_upper', 'rpe_obs_p_upper'"
	)
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
