#' @srrstats {G5.2, G5.2a, G5.2b} tests failure if input is not valid and checks warning messages
test_that("Input is valid", {
	expect_error(
		cpr_classify_signif(1, "pd"),
		"'df' must be of class 'data.frame'"
	)
	expect_error(
		cpr_classify_signif(data.frame(), "pf"),
		"Biodiversity metrics may only be selected from 'pd', 'rpd', 'pe', or 'rpe'"
	)
	expect_error(
		cpr_classify_signif(data.frame(pd_obs_p_lower = 0.5), "pd"),
		"'df' does not include percentage of times observed value was higher than random values"
	)
	expect_error(
		cpr_classify_signif(data.frame(pd_obs_p_upper = 0.5), "pd"),
		"'df' does not include percentage of times observed value was lower than random values"
	)
	expect_error(
		cpr_classify_signif(data.frame(pd_obs_p_upper = 2, pd_obs_p_lower = 0.5), "pd"),
		"Values for percentage of times observed value was higher than random values should be between 0 and 1, inclusive"
	)
	expect_error(
		cpr_classify_signif(data.frame(pd_obs_p_upper = 0.5, pd_obs_p_lower = 2), "pd"),
		"Values for percentage of times observed value was lower than random values should be between 0 and 1, inclusive"
	)
})

test_that("Calculations work", {
	expect_equal(cpr_classify_signif(
		data.frame(
			pd_obs_p_lower = c(0.98),
			pd_obs_p_upper = c(0.0001)
		),
		metric = "pd"
	)[["pd_signif"]],
	"< 0.025"
	)
	expect_equal(cpr_classify_signif(
		data.frame(
			pd_obs_p_lower = c(0.5),
			pd_obs_p_upper = c(0.0001)
		),
		metric = "pd"
	)[["pd_signif"]],
	"not significant"
	)
	expect_equal(cpr_classify_signif(
		data.frame(
			pd_obs_p_lower = c(0.0001),
			pd_obs_p_upper = c(0.99)
		),
		metric = "pd"
	)[["pd_signif"]],
	"> 0.975"
	)
})

test_that("Output is formatted as expected", {
	expect_s3_class(
		cpr_classify_signif(
			data.frame(
				pd_obs_p_lower = c(0.0001),
				pd_obs_p_upper = c(0.99)
			),
			metric = "pd"
		),
		"data.frame")
	expect_type(
		cpr_classify_signif(
			data.frame(
				pd_obs_p_lower = c(0.0001),
				pd_obs_p_upper = c(0.99)
			),
			metric = "pd"
		)[["pd_signif"]],
		"character"
	)
	# Run cpr_classify_signif() a bunch of times to test the output values
	random_results_two_sided <- cpr_classify_signif(
		data.frame(
			pd_obs_p_lower = runif(1000),
			pd_obs_p_upper = runif(1000)
		),
		metric = "pd"
	)
	random_results_one_sided <- cpr_classify_signif(
		data.frame(
			pd_obs_p_lower = runif(1000),
			pd_obs_p_upper = runif(1000)
		),
		metric = "pd",
		one_sided = TRUE
	)
	expect_true(
		isTRUE(
			all(random_results_two_sided[["pd_signif"]] %in%
						c("< 0.01", "< 0.025", "> 0.975", "> 0.99", "not significant")
			)
		)
	)
	expect_true(
		isTRUE(
			all(random_results_one_sided[["pd_signif"]] %in%
						c("< 0.01", "< 0.05", "> 0.99", "> 0.95", "not significant")
			)
		)
	)
})
