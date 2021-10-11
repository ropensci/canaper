test_that("counting higher values works", {
	expect_identical(count_higher(4, 1:10), 3L)
	expect_equal(count_higher(4, 1:10), 3L)
	expect_equal(count_higher(4, 1:10), 3)
})

# see https://stackoverflow.com/questions/2769510/numeric-comparison-difficulty-in-r
test_that("modified equalities work", {
	a <- 0.58
	b <- 0.08
	expect_true(
		(a - b) %>=% 0.5
	)
	expect_false(
		(a - b) >= 0.5
	)
	expect_true(
	  2.0 %>=% 2.0
	)
	expect_true(
		2.1 %>=% 2.0
	)
	expect_false(
		2.0 %>=% 2.1
	)
	expect_true(
		2.0 %<=% 2.0
	)
	expect_false(
		2.1 %<=% 2.0
	)
	expect_true(
		2.0 %<=% 2.1
	)
}
)
