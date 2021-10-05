test_that("counting higher values works", {
  expect_identical(count_higher(4, 1:10), 3L)
  expect_equal(count_higher(4, 1:10), 3L)
  expect_equal(count_higher(4, 1:10), 3)
})
