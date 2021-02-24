context("portfolio_compute")

test_that("portfolio_compute works (arg method)", {
	# skip_on_cran()
	expect_equal(portfolio_compute(investor, marketprices, method = "none"), portfolio_results[, c(1:5)])
	expect_equal(portfolio_compute(investor, marketprices, method = "count"), portfolio_results[, c(1:5, 6:9)])
	expect_equal(portfolio_compute(investor, marketprices, method = "total"), portfolio_results[, c(1:5, 10:13)])
	expect_equal(portfolio_compute(investor, marketprices, method = "value"), portfolio_results[, c(1:5, 14:17)])
	expect_equal(portfolio_compute(investor, marketprices, method = "duration"), portfolio_results[, c(1:5, 18:21)], tolerance = 0.01)
	expect_equal(portfolio_compute(investor, marketprices, method = "all"), portfolio_results, tolerance = 0.01)
})

test_that("portfolio_compute works (arg allow_short)", {
	# skip_on_cran()
	expect_type(portfolio_compute(investor, marketprices, allow_short = FALSE), "list")
})

test_that("portfolio_compute works (arg time_threshold)", {
	# skip_on_cran()
	expect_equal(portfolio_compute(investor, marketprices, time_threshold = "5 mins"), portfolio_results, tolerance = 0.01)
})

test_that("portfolio_compute works (arg portfolio_driven)", {
	skip_on_cran()
	expect_type(portfolio_compute(investor, marketprices, portfolio_driven = TRUE), "list")
})

