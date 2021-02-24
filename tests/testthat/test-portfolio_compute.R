context("portfolio_compute")

test_that("portfolio_compute works (arg method)", {
	expect_equal(portfolio_compute(investor, marketprices, method = "none"), portfolio_results[, c(1:5)])
	expect_equal(portfolio_compute(investor, marketprices, method = "count"), portfolio_results[, c(1:5, 6:9)])
	expect_equal(portfolio_compute(investor, marketprices, method = "total"), portfolio_results[, c(1:5, 10:13)])
	expect_equal(portfolio_compute(investor, marketprices, method = "value"), portfolio_results[, c(1:5, 14:17)])
	expect_equal(portfolio_compute(investor, marketprices, method = "duration"), portfolio_results[, c(1:5, 18:21)])
	expect_equal(portfolio_compute(investor, marketprices, method = "all"), portfolio_results)
})

test_that("portfolio_compute works (arg allow_short)", {
	expect_type(portfolio_compute(investor, marketprices, allow_short = FALSE), "list")
})

test_that("portfolio_compute works (arg time_threshold)", {
	expect_equal(portfolio_compute(investor, marketprices, time_threshold = "5 mins"), portfolio_results)
})

test_that("portfolio_compute works (arg portfolio_driven)", {
	expect_type(portfolio_compute(investor, marketprices, portfolio_driven = TRUE), "list")
})

