context("utilities")

test_that("negative match works", {
	expect_false(1 %!in% 1:3)
	expect_true(1 %!in% 4:6)
})

test_that("ewise_mean works", {
	expect_equal(ewise_mean(1:10, 99:90), rep(50, 10))
	expect_equal(ewise_mean(c(NA, 2:10), 99:90, na.rm = TRUE), c(99, rep(50, 9)))
	expect_equal(ewise_mean(c(0, 2:10), 99:90, zero.substitute = TRUE), c(99, rep(50, 9)))
})

df <- data.frame(
	"RG_value" = c(-1, 1, 1, 1, -1, 1),
	"RL_value" = c(-1, 1, -1, -1, 1, -1),
	"PG_value" = c(1, 1, -1, 1, -1, 1),
	"PL_value" = c(-1, -1, -1, 1, 1, -1)
)

test_that("check_gainloss works", {
	expect_type(check_gainloss(df[1,]), "character")
	expect_type(check_gainloss(df[2,]), "character")
	expect_type(check_gainloss(df[3,]), "character")
	expect_type(check_gainloss(df[4,]), "character")
	expect_type(check_gainloss(df[5,]), "character")
	expect_type(check_gainloss(df[6,]), "NULL")
})

test_that("check_market_prices works", {
	expect_type(check_market_prices(NULL, c("A", "B", "C")), "character")
	expect_type(check_market_prices("A", c("A", "B", "C")), "character")
	expect_type(check_market_prices(c("A", "B"), c("A", "B", "C")), "character")
	expect_type(check_market_prices(c("A", "B", "C"), c("A", "B", "C")), "NULL")
})
