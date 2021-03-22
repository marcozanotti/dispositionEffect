#' @name evaluate
#'
#' @title Portfolio evaluation
#'
#' @description Portfolio evaluation
#'
#' @param portfolio Data frame of the investor's portfolio at time t.
#' @inheritParams closest_market_price
#'
#' @return The portfolio value as the sum of each asset quantity times the
#'   excess return of each asset with respect to the market.
#'
#' @author L. Mazzucchelli & M. Zanotti
#'
#' @references H. Shefrin & M. Statman, 1985
#'
#' @seealso \code{\link{portfolio_compute}}, \code{\link{gains_losses}},
#'   \code{\link{closest_market_price}}
#'
#' @export
evaluate_portfolio <- function(portfolio, market_prices) {

	# remove asset with missing qty
	portfolio <- portfolio[!is.na(portfolio$quantity) & portfolio$quantity != 0, ]
	# extract prices of assets still into portfolio
	market_prices <- market_prices[order(factor(market_prices$asset, levels = portfolio$asset), method = "radix"), ]
	market_prices <- market_prices[market_prices$asset %in% portfolio$asset, ]$price

	value <- sum(portfolio$quantity * (market_prices - portfolio$price))

	return(value)

}
