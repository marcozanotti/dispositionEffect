#' @name evaluate
#'
#' @title Portfolio evaluation
#'
#' @description Portfolio evaluation
#'
#' @param portfolio Data frame of the investor's portfolio at time t.
#' @inheritParams closest_market_price
#' @param portfolio_statistics Logical. If TRUE some statistical indexes are computed
#'   on the portfolio and returned.
#'
#' @return The portfolio value as the sum of each asset quantity times the
#'   excess return of each asset with respect to the market.
#'
#' @author L. Mazzucchelli & M. Zanotti
#'
#' @references H. Shefrin & M. Statman, 1985
#'
#' @seealso \code{\link{portfolio_compute}}, \code{\link{gains_and_losses}},
#'   \code{\link{closest_market_price}}
#'
#' @export
evaluate_portfolio <- function(portfolio,
															 market_prices,
															 portfolio_statistics = FALSE) {

	portfolio <- portfolio[!is.na(portfolio$quantity),] # remove asset with missing qty
	market_prices <- market_prices[market_prices$asset %in% portfolio$asset, ]$price

	value <- sum(portfolio$quantity * (market_prices - portfolio$price))

	if (portfolio_statistics) {
		# compute some other portfolio statistics
	}

	return(value)

}

