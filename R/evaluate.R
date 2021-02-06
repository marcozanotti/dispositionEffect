#' @name evaluate
#'
#' @title Portfolio evaluation
#'
#' @description Portfolio evaluation
#'
#' @param portfolio Data frame of the investor's portfolio at time t.
#' @param transaction_datetime POSIXct value of the transaction date.
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
															 transaction_datetime,
															 market_prices,
															 portfolio_statistics = FALSE) {

	portfolio <- portfolio[!is.na(portfolio$quantity),] # remove asset with missing qty

	if (nrow(portfolio) == 0) {
		# check on rows: if zero initial condition where the portfolio is empty
		value <- NULL

	} else {

		market_values <- purrr::map_dbl(portfolio$asset, closest_market_price,
																		transaction_datetime, market_prices, price_only = TRUE)
		value <- sum(portfolio$quantity * (market_values - portfolio$price))

		if (portfolio_statistics) {
			# compute some other portfolio statistics
		}

	}

	return(value)

}

