#' @name evaluate
#'
#' @title Portfolio evaluation
#'
#' @description Portfolio evaluation
#'
#' @details
#'
#' @param portfolio_df Data frame of the investor's portfolio at time t.
#' @param trx_date POSIXct value of the transaction date.
#' @param df_asset_prices Data frame of the market prices.
#' @param statistics Logical. If TRUE some statistical indexes are computed
#'   on the portfolio and returned.
#'
#' @return The portfolio value as the sum of each asset quantity times the
#'   excess return of each asset with respect to the market.
#'
#' @author L. Mazzucchelli & M. Zanotti
#'
#' @references H. Shefrin & M. Statman, 1985
#'
#' @seealso \code{\link{portfolio_update}}, \code{\link{gains_and_losses}}
#'
#' @export
evaluate_portfolio <- function(portfolio_df, trx_date, df_asset_prices, statistics = FALSE) {

	portfolio_df <- portfolio_df[which(!is.na(portfolio_df$qty)),] # remove asset with missing qty

	if (nrow(portfolio_df) == 0) {
		# check on rows: if zero initial condition where the portfolio is empty
		value <- NULL

	} else {

		market_values <- map_dbl(portfolio_df$asset, closest_historical_price, trx_date, df_asset_prices)
		value <- sum(portfolio_df$qty * (market_values - portfolio_df$prz))

		if (statistics) {
			# compute some other portfolio statistics
		}

	}

	return(value)

}
