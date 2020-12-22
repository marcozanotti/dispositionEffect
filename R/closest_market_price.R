#' @title Closest market price
#'
#' @description Find the market price closest to a certain datetime and for
#'   as specific asset.
#'
#' @details
#'
#' @param asset Character name of the asset to look for.
#' @param datetime POSIXct of the datetime at which looking for the asset's
#'   price.
#' @param market_prices Data frame containing the market prices.
#'
#' @return The value of the asset's price.
#'
#' @author L. Mazzucchelli & M. Zanotti
#'
#' @references H. Shefrin & M. Statman, 1985
#'
#' @seealso \code{\link{evaluate}}
#'
#' @export
closest_historical_price <- function(asset,
																		 datetime,
																		 market_prices) {

	round_datetime_unit <- "15 mins" # generalize this !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

	# convert datetime into some rounded unit
	datetime <- lubridate::round_date(datetime, unit = round_datetime_unit)

	# filter historical przs for asset and datetime
	market_prices <- market_prices[(market_prices$asset == asset) &
																 (market_prices$datetime <= datetime), ]
	# extract the closest date which is before the datetime
	closest_date <- which.min(abs(datetime - market_prices$datetime))
	# extract the price
	res <- market_prices[closest_date, ]$price

	return(res)

}
