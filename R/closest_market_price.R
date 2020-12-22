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
#' @param unit Character string specifying a time unit or a multiple of a
#'   unit to be rounded to. Valid base units are second, minute, hour, day,
#'   week, etc. See \code{lubridate::\link[lubridate:round_date]{round_date}}.
#'
#' @return The value of the asset's price.
#'
#' @author L. Mazzucchelli & M. Zanotti
#'
#' @references H. Shefrin & M. Statman, 1985
#'
#' @seealso \code{\link{evaluate}},
#'   \code{lubridate::\link[lubridate:round_date]{round_date}}
#'
#' @export
closest_market_price <- function(asset,
																 datetime,
																 market_prices,
																 unit = "15 mins") {

	# convert datetime into some rounded unit
	datetime <- lubridate::floor_date(datetime, unit = unit)

	# filter historical przs for asset and datetime
	market_prices <- market_prices[(market_prices$asset == asset) &
																 (market_prices$datetime <= datetime), ]
	# extract the closest date which is before the datetime
	closest_date <- which.min(abs(datetime - market_prices$datetime))
	# extract the price
	res <- market_prices[closest_date, ]$price

	return(res)

}
