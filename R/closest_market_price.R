#' @title Closest market price
#'
#' @description Find the market price closest to a certain datetime and for
#'   specific assets.
#'
#' @param asset Character vector of assets' names to look for.
#' @param datetime POSIXct of the datetime at which looking for the asset's
#'   price.
#' @param market_prices Data frame containing the market prices.
#' @param price_only Logical. If TRUE then only the price is returned.
#' @param exact Logical. If TRUE then it looks for market prices at the same
#'   datetime only, otherwise it looks for the nearest before the datetime.
#' @param substitute_datetime Logical. If TRUE the datetime is substituted
#'   with the datetime argument.
#'
#' @return The data frame of closest market prices.
#'
#' @seealso \code{\link{evaluate}},
#'   \code{lubridate::\link[lubridate:round_date]{round_date}}
#'
#' @export
closest_market_price <- function(
	asset,
	datetime,
	market_prices,
	price_only = FALSE,
	exact = FALSE,
	substitute_datetime = FALSE
) {

	# filter historical prices for asset and datetime
	if (exact) {
		res <- market_prices[market_prices$asset %in% asset & market_prices$datetime == datetime, ]
	} else {
		res <- market_prices[market_prices$asset %in% asset & market_prices$datetime <= datetime, ]
	}

	# extract the closest date which is before the datetime
	res <-
		as.data.frame(
			dplyr::ungroup(
				dplyr::slice(
					dplyr::group_by(res, !!dplyr::sym("asset")),
					dplyr::n()
				)
			)
		)

	# change datetime
	if (substitute_datetime) {
		res$datetime <- datetime
	}

	# extract the price
	if (price_only) {
		res <- res$price
	}

	return(res)

}

