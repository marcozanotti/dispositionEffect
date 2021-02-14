#' @title Closest market price
#'
#' @description Find the market price closest to a certain datetime and for
#'   as specific asset.
#'
#' @param asset Character vector of assets' names to look for.
#' @param datetime POSIXct of the datetime at which looking for the asset's
#'   price.
#' @param market_prices Data frame containing the market prices.
#' @param price_only Logical. If TRUE then only the price is returned.
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
																 price_only = FALSE) {

	# filter historical przs for asset and datetime
	res <- market_prices[market_prices$asset %in% asset &
											 	market_prices$datetime <= datetime, ]
	# extract the closest date which is before the datetime
	res <- dplyr::ungroup(
		dplyr::slice(
			dplyr::group_by(res, !!rlang::sym("asset")), dplyr::n()
		)
	)
	# extract the price
	if (price_only) {
		res <- res$price
	}

	return(res)

}

