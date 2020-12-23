#' @title Aggregate market price
#'
#' @description Aggregate the data frame of market prices with a
#'   specific aggregate function within a time interval.
#'
#' @details
#'
#' @param market_prices Data frame containing the market prices.
#' @param unit Character string specifying a time unit or a multiple of a
#'   unit to be rounded to. Valid base units are second, minute, hour, day,
#'   week, etc. See \code{lubridate::\link[lubridate:round_date]{round_date}}.
#' @param aggregate_price_fun Function to use to aggregate prices within
#'   the round unit. Default to \code{mean}.
#'
#' @return The data frame of aggregated market prices.
#'
#' @author L. Mazzucchelli & M. Zanotti
#'
#' @references H. Shefrin & M. Statman, 1985
#'
#' @seealso \code{\link{closest_market_price}},
#'   \code{lubridate::\link[lubridate:round_date]{round_date}}
#'
#' @export
aggregate_market_price <- function(market_prices,
															     unit = "15 mins",
															     aggregate_price_fun = mean()) {

	market_prices <- market_prices %>%
		dplyr::arrange(!!rlang::sym("asset"), !!rlang::sym("datetime")) %>%
		dplyr::mutate(datetime = lubridate::floor_date(!!rlang::sym("datetime"),
																									 unit = unit)) %>%
		dplyr::group_by(!!rlang::sym("asset"), !!rlang::sym("datetime")) %>%
		dplyr::summarise(price = aggregate_price_fun(!!rlang::sym("price")), .groups = "drop")

	return(market_prices)

}
