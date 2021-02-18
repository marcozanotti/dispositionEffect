#' @name manipulate_initial_data
#'
#' @title Manipulate market prices
#'
#' @description Aggregate the data frame of market prices with a
#'   specific aggregate function within a time interval.
#'
#' @param portfolio_transactions Data frame. The investor's transactions data frame.
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
NULL


#' @describeIn manipulate_initial_data Aggregate the data frame of investors'
#'   transactions that happened at the same datetime.
#' @export
aggregate_transactions <- function(portfolio_transactions) {

	portfolio_transactions <- portfolio_transactions %>%
		dplyr::group_by(
			!!rlang::sym("investor"),
			!!rlang::sym("type"),
			!!rlang::sym("asset"),
			!!rlang::sym("datetime")
		) %>%
		dplyr::summarise( # use mean prz and total qty
			quantity = sum(!!rlang::sym("quantity")),
			price = mean(!!rlang::sym("price")),
			.groups = "drop"
		) %>%
		dplyr::arrange(!!rlang::sym("datetime")) %>%
		dplyr::select("investor", "type", "asset", "quantity", "price", "datetime")

	return(portfolio_transactions)

}


#' @describeIn manipulate_initial_data Reduce the data frame of investors'
#'   transactions by aggregating those transactions that happened within
#'   a time interval.
#' @export
reduce_transactions <- function(portfolio_transactions, unit = "1 mins") {

	portfolio_transactions <- portfolio_transactions %>%
		dplyr::mutate(
			datetime = lubridate::floor_date(!!rlang::sym("datetime"), unit = unit),
			quantity = ifelse(!!rlang::sym("type") == "B",
												!!rlang::sym("quantity"),
												-1 * !!rlang::sym("quantity"))
		) %>%
		dplyr::group_by(
			!!rlang::sym("investor"),
			!!rlang::sym("asset"),
			!!rlang::sym("datetime")
		) %>%
		dplyr::summarise( # use mean prz and total qty
			quantity = sum(!!rlang::sym("quantity")),
			price = mean(!!rlang::sym("price")),
			.groups = "drop"
		) %>%
		dplyr::mutate(
			type = ifelse(!!rlang::sym("quantity") >= 0, "B", "S"),
			quantity = abs(!!rlang::sym("quantity"))
		) %>%
		dplyr::arrange(!!rlang::sym("datetime")) %>%
		dplyr::select("investor", "type", "asset", "quantity", "price", "datetime")

	return(portfolio_transactions)

}


#' @describeIn manipulate_initial_data Clean the data frame of investors'
#'   transactions.
#' @export
clean_tansactions <- function(portfolio_transactions, unit = "1 mins") {

	portfolio_transactions <- portfolio_transactions %>%
		aggregate_transactions() %>%
		reduce_transactions(unit = unit)

	return(portfolio_transactions)

}


#' @describeIn manipulate_initial_data Aggregate the data frame of market prices
#'   with a specific aggregate function within a time interval.
#' @export
aggregate_market_prices <- function(market_prices,
																		unit = "15 mins",
																		aggregate_price_fun = mean) {

	market_prices <- market_prices %>%
		dplyr::arrange(!!rlang::sym("asset"), !!rlang::sym("datetime")) %>%
		dplyr::mutate(datetime = lubridate::floor_date(!!rlang::sym("datetime"),
																									 unit = unit)) %>%
		dplyr::group_by(!!rlang::sym("asset"), !!rlang::sym("datetime")) %>%
		dplyr::summarise(price = aggregate_price_fun(!!rlang::sym("price")), .groups = "drop")

	return(market_prices)

}


#' @describeIn manipulate_initial_data Subset the data frame of market prices
#'   based on investor's traded assets and datetimes of transactions.
#' @export
subset_market_prices <- function(market_prices,
																 portfolio_transactions,
																 unit = "15 mins") {

	grid <- expand.grid(unique(portfolio_transactions$asset),
											unique(lubridate::floor_date(portfolio_transactions$datetime, unit = unit)),
											stringsAsFactors = FALSE) %>%
		purrr::set_names("asset", "datetime") %>%
		dplyr::arrange(!!rlang::sym("asset"), !!rlang::sym("datetime"))

	market_prices <- purrr::map2_df(grid[, 1], grid[, 2],
																	closest_market_price,
																	market_prices = market_prices,
																	price_only = FALSE)

	return(market_prices)

}


#' @describeIn manipulate_initial_data Clean the data frame of market prices.
#' @export
clean_market_prices <- function(market_prices,
																portfolio_transactions,
																unit = "15 mins",
																aggregate_price_fun = mean) {

	market_prices <- market_prices %>%
		aggregate_market_prices(unit = unit, aggregate_price_fun = aggregate_price_fun) %>%
		subset_market_prices(portfolio_transactions, unit = unit)

	return(market_prices)

}
