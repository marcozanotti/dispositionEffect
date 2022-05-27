#' @name manipulate_initial_data
#'
#' @title Pre-process transactions and market prices data frames
#'
#' @description These functions allows to pre-process transactions
#'   and market prices data frames performing aggregation and
#'   subsetting operations.
#'
#' @param portfolio_transactions Data frame. The investor's transactions data frame.
#' @param market_prices Data frame containing the market prices.
#' @param unit Character string specifying a time unit or a multiple of a
#'   unit to be rounded to. Valid base units are second, minute, hour, day,
#'   week, etc. See \code{lubridate::\link[lubridate:round_date]{round_date}}.
#' @param reduce Logical. If TRUE use `reduce_transactions`, otherwise use
#'   `aggregate_transactions` to aggregate portfolio transactions.
#' @param aggregate_price_fun Function to use to aggregate prices within
#'   the round unit. Default to \code{mean}.
#' @param portfolio_assets Character vector of the transaction assets.
#' @param portfolio_datetimes POSIXct vector of the transaction datetimes.
#' @param investor_name Character. The name to be assigned to the investor.
#' @param subset Logical. If TRUE, `subset_market_prices` is used to generate
#'   investor's data.
#'
#' @return A modified version of the input data frame.
#'
#' @seealso \code{\link{closest_market_price}},
#'   \code{lubridate::\link[lubridate:round_date]{round_date}}
#'
#' @keywords internal
NULL


#' @describeIn manipulate_initial_data Aggregate the data frame of investors'
#'   transactions that happened at the same datetime.
aggregate_transactions <- function(portfolio_transactions, unit = "1 mins") {

	portfolio_transactions <- portfolio_transactions %>%
		dplyr::mutate(datetime = lubridate::floor_date(!!dplyr::sym("datetime"), unit = unit)) %>%
		dplyr::group_by(
			!!dplyr::sym("investor"),
			!!dplyr::sym("type"),
			!!dplyr::sym("asset"),
			!!dplyr::sym("datetime")
		) %>%
		dplyr::summarise(
			quantity = sum(!!dplyr::sym("quantity")),
			price = mean(!!dplyr::sym("price")),
			.groups = "drop"
		) %>%
		dplyr::arrange(!!dplyr::sym("datetime")) %>%
		dplyr::select("investor", "type", "asset", "quantity", "price", "datetime")

	return(portfolio_transactions)

}


#' @describeIn manipulate_initial_data Reduce the data frame of investors'
#'   transactions by aggregating those transactions that happened within
#'   a time interval.
reduce_transactions <- function(portfolio_transactions, unit = "1 mins") {

	portfolio_transactions <- portfolio_transactions %>%
		dplyr::mutate(
			datetime = lubridate::floor_date(!!dplyr::sym("datetime"), unit = unit),
			quantity = ifelse(!!dplyr::sym("type") == "B", !!dplyr::sym("quantity"), -1 * !!dplyr::sym("quantity"))
		) %>%
		dplyr::group_by(
			!!dplyr::sym("investor"),
			!!dplyr::sym("asset"),
			!!dplyr::sym("datetime")
		) %>%
		dplyr::summarise( # use mean prz and total qty
			quantity = sum(!!dplyr::sym("quantity")),
			price = mean(!!dplyr::sym("price")),
			.groups = "drop"
		) %>%
		dplyr::mutate(
			type = ifelse(!!dplyr::sym("quantity") >= 0, "B", "S"),
			quantity = abs(!!dplyr::sym("quantity"))
		) %>%
		dplyr::arrange(!!dplyr::sym("datetime")) %>%
		dplyr::select("investor", "type", "asset", "quantity", "price", "datetime")

	return(portfolio_transactions)

}


#' @describeIn manipulate_initial_data Clean the data frame of investors'
#'   transactions.
clean_transactions <- function(portfolio_transactions, unit = "1 mins", reduce = FALSE) {

	if (reduce) {
		portfolio_transactions <- portfolio_transactions %>%
			reduce_transactions(unit = unit)
	} else {
		portfolio_transactions <- portfolio_transactions %>%
			aggregate_transactions(unit = unit)
	}

	return(portfolio_transactions)

}


#' @describeIn manipulate_initial_data Aggregate the data frame of market prices
#'   with a specific aggregate function within a time interval.
aggregate_market_prices <- function(market_prices, unit = NULL, aggregate_price_fun = mean) {

	if (!is.null(unit)) {
		market_prices <- market_prices %>%
			dplyr::mutate(datetime = lubridate::floor_date(!!dplyr::sym("datetime"), unit = unit))
	}

	market_prices <- market_prices %>%
		dplyr::select(!!dplyr::sym("asset"), !!dplyr::sym("datetime"), !!dplyr::sym("price")) %>%
		dplyr::arrange(!!dplyr::sym("asset"), !!dplyr::sym("datetime")) %>%
		dplyr::group_by(!!dplyr::sym("asset"), !!dplyr::sym("datetime")) %>%
		dplyr::summarise(price = aggregate_price_fun(!!dplyr::sym("price")), .groups = "drop")

	return(market_prices)

}


#' @describeIn manipulate_initial_data Subset the data frame of market prices
#'   based on investor's traded assets and datetimes of transactions.
subset_market_prices <- function(market_prices, portfolio_assets, portfolio_datetimes, unit = NULL) {

	if (!is.null(unit)) {
		dtt <- unique(lubridate::floor_date(portfolio_datetimes, unit = unit))
	} else {
		dtt <- unique(portfolio_datetimes)
	}
	market_prices <- purrr::map_df(dtt, ~ closest_market_price(portfolio_assets, .x, market_prices, substitute_datetime = TRUE))

	return(market_prices)

}


#' @describeIn manipulate_initial_data Generate investors' data from transactions
#'   and market prices.
generate_data <- function(
	portfolio_transactions,
	market_prices,
	investor_name = NULL,
	unit = NULL,
	subset = FALSE
) {

	if (!is.null(investor_name)) {
		portfolio_transactions$investor <- investor_name
	}

	assets <- unique(portfolio_transactions$asset)
	mrkt <- market_prices[market_prices$asset %in% assets, ]

	if (!subset) {

		if (is.null(unit)) {
			first_date <- min(portfolio_transactions$datetime)
			last_date <- max(portfolio_transactions$datetime)
		} else {
			first_date <- lubridate::floor_date(min(portfolio_transactions$datetime), unit = unit)
			last_date <- lubridate::ceiling_date(max(portfolio_transactions$datetime), unit = unit)
		}

		mrkt <- market_prices[market_prices$datetime >= first_date & market_prices$datetime <= last_date, ]

	} else {

		datetimes <- unique(portfolio_transactions$datetime)
		mrkt <- subset_market_prices(mrkt, assets, datetimes, unit) # dispositionEffect function

	}

	res <- list(
		"transactions" = portfolio_transactions,
		"marketprices" = mrkt
	)

	return(res)

}
