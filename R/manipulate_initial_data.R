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
		dplyr::mutate(datetime = lubridate::floor_date(!!rlang::sym("datetime"), unit = unit)) %>%
		dplyr::group_by(
			!!rlang::sym("investor"),
			!!rlang::sym("type"),
			!!rlang::sym("asset"),
			!!rlang::sym("datetime")
		) %>%
		dplyr::summarise(
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
reduce_transactions <- function(portfolio_transactions, unit = "1 mins") {

	portfolio_transactions <- portfolio_transactions %>%
		dplyr::mutate(
			datetime = lubridate::floor_date(!!rlang::sym("datetime"), unit = unit),
			quantity = ifelse(!!rlang::sym("type") == "B", !!rlang::sym("quantity"), -1 * !!rlang::sym("quantity"))
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
			dplyr::mutate(datetime = lubridate::floor_date(!!rlang::sym("datetime"), unit = unit))
	}

	market_prices <- market_prices %>%
		dplyr::select(!!rlang::sym("asset"), !!rlang::sym("datetime"), !!rlang::sym("price")) %>%
		dplyr::arrange(!!rlang::sym("asset"), !!rlang::sym("datetime")) %>%
		dplyr::group_by(!!rlang::sym("asset"), !!rlang::sym("datetime")) %>%
		dplyr::summarise(price = aggregate_price_fun(!!rlang::sym("price")), .groups = "drop")

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


#' @describeIn manipulate_initial_data Select months with assets' returns
#'   greater (or lower) than the threshold.
select_months <- function(market_prices, return_threshold = 5) {

	db_monthly_logreturns <- market_prices %>%
		dplyr::arrange(!!rlang::sym("asset"), !!rlang::sym("datetime")) %>%
		dplyr::mutate(
			yearmon = paste(
				lubridate::year(!!rlang::sym("datetime")),
				lubridate::month(!!rlang::sym("datetime")),
				sep = "-"
			),
			yearmon = stringr::str_replace_all(!!rlang::sym("yearmon"), "^(\\d{4,4}-)(\\d$)", "\\10\\2")
		) %>%
		dplyr::group_by(!!rlang::sym("asset"), !!rlang::sym("yearmon")) %>%
		dplyr::slice(1, dplyr::n()) %>% # keep first and last transaction per month
		dplyr::mutate(return = diff(log(!!rlang::sym("price"))) * 100) %>% # monthly log-returns = log(last) - log(first)
		dplyr::ungroup() %>%
		dplyr::select(!!rlang::sym("asset"), !!rlang::sym("yearmon"), !!rlang::sym("return")) %>%
		dplyr::distinct(!!rlang::sym("asset"), !!rlang::sym("yearmon"), !!rlang::sym("return"))

	# remove assets traded in just one month
	assets_to_remove <- db_monthly_logreturns %>%
		dplyr::count(!!rlang::sym("asset")) %>%
		dplyr::filter(!!rlang::sym("n") == 1) %>%
		dplyr::pull(!!rlang::sym("asset"))
	db_monthly_logreturns <- db_monthly_logreturns %>%
		dplyr::filter(!(!!rlang::sym("asset") %in% assets_to_remove))

	assets <- unique(db_monthly_logreturns$asset)
	res <- vector("list", length(assets))
	# assign 1 to consecutive months with return > (or <) than threshold
	for (a in seq_along(assets)) {

		db_tmp <- db_monthly_logreturns %>%
			dplyr::filter(!!rlang::sym("asset") == assets[a])

		is_ok <- vector("numeric", nrow(db_tmp))
		for (i in 2:nrow(db_tmp)) {
			# if threshold > 0 then >= otherwise <=
			if (return_threshold > 0) {
				check_i1 <- db_tmp$return[i - 1] >= return_threshold
				check_i <- db_tmp$return[i] >= return_threshold
			} else {
				check_i1 <- db_tmp$return[i - 1] <= return_threshold
				check_i <- db_tmp$return[i] <= return_threshold
			}
			# if both months satisfy the condition then set them to 1
			if (check_i1 & check_i) {
				is_ok[i - 1] <- 1
				is_ok[i] <- 1
			}
		}

		db_tmp$is_ok <- is_ok
		res[[a]] <- db_tmp

	}

	db_monthly_threshold <- dplyr::bind_rows(res)
	return(db_monthly_threshold)

}


#' @describeIn manipulate_initial_data Select assets with highest number of
#'   transactions within a reference period.
select_assets <- function(market_prices, market_thresholds, yearmon_t1, yearmon_t2, n_top = 20) {

	# assets ok in month t1
	assets_ym1 <- market_thresholds %>%
		dplyr::filter(!!rlang::sym("is_ok") == 1 & !!rlang::sym("yearmon") == yearmon_t1) %>%
		dplyr::pull("asset")
	# assets ok in month t2
	assets_ym2 <- market_thresholds %>%
		dplyr::filter(!!rlang::sym("is_ok") == 1 & !!rlang::sym("yearmon") == yearmon_t2) %>%
		dplyr::pull("asset")
	# keep assets traded in both periods
	db_marketprices_top_assets <- db_marketprices %>%
		dplyr::filter(!!rlang::sym("asset") %in% assets_ym1[assets_ym1 %in% assets_ym2]) %>%
		dplyr::mutate(
			yearmon =
				paste(lubridate::year(!!rlang::sym("datetime")), lubridate::month(!!rlang::sym("datetime")), sep = "-") %>%
				stringr::str_replace_all("^(\\d{4,4}-)(\\d$)", "\\10\\2")
		) %>%
		dplyr::filter(!!rlang::sym("yearmon") == yearmon_t1 | !!rlang::sym("yearmon") == yearmon_t2)

	# n_top assets by number of transactions
	top_assets <- db_marketprices_top_assets %>%
		dplyr::count(!!rlang::sym("asset")) %>%
		dplyr::arrange(dplyr::desc(!!rlang::sym("n"))) %>%
		dplyr::slice(1:n_top) %>%
		dplyr::pull("asset")
	# keep only n_top assets
	db_marketprices_top_assets <- db_marketprices_top_assets %>%
		dplyr::filter(!!rlang::sym("asset") %in% top_assets)

	return(db_marketprices_top_assets)

}


#' @describeIn manipulate_initial_data Select investors that traded specific
#'   assets within a specific year-month.
select_investors <- function(portfolio_transactions, top_assets) {

	# clean portfolio transaction from outliers
	portfolio_transactions <- portfolio_transactions %>%
		dplyr::filter(!(!!rlang::sym("investor") %in% c("7381Z", "8147Z", "63763"))) %>% # remove outliers
		dplyr::mutate(
			yearmon =
				paste(lubridate::year(!!rlang::sym("datetime")), lubridate::month(!!rlang::sym("datetime")), sep = "-") %>%
				stringr::str_replace_all("^(\\d{4,4}-)(\\d$)", "\\10\\2")
		)

	# select investors based on top_assets
	top_investors <- vector("list", length(top_assets)) %>%
		purrr::set_names(names(top_assets))
	for (nm in names(top_assets)) {
		ym2_tmp <- stringr::str_remove_all(nm, "^.*_")
		assets_tmp <- top_assets[[nm]]
		investors_tmp <- portfolio_transactions %>%
			dplyr::filter(!!rlang::sym("yearmon") == ym2_tmp & !!rlang::sym("asset") %in% assets_tmp) %>%
			dplyr::pull("investor") %>%
			unique()
		top_investors[[nm]] <- portfolio_transactions %>%
			dplyr::filter(!!rlang::sym("yearmon") == ym2_tmp & !!rlang::sym("investor") %in% investors_tmp) %>%
			dplyr::count(!!rlang::sym("investor")) %>%
			dplyr::filter(!!rlang::sym("n") >= 12) %>% # keep only investors with at least 12 transactions
			dplyr::pull("investor")
	}

	return(top_investors)

}
