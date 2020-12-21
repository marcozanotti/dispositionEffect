#' @title Portfolio Compute
#'
#' @description Computation of all the transaction updates and the
#'   realized and paper gains and losses for each assets.
#'
#' @details
#'
#' @param portfolio_transactions Data frame. The investor's transactions data frame.
#' @inheritParams closest_historical_price
#' @inheritParams paper_compute
#' @inheritParams difference_in_time
#' @inheritParams evaluate
#' @param method Character string containing the method to use to compute
#'   realized and paper gains and losses. If "none" nothing is computed but the
#'   investor's portfolio update. Otherwise it has to be one of "count", "total",
#'   "value", "duration" and "all".
#' @param posneg_portfolios Logical. If TRUE the realized and paper gains and
#'   losses for the positive (that is when the investor's portfolio value, as
#'   computed through \code{\link{evaluate_portfolio}}, is greater than zero)
#'   and the negative (that is when the investor's portfolio value, as computed
#'   through \code{\link{evaluate_portfolio}}, is smaller than zero) portfolios
#'   are returned.
#' @param verbose Numeric vector of length 2 that allows to control
#'   for the function verbosity.
#' @param progress Logical. If TRUE a progress bar is displayed.
#'
#' @return A [tibble][tibble::tibble-package] containing the investor's
#'   portfolio and the values of realized and paper gains and losses
#'   computed by means of the chosen method on each portfolio assets.
#'
#' @author L. Mazzucchelli & M. Zanotti
#'
#' @references H. Shefrin & M. Statman, 1985
#'
#' @seealso \code{\link{realized_compute}}, \code{\link{paper_compute}},
#'   \code{\link{portfolio_update}}
#'
#' @export
portfolio_update <- function(portfolio_transactions,
														 market_prices,
														 method = "all",
														 allow_short = FALSE,
														 time_threshold = "5 mins",
														 posneg_portfolios = FALSE,
														 portfolio_statistics = FALSE,
														 verbose = c(0, 0),
														 progress = TRUE) {

	# checks on inputs
	# assumes that portfolio_transactions is ordered by datetime

	# # portfolio_transactions column names
	# msg <- check_df_names("portfolio_transactions", names(portfolio_transactions),
	# 											c("client", "type", "asset", "qty", "prz", "datetime"))
	# if (!is.null(msg)) { stop(msg, call. = FALSE) }
	# # portfolio_transactions column types
	# typ <- purrr::map(portfolio_transactions, class) %>% purrr::map(1) %>% unlist()
	# msg <- check_var_types("portfolio_transactions", typ,
	# 											 c("client" = "character", "type" = "character",
	# 											 	"asset" = "character", "qty" = "integer",
	# 											 	"prz" = "numeric", "datetime" = "POSIXct"))
	# if (!is.null(msg)) { stop(msg, call. = FALSE) }
	# # portfolio_transactions column "type" values
	# msg <- check_values("portfolio_transactions$type", unique(portfolio_transactions$type), c("B", "S"), identical = TRUE)
	# if (!is.null(msg)) { stop(msg, call. = FALSE) }
	# # market_prices column names
	# msg <- check_df_names("market_prices", names(market_prices),
	# 											c("asset", "datetime", "prz", "qty"))
	# if (!is.null(msg)) { stop(msg, call. = FALSE) }
	# # market_prices column types
	# typ <- purrr::map(market_prices, class) %>% purrr::map(1) %>% unlist()
	# msg <- check_var_types("market_prices", typ,
	# 											 c("asset" = "character", "datetime" = "POSIXct",
	# 											 	"qty" = "integer", "prz" = "numeric"))
	# if (!is.null(msg)) { stop(msg, call. = FALSE) }
	# # method values
	# msg <- check_values("method", method,
	# 										c("count", "total", "value", "duration", "all", "none"))
	# if (!is.null(msg)) { stop(msg, call. = FALSE) }

	# verbosity
	verb <- verbose[1] == 1

	# global parameters
	client <- portfolio_transactions$client[1]
	client_assets <- unique(portfolio_transactions$asset)
	asset_ntrx <- portfolio_transactions %>%
		dplyr::group_by(asset) %>%
		dplyr::summarise(ntrx = dplyr::n())

	# client's initial portfolio (portfolio at time 0):
	# an empty df with all the assets traded by the client
	# with qty = NA and prz = NA for all the assets (initial condition)
	portfolio <- initializer_portfolio(client, client_assets)

	# initialize the df of computation: RG, RL, PG, PL and other
	if (!posneg_portfolios) {
		results_df <- initializer_results(client, client_assets, method)
	} else {
		pos_results_df <- initializer_results(client, client_assets, method)
		neg_results_df <- initializer_results(client, client_assets, method)
	}

	# progress bar
	if (progress) {
		# initialize progress bar
		pb <- progress::progress_bar$new(format = ":current  [:bar] :percent in :elapsed",
													           total = nrow(portfolio_transactions),
													           clear = FALSE, width = 60, show_after = 0)
		pb$tick(0)
	}

	for (i in 1:nrow(portfolio_transactions)) {

		# extract scalars (trx = transaction)
		trx_type <- portfolio_transactions[i, ]$type # trx type
		trx_asset <- portfolio_transactions[i, ]$asset # trx asset
		trx_qty <- portfolio_transactions[i, ]$qty # trx qty
		trx_prz <- portfolio_transactions[i, ]$prz # trx prz
		trx_dtt <- portfolio_transactions[i, ]$datetime # trx datetime
		previous_dtt <- portfolio_transactions[i - 1, ]$datetime

		if (trx_type == "S") {
			trx_qty <- trx_qty * -1L # if it's a sell transaction then consider qty as negative
		}

		# compute RG/RL/PG/PL
		if (method != "none") {
			if (verb) message("\nStart computing RG/RL/PG/PL..")
			df_info <- gains_and_losses(trx_type, trx_asset, trx_qty, trx_prz, trx_dtt,
																	previous_dtt, portfolio, market_prices,
																	time_threshold, method, allow_short, verbose)
		}

		# evaluate global portfolio value
		if (verb) message("Evaluating global portfolio position..")
		portfolio_value <- evaluate_portfolio(portfolio, trx_dtt, market_prices,
																					portfolio_statistics = portfolio_statistics)

		# update the portfolio
		if (verb) message(paste0("Updating portfolio.. (", trx_asset, " asset)"))
		# qty, prz and dtt of trx_asset already into portfolio
		ptf_qty <- portfolio[portfolio$asset == trx_asset, ]$qty
		ptf_prz <- portfolio[portfolio$asset == trx_asset, ]$prz
		ptf_dtt <- portfolio[portfolio$asset == trx_asset, ]$dtt
		if (is.na(ptf_qty)) {
			# if qty is NA (initial condition), simply update the portfolio
			# with the values of qty, prz and dtt of the transaction
			portfolio[portfolio$asset == trx_asset,]$qty <- trx_qty
			portfolio[portfolio$asset == trx_asset,]$prz <- trx_prz
			portfolio[portfolio$asset == trx_asset,]$dtt <- trx_dtt
		} else {
			# else sum the qtys
			portfolio[portfolio$asset == trx_asset,]$qty <- ptf_qty + trx_qty
			# and adjust the przs based on conditions
			portfolio[portfolio$asset == trx_asset,]$prz <-
				prz_update(ptf_qty, ptf_prz, trx_qty, trx_prz, trx_type)
			portfolio[portfolio$asset == trx_asset,]$dtt <-
				dtt_update(ptf_qty, ptf_dtt, trx_qty, trx_dtt, trx_type)
		}

		# update the results_df
		if (method != "none" && !is.null(df_info)) {
			# if empty portfolio, then gains_and_losses() returns NULL
			# if empty portfolio, then evaluate_portfolio() returns NULL
			if (verb) message("Updating results..")
			if (!posneg_portfolios) {
				results_df <- results_update(results_df, df_info, method)
			} else {
				if (portfolio_value >= 0) {
					pos_results_df <- results_update(pos_results_df, df_info, method)
				} else {
					neg_results_df <- results_update(neg_results_df, df_info, method)
				}
			}

		}

		if (progress) { pb$tick() } # update progress bar

	}

	rm(i, trx_type, trx_asset, trx_qty, trx_prz, trx_dtt,
		 previous_dtt, ptf_qty, ptf_prz, ptf_dtt, df_info, portfolio_value)

	# compute the mean expected return for RG, RL, PG, and PL
	if (!posneg_portfolios) {
		results_df <- meanvalue_compute(results_df, asset_ntrx)
	} else {
		if (portfolio_value >= 0) {
			pos_results_df <- meanvalue_compute(pos_results_df, asset_ntrx)
		} else {
			neg_results_df <- meanvalue_compute(neg_results_df, asset_ntrx)
		}
	}


	# join the dataframes and return a single result dataframe
	if (method != "none") {
		if (!posneg_portfolios) {
			final_res <- dplyr::left_join(portfolio, results_df, by = c("client", "asset"))
			# inserire value / nrow() o per il count
		} else {
			pos_results_df$type <- "positive"
			neg_results_df$type <- "negative"
			results_df <- dplyr::bind_rows(pos_results_df, neg_results_df)
			final_res <- dplyr::left_join(portfolio, results_df, by = c("client", "asset")) %>%
				dplyr::relocate(type, .after = prz)
			# inserire value / nrow() or per il count
		}

	} else {
		final_res <- portfolio
	}

	return(final_res) # return the updated portfolio

}

