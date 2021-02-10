#' @name portfolio_compute
#'
#' @title Portfolio Compute
#'
#' @description Computation of all the transaction updates and the
#'   realized and paper gains and losses for each assets.
#'
#' @param portfolio_transactions Data frame. The investor's transactions data frame.
#' @inheritParams closest_market_price
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
#' @param verbose Numeric or logical vector of length 2 that allows to control
#'   for the function verbosity.
#' @param progress Logical. If TRUE a progress bar is displayed.
#' @param ... Further arguments to be passed to \code{portfolio_compute}.
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
#'   \code{\link{gains_and_losses}}
#'
#' @export
portfolio_compute <- function(portfolio_transactions,
														  market_prices,
														  method = "all",
														  allow_short = TRUE,
														  time_threshold = "0 mins",
														  posneg_portfolios = FALSE,
														  portfolio_statistics = FALSE,
														  verbose = c(0, 0),
														  progress = FALSE) {

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
	verb_lvl1 <- as.logical(verbose[1])
	verb_lvl2 <- as.logical(verbose[2])

	# global parameters
	investor_id <- portfolio_transactions$investor[1]
	investor_assets <- unique(portfolio_transactions$asset)
	asset_numtrx <- portfolio_transactions %>%
		dplyr::group_by(!!rlang::sym("asset")) %>%
		dplyr::summarise(numtrx = dplyr::n(), .groups = "drop")

	# investor's initial portfolio (portfolio at time 0):
	# an empty df with all the assets traded by the investor
	# with qty = NA and prz = NA for all the assets (initial condition)
	portfolio <- initializer_portfolio(investor_id, investor_assets)

	# initialize the df of computation: RG, RL, PG, PL and other
	if (!posneg_portfolios) {
		results_df <- initializer_realized_and_paper(investor_id, investor_assets, method)
	} else {
		pos_results_df <- initializer_realized_and_paper(investor_id, investor_assets, method)
		neg_results_df <- initializer_realized_and_paper(investor_id, investor_assets, method)
	}

	# progress bar
	if (progress) {
		# initialize progress bar
		pb <- progress::progress_bar$new(format = ":current  [:bar] :percent in :elapsed\n\n",
													           total = nrow(portfolio_transactions),
													           clear = FALSE,
																		 width = 60,
																		 show_after = 0)
		pb$tick(0)
	}

	for (i in 1:nrow(portfolio_transactions)) {

		# extract scalars (trx = transaction)
		trx_type <- portfolio_transactions[i, ]$type # trx type
		trx_asset <- portfolio_transactions[i, ]$asset # trx asset
		trx_qty <- portfolio_transactions[i, ]$quantity # trx quantity
		trx_prz <- portfolio_transactions[i, ]$price # trx price
		trx_dtt <- portfolio_transactions[i, ]$datetime # trx datetime
		previous_dtt <- portfolio_transactions[i - 1, ]$datetime

		# if it's a sell transaction then consider qty as negative
		if (trx_type == "S") { trx_qty <- trx_qty * -1L }

		# extract assets already into portfolio
		ptf_assets <- portfolio[!is.na(portfolio$quantity), ]$asset

		market_przs <- gainloss_df <- portfolio_value <- NULL

		# if method is not "none" and the portfolio is not empty (initial condition),
		# then calls closest_market_price, gains_and_losses and evaluate_portfolio
		if (method != "none" && length(ptf_assets) > 0) {

			# extract the market prices at transaction_datetime of all the portfolio assets
			market_przs <- purrr::map_df(ptf_assets, closest_market_price,
																	 trx_dtt, market_prices, price_only = FALSE)[, -2]

			# compute RG/RL/PG/PL
			if (verb_lvl1) message("Start computing RG/RL/PG/PL..")
			gainloss_df <- gains_and_losses(transaction_type = trx_type,
																			transaction_asset = trx_asset,
																			transaction_quantity = trx_qty,
																			transaction_price = trx_prz,
																			transaction_datetime = trx_dtt,
																			previous_datetime = previous_dtt,
																			portfolio = portfolio,
																			market_prices = market_przs,
																			time_threshold = time_threshold,
																			method = method,
																			allow_short = allow_short,
																			verbose = verb_lvl2)

			# evaluate global portfolio value
			if (verb_lvl1) message("Evaluating global portfolio position..")
			portfolio_value <- evaluate_portfolio(portfolio,
																						market_przs,
																						portfolio_statistics)

		}

		# update the portfolio
		if (verb_lvl1) message(paste0("Updating portfolio.. (", trx_asset, " asset)"))
		portfolio <- update_portfolio(portfolio,
																	trx_asset,
																	trx_qty,
																	trx_prz,
																	trx_dtt,
																	trx_type)

		# update the results_df
		if (method != "none" && !is.null(gainloss_df)) {
			if (verb_lvl1) message("Updating realized and paper results..")
			if (!posneg_portfolios) {
				results_df <- update_realized_and_paper(results_df, gainloss_df, method)
			} else {
				if (portfolio_value >= 0) {
					pos_results_df <- update_realized_and_paper(pos_results_df, gainloss_df, method)
				} else {
					neg_results_df <- update_realized_and_paper(neg_results_df, gainloss_df, method)
				}
			}

		}

		if (verb_lvl1) message("Done!")
		if (progress) { pb$tick() } # update progress bar

		rm(trx_type, trx_asset, trx_qty, trx_prz, trx_dtt, previous_dtt,
			 ptf_assets, market_przs, gainloss_df, portfolio_value)


	} # close loop


	# compute the mean expected return for RG, RL, PG, and PL value
	if (method %in% c("value", "all")) {
		if (!posneg_portfolios) {
			results_df <- update_expectedvalue(results_df, asset_numtrx)
		} else {
			if (portfolio_value >= 0) {
				pos_results_df <- update_expectedvalue(pos_results_df, asset_numtrx)
			} else {
				neg_results_df <- update_expectedvalue(neg_results_df, asset_numtrx)
			}
		}
	}

	# join the dataframes and return a single result dataframe
	if (method != "none") {
		if (!posneg_portfolios) {
			final_res <- dplyr::left_join(portfolio, results_df, by = c("investor", "asset"))
		} else {
			pos_results_df$type <- "positive"
			neg_results_df$type <- "negative"
			results_df <- dplyr::bind_rows(pos_results_df, neg_results_df)
			final_res <- dplyr::left_join(portfolio, results_df, by = c("investor", "asset"))
		  final_res <- dplyr::relocate(!!rlang::sym("type"), .after = !!rlang::sym("datetime"))
		}

	} else {
		final_res <- portfolio
	}

	return(final_res) # return the updated portfolio

}


#' @describeIn portfolio_compute Temporary parallel version of portfolio_compute
#' @export
portfolio_compute_parallel <- function(portfolio_transactions, market_prices, ...) {

	investors_id <- purrr::map_chr(portfolio_transactions, ~purrr::pluck(., "investor")[1])
	portfolio_compute_safe <- purrr::safely(portfolio_compute)

	ncores <- future::availableCores()
	# if there are more than 2 cores than use parallel computing
	# otherwise use sequential computing
	# RULE: always leave at least 1 free core
	if ((ncores - 1) > 1) {
		new_plan <- "multiprocess"
	} else {
		new_plan <- "sequential"
	}
	old_plan <- future::plan(strategy = new_plan)

	res <- furrr::future_map(
		portfolio_transactions,
		portfolio_compute_safe,
		market_prices,
		...)

	res <- purrr::transpose(res)$result
	names(res) <- investors_id

	future::plan(old_plan) # set back the old plan

	return(res)

}
