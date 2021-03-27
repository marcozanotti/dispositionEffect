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
#' @param method Character string containing the method to use to compute
#'   realized and paper gains and losses. If "none" nothing is computed but the
#'   investor's portfolio updates. Otherwise it has to be one of "count" (default),
#'   "total", "value", "duration", or "all".
#' @param exact_market_prices Logical. If TRUE then \code{\link{closest_market_price}}
#'   uses exact datetime match to look for the closest price of each asset.
#'   It usually speeds up computation by a small degree, but it requires the
#'   `market_prices` to have the prices for each transaction asset along each
#'   transaction datatimes.
#' @param portfolio_driven_DE Logical. If TRUE the realized and paper gains and
#'   losses for the positive (that is when the investor's portfolio value, as
#'   computed through \code{\link{evaluate_portfolio}}, is greater than zero)
#'   and the negative (that is when the investor's portfolio value, as computed
#'   through \code{\link{evaluate_portfolio}}, is smaller than zero) portfolios
#'   are returned.
#' @param time_series_DE Logical. If TRUE the time series of disposition effect
#'   is computed on 'count' and 'value' methods only.
#' @param assets_time_series_DE Character vector of assets' names as contained
#'   into `portfolio_transactions` on which to compute the time series disposition
#'   effect.
#' @param verbose Numeric or logical vector of length 2 that allows to control
#'   for the function verbosity.
#' @param progress Logical. If TRUE a progress bar is displayed.
#'
#' @return A data frame containing the investor's
#'   portfolio and the values of realized and paper gains and losses
#'   computed by means of the chosen method on each portfolio assets.
#'
#'   If time_series_DE is set to TRUE, then also time series disposition effect
#'   results are returned.
#'
#' @seealso \code{\link{realized_compute}}, \code{\link{paper_compute}},
#'   \code{\link{gains_losses}}
#'
#' @export
portfolio_compute <- function(
	portfolio_transactions,
	market_prices,
	method = "count",
	allow_short = TRUE,
	time_threshold = "0 mins",
	exact_market_prices = TRUE,
	portfolio_driven_DE = FALSE,
	time_series_DE = FALSE,
	assets_time_series_DE = NULL,
	verbose = c(0, 0),
	progress = FALSE
) {

	# defensive programming
	# check portfolio_transactions column names
	trg <- c("investor", "type", "asset", "quantity", "price", "datetime")
	chk <- check_values(names(portfolio_transactions), trg)
	if (!is.null(chk)) {
		stop(paste0("portfolio_transactions must contain columns '", paste(trg, collapse = "', '"), "'.\n",
								"Can't find column(s) '", paste(chk, collapse = "', '"), "'. Possibly misspelled column names?\n"),
				 call. = FALSE)
	}
	# check market_prices column names
	trg <- c("asset", "datetime", "price")
	chk <- check_values(names(market_prices), trg)
	if (!is.null(chk)) {
		stop(paste0("market_prices must contain columns '", paste(trg, collapse = "', '"), "'.\n",
								"Can't find column(s) '", paste(chk, collapse = "', '"), "'. Possibly misspelled column names?\n"),
				 call. = FALSE)
	}
	# check portfolio_transactions column type values
	trg <- c("B", "S")
	chk <- check_values(unique(portfolio_transactions$type), trg, no_exception = TRUE, weak_target = TRUE)
	if (!is.null(chk$target) | !is.null(chk$input)) {
		stop(paste0("Column type of portfolio_transactions should contain 'B' or 'S' only.\n"), call. = FALSE)
	}
	# check method argument
	trg <- c("count", "total", "value", "duration", "all", "none")
	chk <- check_values(method, trg, weak_target = TRUE)
	if (!is.null(chk)) {
		stop(paste0("method should be one of '", paste(trg, collapse = "', '"), "'.\n"), call. = FALSE)
	}
	# check time_threshold argument
	trg <- c("sec", "min", "hour", "day", "week")
	chk <- grepl("(sec)|(min)|(hour)|(day)|(week)", time_threshold)
	if (!chk) {
		stop(paste0("time_threshold units should be one of '", paste(trg, collapse = "', '"), "'.\n"), call. = FALSE)
	}
	# check method & time_series_DE
	if (method %in% c("total", "duration", "none") & time_series_DE) {
		warning("time_series_DE computation does not exist for method 'total', 'duration' and 'none'. Forced time_series_DE to FALSE",
						call. = FALSE)
		time_series_DE <- FALSE
	}
	# check portfolio_driven_DE & time_series_DE
	if (portfolio_driven_DE & time_series_DE) {
		warning("time_series_DE computation is not allowed with portfolio_driven_DE. Forced time_series_DE to FALSE",
						call. = FALSE)
		time_series_DE <- FALSE
	}
	# check time_series_DE & assets_time_series_DE
	if (!time_series_DE & !is.null(assets_time_series_DE)) {
		warning("assets_time_series_DE can only be used with time_series_DE set to TRUE. Forced asset_time_series_DE to NULL",
						call. = FALSE)
		assets_time_series_DE <- NULL
	}
	# check assets_time_series_DE values
	if (!is.null(assets_time_series_DE)) {
		trg <- unique(portfolio_transactions$asset)
		chk <- check_values(assets_time_series_DE, trg, no_exception = TRUE, weak_target = TRUE)
		if (!is.null(chk$target) | !is.null(chk$input)) {
			stop(paste0("assets_time_series_DE must contain valid assets' names (present into portfolio_transactions).\n"), call. = FALSE)
		}
	}
	# check exact_market_prices
	if (!exact_market_prices) {
		message("Note: exact_market_prices set to FALSE, unreliable results may be obtained when
						transactions occur with low frequency.\n")
	}


	# verbosity
	verb_lvl1 <- as.logical(verbose[1])
	verb_lvl2 <- as.logical(verbose[2])

	# global parameters
	investor_id <- portfolio_transactions$investor[1]
	investor_assets <- sort(unique(portfolio_transactions$asset), method = "radix")
	investor_datetimes <- portfolio_transactions$datetime
	asset_numtrx <- portfolio_transactions %>%
		dplyr::group_by(!!rlang::sym("asset")) %>%
		dplyr::summarise(numtrx = dplyr::n(), .groups = "drop")
	if (!is.null(assets_time_series_DE)) {
		assets_time_series_DE <- sort(assets_time_series_DE, method = "radix")
	}


	# investor's initial portfolio (portfolio at time 0):
	# an empty df with all the assets traded by the investor
	# with qty = NA and prz = NA for all the assets (initial condition)
	portfolio <- initializer_portfolio(investor_id, investor_assets)

	# initialize the df of computations: RG, RL, PG, PL and other
	if (!portfolio_driven_DE) {
		results_df <- initializer_realized_and_paper(investor_id, investor_assets, method)
	} else {
		pos_results_df <- initializer_realized_and_paper(investor_id, investor_assets, method)
		neg_results_df <- initializer_realized_and_paper(investor_id, investor_assets, method)
	}

	# initialize the df of time series DE
	if (time_series_DE) {
		DE_results_df <- initializer_timeseries_DE(investor_id, assets_time_series_DE, investor_datetimes, method)
	}


	# progress bar
	if (progress) {
		# initialize progress bar
		pb <- progress::progress_bar$new(
			format = ":current  [:bar] :percent in :elapsed\n\n",
			total = nrow(portfolio_transactions),
			clear = FALSE, width = 60, show_after = 0
		)
		pb$tick(0)
	}


	for (i in seq_len(nrow(portfolio_transactions))) {

		# extract scalars (trx = transaction)
		trx_type <- portfolio_transactions[i, ]$type # trx type
		trx_asset <- portfolio_transactions[i, ]$asset # trx asset
		trx_qty <- portfolio_transactions[i, ]$quantity # trx quantity
		trx_prz <- portfolio_transactions[i, ]$price # trx price
		trx_dtt <- portfolio_transactions[i, ]$datetime # trx datetime
		previous_dtt <- portfolio_transactions[i - 1, ]$datetime

		# if it's a sell transaction then consider qty as negative
		if (trx_type == "S") {
			trx_qty <- trx_qty * -1L
		}

		# extract assets already into portfolio
		ptf_assets <- portfolio[!is.na(portfolio$quantity) & portfolio$quantity != 0, ]$asset

		portfolio_adjusted <- portfolio
		market_przs <- gainloss_df <- portfolio_value <- NULL

		# if method is not "none" and the portfolio is not empty (initial condition),
		# then calls closest_market_price, gains_losses and evaluate_portfolio
		if (method != "none" & length(ptf_assets) > 0) {

			# if the portfolio contains more assets than the traded asset, then extract
			# the market prices at transaction_datetime of all the portfolio assets
			ptf_assets <- ptf_assets[ptf_assets %!in% trx_asset]
			if (length(ptf_assets) > 0) {
				# if the market_prices contain the exact datetimes of portfolio transactions
				# then exact computation can be performed and the market_prices data frame
				# can be sliced rolling forward to speed up computations
				if (exact_market_prices) {
					market_przs <- closest_market_price(ptf_assets, trx_dtt, market_prices, exact = TRUE)[, -2]
					# market_prices <- market_prices[market_prices$datetime >= trx_dtt, ]
				} else {
					market_przs <- closest_market_price(ptf_assets, trx_dtt, market_prices)[, -2]
				}

				chk_mp <- check_values(market_przs$asset, ptf_assets)
				if (!is.null(chk_mp)) {
					warning(paste0("Investor ", investor_id, ", transaction ", i, ", datetime ", trx_dtt, ":\n",
												 "No market prices available for asset(s) ", paste(chk_mp, collapse = ", "), "\n"),
									call. = FALSE)
					portfolio_adjusted <- portfolio_adjusted[portfolio_adjusted$asset %!in% chk_mp, ]
				}

				market_przs <- market_przs[order(factor(market_przs$asset, levels = ptf_assets), method = "radix"), ]

			} else {
				market_przs <- data.frame("asset" = trx_asset, "price" = trx_prz)
			}


			# compute RG/RL/PG/PL
			if (verb_lvl1) message("Start computing RG/RL/PG/PL..")
			gainloss_df <- gains_losses(
				portfolio = portfolio_adjusted,
				market_prices = market_przs,
				transaction_type = trx_type,
				transaction_asset = trx_asset,
				transaction_quantity = trx_qty,
				transaction_price = trx_prz,
				transaction_datetime = trx_dtt,
				previous_datetime = previous_dtt,
				time_threshold = time_threshold,
				method = method,
				allow_short = allow_short,
				verbose = verb_lvl2
			)
			if (method %in% c("value", "all")) {
				chk_gl <- check_gainloss(gainloss_df)
				if (!is.null(chk_gl)) {
					warning(paste0("Investor ", investor_id, ", transaction ", i, ":\n", chk_gl), call. = FALSE)
				}
			}

			# evaluate global portfolio value
			if (verb_lvl1) message("Evaluating global portfolio position..")
			portfolio_value <- evaluate_portfolio(
				portfolio = portfolio,
				market_prices = rbind(market_przs, data.frame("asset" = trx_asset, "price" = trx_prz))
			)


		}

		# update the portfolio
		if (verb_lvl1) message(paste0("Updating portfolio.. (", trx_asset, " asset)"))
		portfolio <- update_portfolio(portfolio, trx_asset, trx_qty, trx_prz, trx_dtt, trx_type)

		# update the results_df
		if (method != "none" & !is.null(gainloss_df)) {
			if (verb_lvl1) message("Updating realized and paper results..")
			if (!portfolio_driven_DE) {
				results_df <- update_realized_and_paper(results_df, gainloss_df, method)
			} else {
				if (portfolio_value >= 0) {
					pos_results_df <- update_realized_and_paper(pos_results_df, gainloss_df, method)
				} else {
					neg_results_df <- update_realized_and_paper(neg_results_df, gainloss_df, method)
				}
			}
		}

		# time series disposition effect
		if (time_series_DE & i != 1) {
			if (verb_lvl1) message("Computing time series DE..")
			DE_results_df <- update_timeseries_DE(
				timeseries_DE = DE_results_df,
				realized_and_paper = results_df,
				gainslosses = gainloss_df,
				transaction_id = i,
				assets_time_series_DE,
				method
			)
		}

		if (verb_lvl1) message("Done!")
		if (progress) {
			pb$tick()
		} # update progress bar

		rm(
			trx_type, trx_asset, trx_qty, trx_prz, trx_dtt, previous_dtt,
			ptf_assets, market_przs, gainloss_df, portfolio_value, portfolio_adjusted
		)


	} # close loop


	# compute the mean expected return for RG, RL, PG, and PL value
	if (method %in% c("value", "all")) {
		if (!portfolio_driven_DE) {
			results_df <- update_expectedvalue(results_df, asset_numtrx)
		} else {
				pos_results_df <- update_expectedvalue(pos_results_df, asset_numtrx)
				neg_results_df <- update_expectedvalue(neg_results_df, asset_numtrx)
		}
	}

	# join the dataframes and return a single result dataframe
	if (method != "none") {
		if (!portfolio_driven_DE) {
			final_res <- dplyr::left_join(portfolio, results_df, by = c("investor", "asset"))
		} else {
			pos_results_df$type <- "positive"
			neg_results_df$type <- "negative"
			results_df <- dplyr::bind_rows(pos_results_df, neg_results_df)
			final_res <- dplyr::left_join(portfolio, results_df, by = c("investor", "asset"))
		  final_res <- dplyr::relocate(final_res, !!rlang::sym("type"), .after = !!rlang::sym("datetime"))
		}

	} else {
		final_res <- portfolio
	}

	if (time_series_DE) {
		final_res <- list("portfolio" = final_res, "timeseries" = DE_results_df)
	}

	return(final_res) # return the updated portfolio

}
