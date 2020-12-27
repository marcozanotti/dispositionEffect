#' @title Gains & Losses
#'
#' @description Calculation of the realized gains and losses and the paper
#'   gains and losses.
#'
#' @details It is essentially a wrapper of the \code{\link{paper_compute}}
#'   and the \code{\link{realized_compute}} functions.
#'
#' @inheritParams paper_compute
#' @inheritParams realized_compute
#' @inheritParams closest_market_price
#' @inheritParams difference_in_time
#' @param portfolio Data frame of the investor's portfolio at time t.
#' @param verbose Logical. If TRUE than messages are printed to the console.
#'
#' @return A [tibble][tibble::tibble-package] containing the values of
#'   realized and paper gains and losses computed by means of the chosen
#'   method on each portfolio assets.
#'
#' @author L. Mazzucchelli & M. Zanotti
#'
#' @references H. Shefrin & M. Statman, 1985
#'
#' @seealso \code{\link{realized_compute}}, \code{\link{paper_compute}},
#'   \code{\link{portfolio_compute}}
#'
#' @export
gains_and_losses <- function(transaction_type,
														 transaction_asset,
														 transaction_quantity,
														 transaction_price,
														 transaction_datetime,
														 previous_datetime,
														 portfolio,
														 market_prices,
														 unit = "15 mins",
														 time_threshold = "5 mins",
														 method = "all",
														 allow_short = FALSE,
														 verbose = FALSE) {

	# # checks on inputs
	# # transaction_type
	# msg <- check_var_types("transaction_type", class(transaction_type), "character", multiple = FALSE)
	# if (!is.null(msg)) { stop(msg, call. = FALSE) }
	# # transaction_asset
	# msg <- check_var_types("transaction_asset", class(transaction_asset), "character", multiple = FALSE)
	# if (!is.null(msg)) { stop(msg, call. = FALSE) }
	# # transaction_quantity
	# msg <- check_var_types("transaction_quantity", class(transaction_quantity), "integer", multiple = FALSE)
	# if (!is.null(msg)) { stop(msg, call. = FALSE) }
	# # transaction_price
	# msg <- check_var_types("transaction_price", class(transaction_price), "numeric", multiple = FALSE)
	# if (!is.null(msg)) { stop(msg, call. = FALSE) }
	# # transaction_datetime
	# msg <- check_var_types("transaction_datetime", class(transaction_datetime)[1], "POSIXct", multiple = FALSE)
	# if (!is.null(msg)) { stop(msg, call. = FALSE) }
	# # previous_datetime
	# msg <- check_var_types("previous_datetime", class(previous_datetime)[1], "POSIXct", multiple = FALSE)
	# if (!is.null(msg)) { stop(msg, call. = FALSE) }

	# verbosity
	verb <- verbose

	# qty, prz and dtt of transaction_asset already into portfolio
	ptf_qty <- portfolio[portfolio$asset == transaction_asset, ]$quantity
	ptf_prz <- portfolio[portfolio$asset == transaction_asset, ]$price
	ptf_dtt <- portfolio[portfolio$asset == transaction_asset, ]$datetime

	# extract assets already into portfolio
	ptf_assets <- portfolio[!is.na(portfolio$quantity), ]$asset
	if (!length(ptf_assets)) {
		# if the portfolio is empty (initial condition), exit the function
		return(NULL)
	}


	if (difftime_compare(previous_datetime, transaction_datetime, time_threshold) == "greater") {
		# if the difference transaction_datetime - previous_datetime >= time_threshold --> compute realized and paper

		if (is.na(ptf_qty) || ptf_qty == 0) { # if ptf_qty (qty of transaction_asset) is NA or 0, compute paper g&l of other assets

			if (verb) message("Computing paper gains and losses..")
			# extract the market prices at transaction_datetime of all the portfolio assets but the transaction_asset
			market_przs <- purrr::map_dbl(ptf_assets, closest_market_price,
																    transaction_datetime, market_prices, unit)
			pft_assets_qtys <- portfolio[portfolio$asset %in% ptf_assets, ]$quantity # extract the portfolio asset quantities but the transaction_asset
			pft_assets_przs <- portfolio[portfolio$asset %in% ptf_assets, ]$price # extract the portfolio asset prices but the transaction_asset
			# compute paper gains and losses
			realized_paper_df <- paper_compute(pft_assets_qtys,
																				 pft_assets_przs,
																				 market_przs,
																				 previous_datetime,
																				 transaction_datetime,
																				 ptf_assets,
																				 allow_short,
																				 method)

		} else {# compute gains and losses for all the assets

			ptf_assets <- ptf_assets[ptf_assets != transaction_asset] # remove transaction_asset from assets already into portfolio

			if (!length(ptf_assets)) { # if there are no other assets but the transaction_asset, just compute on transaction_asset

				if (verb) message("Computing realized gains and losses..")
				realized_paper_df <- realized_compute(ptf_qty,
																							ptf_prz,
																							transaction_quantity,
																							transaction_price,
																							transaction_type,
																							ptf_dtt,
																							previous_datetime,
																							transaction_datetime,
																							transaction_asset,
																							allow_short,
																							realized_only = FALSE,
																							method)

			} else {# compute on both, transaction_asset and ptf_assets

				# extract the market prices at transaction_datetime of all the portfolio assets but the transaction_asset
				market_przs <- purrr::map_dbl(ptf_assets, closest_market_price,
																	    transaction_datetime, market_prices, unit)
				pft_assets_qtys <- portfolio[portfolio$asset %in% ptf_assets, ]$quantity # extract the portfolio asset quantities but the transaction_asset
				pft_assets_przs <- portfolio[portfolio$asset %in% ptf_assets, ]$price # extract the portfolio asset prices but the transaction_asset
				# compute realized and paper gains and losses
				if (verb) message("Computing realized and paper gains and losses..")
				realized_df <- realized_compute(ptf_qty,
																				ptf_prz,
																				transaction_quantity,
																				transaction_price,
																				transaction_type,
																				ptf_dtt,
																				previous_datetime,
																				transaction_datetime,
																				transaction_asset,
																				allow_short,
																				realized_only = FALSE,
																				method)
				paper_df <- paper_compute(pft_assets_qtys,
																	pft_assets_przs,
																	market_przs,
																	previous_datetime,
																	transaction_datetime,
																	ptf_assets,
																	allow_short,
																	method)
				# bind rows of transaction_asset and ptf_assets results
				realized_paper_df <- rbind(realized_df, paper_df)

			}

		}


	} else {

		# if the difference transaction_datetime - previous_datetime < time_threshold --> compute only realized
		if (is.na(ptf_qty) || ptf_qty == 0) { # if ptf_qty (qty of transaction_asset) is NA or 0, return an empty df

			realized_paper_df <- realized_empty(transaction_asset, method)

		} else {# compute gains and losses for all the assets

			if (verb) message("Computing realized gains and losses..")
			realized_paper_df <- realized_compute(ptf_qty,
																						ptf_prz,
																						transaction_quantity,
																						transaction_price,
																						transaction_type,
																						ptf_dtt,
																						previous_datetime,
																						transaction_datetime,
																						transaction_asset,
																						allow_short,
																						realized_only = TRUE,
																						method)

		}


	}

	realized_paper_df <- cbind(portfolio[1, "investor"], realized_paper_df) %>%
		tibble::as_tibble()
	return(realized_paper_df)

}

