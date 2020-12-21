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
#' @inheritParams closest_historical_price
#' @inheritParams difference_in_time
#' @param portfolio Data frame of the investor's portfolio at time t.
#' @param verbose Numeric vector of length 2 that allows to control
#'   for the function verbosity.
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
#'   \code{\link{portfolio_update}}
#'
#' @export
gains_and_losses <- function(trx_type, trx_asset, trx_qty, trx_prz, trx_dtt, last_dtt,
														 portfolio, df_asset_prices, time_threshold = "5 mins",
														 method = "all", allow_short = FALSE,
														 verbose = c(0, 0)) {

	# checks on inputs
	# trx_type
	msg <- check_var_types("trx_type", class(trx_type), "character", multiple = FALSE)
	if (!is.null(msg)) { stop(msg, call. = FALSE) }
	# trx_asset
	msg <- check_var_types("trx_asset", class(trx_asset), "character", multiple = FALSE)
	if (!is.null(msg)) { stop(msg, call. = FALSE) }
	# trx_qty
	msg <- check_var_types("trx_qty", class(trx_qty), "integer", multiple = FALSE)
	if (!is.null(msg)) { stop(msg, call. = FALSE) }
	# trx_prz
	msg <- check_var_types("trx_prz", class(trx_prz), "numeric", multiple = FALSE)
	if (!is.null(msg)) { stop(msg, call. = FALSE) }
	# trx_dtt
	msg <- check_var_types("trx_dtt", class(trx_dtt)[1], "POSIXct", multiple = FALSE)
	if (!is.null(msg)) { stop(msg, call. = FALSE) }
	# last_dtt
	msg <- check_var_types("last_dtt", class(last_dtt)[1], "POSIXct", multiple = FALSE)
	if (!is.null(msg)) { stop(msg, call. = FALSE) }
	# verbosity
	verb <- verbose[2] == 1


	# qty, prz and dtt of trx_asset already into portfolio
	ptf_qty <- portfolio[portfolio$asset == trx_asset, ]$qty
	ptf_prz <- portfolio[portfolio$asset == trx_asset, ]$prz
	ptf_dtt <- portfolio[portfolio$asset == trx_asset, ]$dtt

	# extract assets already into portfolio
	ptf_assets <- portfolio[!is.na(portfolio$qty), ]$asset
	if (!length(ptf_assets)) {
		# if the portfolio is empty (initial condition), exit the function
		return(NULL)
	}


	if (compare_difftime(last_dtt, trx_dtt, time_threshold) == "greater") {
		# if the difference trx_dtt - last_dtt >= time_threshold --> compute realized and paper


		if (is.na(ptf_qty) || ptf_qty == 0) { # if ptf_qty (qty of trx_asset) is NA or 0, compute paper g&l of other assets

			if (verb) message("\nComputing paper gains and losses..")
			hist_prz <- purrr::map_dbl(ptf_assets, closest_historical_price, trx_dtt, df_asset_prices) # extract the historical prices at trx_dtt of all the portfolio assets but the trx_asset
			portf_prz <- portfolio[portfolio$asset %in% ptf_assets, ]$prz # extract the portfolio asset prices but the trx_asset
			portf_qty <- portfolio[portfolio$asset %in% ptf_assets, ]$qty # extract the portfolio asset quantities but the trx_asset
			# compute paper gains and losses
			realized_paper_df <- paper_compute(portf_qty, portf_prz, hist_prz, last_dtt, trx_dtt, ptf_assets,
																				 allow_short, method)

		} else {# compute gains and losses for all the assets

			ptf_assets <- ptf_assets[ptf_assets != trx_asset] # remove trx_asset from assets already into portfolio

			if (!length(ptf_assets)) { # if there are no other assets but the trx_asset, just compute on trx_asset

				if (verb) message("\nComputing realized gains and losses..")
				realized_paper_df <- realized_compute(ptf_qty, ptf_prz, trx_qty, trx_prz, trx_type,
																							ptf_dtt, last_dtt, trx_dtt, trx_asset,
																							allow_short, realized_only = FALSE, method)

			} else {# compute on both, trx_asset and ptf_assets

				hist_prz <- purrr::map_dbl(ptf_assets, closest_historical_price, trx_dtt, df_asset_prices) # extract the historical prices at trx_dtt of all the portfolio assets but the trx_asset
				portf_prz <- portfolio[portfolio$asset %in% ptf_assets, ]$prz # extract the portfolio asset prices but the trx_asset
				portf_qty <- portfolio[portfolio$asset %in% ptf_assets, ]$qty # extract the portfolio asset quantities but the trx_asset
				# compute realized and paper gains and losses
				if (verb) message("\nComputing realized and paper gains and losses..")
				realized_df <- realized_compute(ptf_qty, ptf_prz, trx_qty, trx_prz, trx_type,
																				ptf_dtt, last_dtt, trx_dtt, trx_asset,
																				allow_short, realized_only = FALSE, method)
				paper_df <- paper_compute(portf_qty, portf_prz, hist_prz, last_dtt, trx_dtt, ptf_assets,
																	allow_short, method)
				# bind rows of trx_asset and ptf_assets results
				realized_paper_df <- rbind(realized_df, paper_df)

			}

		}


	} else {
		# if the difference trx_dtt - last_dtt < time_threshold --> compute only realized


		if (is.na(ptf_qty) || ptf_qty == 0) { # if ptf_qty (qty of trx_asset) is NA or 0, return an empty df

			realized_paper_df <- realized_empty(trx_asset, method)

		} else {# compute gains and losses for all the assets

			if (verb) message("\nComputing realized gains and losses..")
			realized_paper_df <- realized_compute(ptf_qty, ptf_prz, trx_qty, trx_prz, trx_type,
																						ptf_dtt, last_dtt, trx_dtt, trx_asset,
																						allow_short, realized_only = TRUE, method)


		}


	}

	realized_paper_df <- cbind(portfolio[1, "client"], realized_paper_df) %>%
		tibble::as_tibble()
	return(realized_paper_df)

}

