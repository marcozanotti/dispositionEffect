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
gains_and_losses <- function(
	portfolio,
	market_prices,
	transaction_type,
	transaction_asset,
	transaction_quantity,
	transaction_price,
	transaction_datetime,
	previous_datetime,
	time_threshold = "0 mins",
	method = "all",
	allow_short = FALSE,
	verbose = FALSE
) {

	# verbosity
	verb <- verbose

	# qty, prz and dtt of transaction_asset already into portfolio
	ptf_qty <- portfolio[portfolio$asset == transaction_asset, ]$quantity
	ptf_prz <- portfolio[portfolio$asset == transaction_asset, ]$price
	ptf_dtt <- portfolio[portfolio$asset == transaction_asset, ]$datetime

	# extract assets already into portfolio
	ptf_assets <- portfolio[!is.na(portfolio$quantity) & portfolio$quantity != 0, ]$asset
	# remove transaction_asset from assets already into portfolio
	ptf_assets <- ptf_assets[ptf_assets != transaction_asset]

	# if the difference transaction_datetime - previous_datetime >= time_threshold
	# --> compute realized and paper
	if (difftime_compare(previous_datetime, transaction_datetime, time_threshold) == "greater") {

		# if ptf_qty (qty of transaction_asset) is NA or 0, compute paper g&l of other assets
		if (is.na(ptf_qty) | ptf_qty == 0) {

			# if there are no other assets but the transaction_asset, return an empty df
			if (!length(ptf_assets)) {
				realized_paper_df <- realized_empty(transaction_asset, method)
			} else {
				if (verb) message("Computing paper gains and losses..")
				# extract the portfolio assets' quantities, prices and market prices
				pft_assets_qtys <- portfolio[portfolio$asset %in% ptf_assets, ]$quantity
				pft_assets_przs <- portfolio[portfolio$asset %in% ptf_assets, ]$price
				ptf_assets_market_przs <- market_prices[market_prices$asset %in% ptf_assets, ]$price
				# compute paper gains and losses
				realized_paper_df <- paper_compute(
					pft_assets_qtys,
					pft_assets_przs,
					ptf_assets_market_przs,
					previous_datetime,
					transaction_datetime,
					ptf_assets,
					allow_short,
					method
				)
			}

		} else {# compute gains and losses for all the assets

			# if there are no other assets but the transaction_asset, compute on transaction_asset
			if (length(ptf_assets) < 1) {
				if (verb) message("Computing realized gains and losses..")
				realized_paper_df <- realized_compute(
					ptf_qty,
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
					method
				)
			} else {# compute on both, transaction_asset and ptf_assets

				# extract the portfolio assets' quantities, prices and market prices
				pft_assets_qtys <- portfolio[portfolio$asset %in% ptf_assets, ]$quantity
				pft_assets_przs <- portfolio[portfolio$asset %in% ptf_assets, ]$price
				ptf_assets_market_przs <- market_prices[market_prices$asset %in% ptf_assets, ]$price

				# compute realized and paper gains and losses
				if (verb) message("Computing realized and paper gains and losses..")
				realized_df <- realized_compute(
					ptf_qty,
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
					method
				)
				paper_df <- paper_compute(
					pft_assets_qtys,
					pft_assets_przs,
					ptf_assets_market_przs,
					previous_datetime,
					transaction_datetime,
					ptf_assets,
					allow_short,
					method
				)
				# bind rows of transaction_asset and ptf_assets results
				realized_paper_df <- dplyr::bind_rows(realized_df, paper_df)

			}

		}


		# if the difference transaction_datetime - previous_datetime < time_threshold
		# --> compute only realized
	} else {

		# if ptf_qty (qty of transaction_asset) is NA or 0, return an empty df
		if (is.na(ptf_qty) | ptf_qty == 0) {
			realized_paper_df <- realized_empty(transaction_asset, method)
		} else {# compute realized gains and losses for the transaction_asset

			if (verb) message("Computing realized gains and losses..")
			realized_paper_df <- realized_compute(
				ptf_qty,
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
				method
			)

		}

	}

	realized_paper_df <- dplyr::bind_cols(
		data.frame("investor" = portfolio[["investor"]][1]),
		realized_paper_df
	)

	return(realized_paper_df)

}
