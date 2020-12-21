#' @name updates
#'
#' @title Update investor's portfolio or realized and paper results
#'
#' @description Unpdate investor's portfolio or realized and paper results
#'
#' @details
#'
#' @param portfolio_price Numeric value. The portfolio price of the traded asset.
#' @param portfolio_quantity Numeric value. The portfolio quantity of the traded asset.
#' @inheritParams realized_compute
#' @inheritParams paper_compute
#' @param realized_and_paper Data frame containing the realized and paper gains and
#'   losses results to be updated.
#' @param new_realized_and_paper Data frame containing the realized and paper gains and
#'   losses results related to the new transaction.
#' @param asset_transactions Data frame containing the number of transactions for
#'   each asset traded by the investor.
#'
#'
#' @return
#'   The described functions have different return behaviours.
#'
#'   \describe{
#'     \item{\code{prz_update}}{returns a numeric value of the new portfolio
#'       price of the traded asset.}
#'     \item{\code{dtt_update}}{returns a datetime value of the new portfolio
#'       datetime of the traded asset.}
#'     \item{\code{results_update}}{returns a [tibble][tibble::tibble-package]
#'       containing the realized and paper gains and losses with the updated
#'       values.}
#'     \item{\code{meanvalue_compute}}{returns a [tibble][tibble::tibble-package]
#'       containing the realized and paper gains and losses with the updated
#'       values.}
#'   }
#'
#'   In particular:
#'
#'   \describe{
#'     \item{\code{RG_"method"}}{contains Realized Gains results.}
#'     \item{\code{RL_"method"}}{contains Realized Losses results.}
#'     \item{\code{PG_"method"}}{contains Paper Gains results.}
#'     \item{\code{PL_"method"}}{contains Paper Losses results.}
#'   }
#'
#' @author L. Mazzucchelli & M. Zanotti
#'
#' @references H. Shefrin & M. Statman, 1985
#'
#' @seealso \code{\link{portfolio_update}}, \code{\link{gains_and_losses}}
NULL


#' @describeIn updates Update the portfolio price of the traded asset.
#' @export
prz_update <- function(portfolio_quantity,
											 portfolio_price,
											 transaction_quantity,
											 transaction_price,
											 transaction_type) {

	# short positions are allowed but qty have to be negative

	# cases:
	# 0) portfolio_quantity + transaction_quantity = 0         =>  new_prz = 0
	# 1) portfolio_quantity = 0                   =>  new_prz = transaction_price
	# 2) portfolio_quantity > 0 & transaction_type = B    =>  new_prz = weighted mean of przs
	# 3) portfolio_quantity < 0 & transaction_type = S    =>  new_prz = weighted mean of przs

	# 4) portfolio_quantity > 0 & transaction_type = S
	#  4.1) if portfolio_quantity > abs(transaction_quantity)  =>  new_prz = portfolio_price
	#  4.2) if portfolio_quantity < abs(transaction_quantity)  =>  new_prz = transaction_price

	# 5) portfolio_quantity < 0 & transaction_type = B
	#  5.1) if abs(portfolio_quantity) > transaction_quantity  =>  new_prz = portfolio_price
	#  5.2) if abs(portfolio_quantity) < transaction_quantity  =>  new_prz = transaction_price

	if ((portfolio_quantity + transaction_quantity) == 0) {

		new_prz <- 0

	} else if (portfolio_quantity == 0) {

		new_prz <- transaction_price

	} else if (portfolio_quantity > 0 && transaction_type == "B") {

		new_prz <- (portfolio_price * portfolio_quantity +
									transaction_price * abs(transaction_quantity)) /
			(portfolio_quantity + abs(transaction_quantity))

	} else if (portfolio_quantity < 0 && transaction_type == "S") {

		new_prz <- (portfolio_price * abs(portfolio_quantity) +
									transaction_price * abs(transaction_quantity)) /
			(abs(portfolio_quantity) + abs(transaction_quantity))

	} else if (portfolio_quantity > 0 && transaction_type == "S") {

		if (portfolio_quantity > abs(transaction_quantity)) {
			new_prz <- portfolio_price
		} else {
			new_prz <- transaction_price
		}

	} else if (portfolio_quantity < 0 && transaction_type == "B") {

		if (abs(portfolio_quantity) > transaction_quantity) {
			new_prz <- portfolio_price
		} else {
			new_prz <- transaction_price
		}

	}

	return(new_prz)

}


#' @describeIn updates Update the portfolio datetime of the traded asset.
#' @export
dtt_update <- function(portfolio_quantity,
											 portfolio_datetime,
											 transaction_quantity,
											 transaction_datetime,
											 transaction_type) {

	# short positions are allowed but qty have to be negative

	# cases:
	# 0) portfolio_quantity + transaction_quantity = 0         =>  new_dtt = transaction_datetime
	# 1) portfolio_quantity = 0                   =>  new_dtt = transaction_datetime
	# 2) portfolio_quantity > 0 & transaction_type = B    =>  new_dtt = portfolio_datetime
	# 3) portfolio_quantity < 0 & transaction_type = S    =>  new_dtt = portfolio_datetime

	# 4) portfolio_quantity > 0 & transaction_type = S
	#  4.1) if portfolio_quantity > abs(transaction_quantity)  =>  new_dtt = portfolio_datetime
	#  4.2) if portfolio_quantity < abs(transaction_quantity)  =>  new_dtt = transaction_datetime

	# 5) portfolio_quantity < 0 & transaction_type = B
	#  5.1) if abs(portfolio_quantity) > transaction_quantity  =>  new_dtt = portfolio_datetime
	#  5.2) if abs(portfolio_quantity) < transaction_quantity  =>  new_dtt = transaction_datetime

	diff_qty <- portfolio_quantity + transaction_quantity


	if (diff_qty == 0) {

		new_dtt <- transaction_datetime

	} else if (portfolio_quantity == 0) {

		new_dtt <- transaction_datetime

	} else if (portfolio_quantity > 0 && transaction_type == "B") {

		new_dtt <- portfolio_datetime

	} else if (portfolio_quantity < 0 && transaction_type == "S") {

		new_dtt <- portfolio_datetime

	} else if (portfolio_quantity > 0 && transaction_type == "S") {

		if (diff_qty > 0) {
			new_dtt <- portfolio_datetime
		} else {
			new_dtt <- transaction_datetime
		}

	} else if (portfolio_quantity < 0 && transaction_type == "B") {

		if (diff_qty < 0) {
			new_dtt <- portfolio_datetime
		} else {
			new_dtt <- transaction_datetime
		}

	}

	return(new_dtt)

}


# add functions update_quantity and update_portfolio ++++++++++++++++++++++++++++++++++++++++++++++


#' @describeIn updates Update the realized and paper gains and losses
#'   results with the results obtained on the last occurred transaction.
#' @export
results_update <- function(realized_and_paper,
													 new_realized_and_paper,
													 method = "all") {

	realized_and_paper <- dplyr::arrange(realized_and_paper, asset)
	new_realized_and_paper <- dplyr::arrange(new_realized_and_paper, asset)
	assets <- new_realized_and_paper$asset

	# replace the realized_and_paper values corresponding to the assets present into new_realized_and_paper
	# if values are NA, then they are simply replaced with the new values of new_realized_and_paper
	# count columns are updated as the element-wise sum
	# total columns are updated as the element-wise sum
	# value columns are updated as the element-wise mean
	# (if values are 0 in the value columns, then they are simply replaced with the newest)

	if (method == "count") {

		realized_and_paper[realized_and_paper$asset %in% assets,] <-
			realized_and_paper[realized_and_paper$asset %in% assets,] %>%
			dplyr::mutate(
				RG_count = dplyr::case_when(is.na(RG_count) ~ new_realized_and_paper$RG_count,
														        TRUE ~ RG_count + new_realized_and_paper$RG_count),
				RL_count = dplyr::case_when(is.na(RL_count) ~ new_realized_and_paper$RL_count,
														        TRUE ~ RL_count + new_realized_and_paper$RL_count),
				PG_count = dplyr::case_when(is.na(PG_count) ~ new_realized_and_paper$PG_count,
														        TRUE ~ PG_count + new_realized_and_paper$PG_count),
				PL_count = dplyr::case_when(is.na(PL_count) ~ new_realized_and_paper$PL_count,
														        TRUE ~ PL_count + new_realized_and_paper$PL_count)
			)

	} else if (method == "total") {

		realized_and_paper[realized_and_paper$asset %in% assets,] <-
			realized_and_paper[realized_and_paper$asset %in% assets,] %>%
			dplyr::mutate(
				RG_total = dplyr::case_when(is.na(RG_total) ~ new_realized_and_paper$RG_total,
														        TRUE ~ RG_total + new_realized_and_paper$RG_total),
				RL_total = dplyr::case_when(is.na(RL_total) ~ new_realized_and_paper$RL_total,
														        TRUE ~ RL_total + new_realized_and_paper$RL_total),
				PG_total = dplyr::case_when(is.na(PG_total) ~ new_realized_and_paper$PG_total,
														        TRUE ~ PG_total + new_realized_and_paper$PG_total),
				PL_total = dplyr::case_when(is.na(PL_total) ~ new_realized_and_paper$PL_total,
														        TRUE ~ PL_total + new_realized_and_paper$PL_total)
			)

	} else if (method == "value") {

		realized_and_paper[realized_and_paper$asset %in% assets,] <-
			realized_and_paper[realized_and_paper$asset %in% assets,] %>%
			dplyr::mutate(
				RG_value = dplyr::case_when(is.na(RG_value) ~ new_realized_and_paper$RG_value,
														        TRUE ~ RG_value + new_realized_and_paper$RG_value),
				RL_value = dplyr::case_when(is.na(RL_value) ~ new_realized_and_paper$RL_value,
														        TRUE ~ RL_value + new_realized_and_paper$RL_value),
				PG_value = dplyr::case_when(is.na(PG_value) ~ new_realized_and_paper$PG_value,
														        TRUE ~ PG_value + new_realized_and_paper$PG_value),
				PL_value = dplyr::case_when(is.na(PL_value) ~ new_realized_and_paper$PL_value,
														        TRUE ~ PL_value + new_realized_and_paper$PL_value)
			)
		# dplyr::mutate(
		#   RG_value = dplyr::case_when(is.na(RG_value) ~ new_realized_and_paper$RG_value,
		#                               TRUE ~ ewise_mean(RG_value, new_realized_and_paper$RG_value, zero.rm = TRUE)),
		#   RL_value = dplyr::case_when(is.na(RL_value) ~ new_realized_and_paper$RL_value,
		#                               TRUE ~ ewise_mean(RL_value, new_realized_and_paper$RL_value, zero.rm = TRUE)),
		#   PG_value = dplyr::case_when(is.na(PG_value) ~ new_realized_and_paper$PG_value,
		#                               TRUE ~ ewise_mean(PG_value, new_realized_and_paper$PG_value, zero.rm = TRUE)),
		#   PL_value = dplyr::case_when(is.na(PL_value) ~ new_realized_and_paper$PL_value,
		#                               TRUE ~ ewise_mean(PL_value, new_realized_and_paper$PL_value, zero.rm = TRUE))
		#   )

	} else if (method == "duration") {

		realized_and_paper[realized_and_paper$asset %in% assets,] <-
			realized_and_paper[realized_and_paper$asset %in% assets,] %>%
			dplyr::mutate(
				RG_duration = dplyr::case_when(is.na(RG_duration) ~ new_realized_and_paper$RG_duration,
																       TRUE ~ RG_duration + new_realized_and_paper$RG_duration),
				RL_duration = dplyr::case_when(is.na(RL_duration) ~ new_realized_and_paper$RL_duration,
																       TRUE ~ RL_duration + new_realized_and_paper$RL_duration),
				PG_duration = dplyr::case_when(is.na(PG_duration) ~ new_realized_and_paper$PG_duration,
																       TRUE ~ PG_duration + new_realized_and_paper$PG_duration),
				PL_duration = dplyr::case_when(is.na(PL_duration) ~ new_realized_and_paper$PL_duration,
																       TRUE ~ PL_duration + new_realized_and_paper$PL_duration)
			)

	} else { # method == "all"

		realized_and_paper[realized_and_paper$asset %in% assets,] <-
			realized_and_paper[realized_and_paper$asset %in% assets,] %>%
			dplyr::mutate(
				RG_count = dplyr::case_when(is.na(RG_count) ~ new_realized_and_paper$RG_count,
													        	 TRUE ~ RG_count + new_realized_and_paper$RG_count),
				RL_count = dplyr::case_when(is.na(RL_count) ~ new_realized_and_paper$RL_count,
														        TRUE ~ RL_count + new_realized_and_paper$RL_count),
				PG_count = dplyr::case_when(is.na(PG_count) ~ new_realized_and_paper$PG_count,
														        TRUE ~ PG_count + new_realized_and_paper$PG_count),
				PL_count = dplyr::case_when(is.na(PL_count) ~ new_realized_and_paper$PL_count,
														        TRUE ~ PL_count + new_realized_and_paper$PL_count),

				RG_total = dplyr::case_when(is.na(RG_total) ~ new_realized_and_paper$RG_total,
														        TRUE ~ RG_total + new_realized_and_paper$RG_total),
				RL_total = dplyr::case_when(is.na(RL_total) ~ new_realized_and_paper$RL_total,
														        TRUE ~ RL_total + new_realized_and_paper$RL_total),
				PG_total = dplyr::case_when(is.na(PG_total) ~ new_realized_and_paper$PG_total,
													          TRUE ~ PG_total + new_realized_and_paper$PG_total),
				PL_total = dplyr::case_when(is.na(PL_total) ~ new_realized_and_paper$PL_total,
													          TRUE ~ PL_total + new_realized_and_paper$PL_total),

				RG_value = dplyr::case_when(is.na(RG_value) ~ new_realized_and_paper$RG_value,
														        TRUE ~ RG_value + new_realized_and_paper$RG_value),
				RL_value = dplyr::case_when(is.na(RL_value) ~ new_realized_and_paper$RL_value,
														        TRUE ~ RL_value + new_realized_and_paper$RL_value),
				PG_value = dplyr::case_when(is.na(PG_value) ~ new_realized_and_paper$PG_value,
														        TRUE ~ PG_value + new_realized_and_paper$PG_value),
				PL_value = dplyr::case_when(is.na(PL_value) ~ new_realized_and_paper$PL_value,
														        TRUE ~ PL_value + new_realized_and_paper$PL_value),

				RG_duration = dplyr::case_when(is.na(RG_duration) ~ new_realized_and_paper$RG_duration,
															         TRUE ~ RG_duration + new_realized_and_paper$RG_duration),
				RL_duration = dplyr::case_when(is.na(RL_duration) ~ new_realized_and_paper$RL_duration,
															         TRUE ~ RL_duration + new_realized_and_paper$RL_duration),
				PG_duration = dplyr::case_when(is.na(PG_duration) ~ new_realized_and_paper$PG_duration,
																       TRUE ~ PG_duration + new_realized_and_paper$PG_duration),
				PL_duration = dplyr::case_when(is.na(PL_duration) ~ new_realized_and_paper$PL_duration,
																       TRUE ~ PL_duration + new_realized_and_paper$PL_duration)
			)

	}

	return(realized_and_paper)

}


#' @describeIn updates Update the realized and paper gains and losses
#'   results averaging the total value by the number of transactions
#'   for each asset.
#' @export
#  new name => update_expectedvalue +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
meanvalue_compute <- function(realized_and_paper,
															asset_transactions) {

	realized_and_paper <- dplyr::arrange(realized_and_paper, "asset")
	asset_transactions <- dplyr::arrange(asset_transactions, "asset")

	weights <- asset_transactions[["ntrx"]]

	realized_and_paper[, "RG_value"] <- realized_and_paper[, "RG_value"] / weights
	realized_and_paper[, "RL_value"] <- realized_and_paper[, "RL_value"] / weights
	realized_and_paper[, "PG_value"] <- realized_and_paper[, "PG_value"] / weights
	realized_and_paper[, "PL_value"] <- realized_and_paper[, "PL_value"] / weights

	return(realized_and_paper)

}

