#' @name updates
#'
#' @title Update investor's portfolio or realized and paper results
#'
#' @description Unpdate investor's portfolio or realized and paper results
#'
#' @details
#'
#' @param ptf_prz Numeric value. The portfolio price of the traded asset.
#' @param ptf_qty Numeric value. The portfolio quantity of the traded asset.
#' @inheritParams realized_compute
#' @inheritParams paper_compute
#' @param df_to_update Data frame containing the realized and paper gains and
#'   losses results to be updated.
#' @param df_new_info Data frame containing the realized and paper gains and
#'   losses results related to the new transaction.
#' @param df_numtrx Data frame containing the number of transactions for
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
prz_update <- function(ptf_qty, ptf_prz, trx_qty, trx_prz, trx_type) {

	# short positions are allowed but qty have to be negative

	# cases:
	# 0) ptf_qty + trx_qty = 0         =>  new_prz = 0
	# 1) ptf_qty = 0                   =>  new_prz = trx_prz
	# 2) ptf_qty > 0 & trx_type = B    =>  new_prz = weighted mean of przs
	# 3) ptf_qty < 0 & trx_type = S    =>  new_prz = weighted mean of przs

	# 4) ptf_qty > 0 & trx_type = S
	#  4.1) if ptf_qty > abs(trx_qty)  =>  new_prz = ptf_prz
	#  4.2) if ptf_qty < abs(trx_qty)  =>  new_prz = trx_prz

	# 5) ptf_qty < 0 & trx_type = B
	#  5.1) if abs(ptf_qty) > trx_qty  =>  new_prz = ptf_prz
	#  5.2) if abs(ptf_qty) < trx_qty  =>  new_prz = trx_prz

	if ((ptf_qty + trx_qty) == 0) {

		new_prz <- 0

	} else if (ptf_qty == 0) {

		new_prz <- trx_prz

	} else if (ptf_qty > 0 && trx_type == "B") {

		new_prz <- (ptf_prz * ptf_qty + trx_prz * abs(trx_qty)) / (ptf_qty + abs(trx_qty))

	} else if (ptf_qty < 0 && trx_type == "S") {

		new_prz <- (ptf_prz * abs(ptf_qty) + trx_prz * abs(trx_qty)) / (abs(ptf_qty) + abs(trx_qty))

	} else if (ptf_qty > 0 && trx_type == "S") {

		if (ptf_qty > abs(trx_qty)) {
			new_prz <- ptf_prz
		} else {
			new_prz <- trx_prz
		}

	} else if (ptf_qty < 0 && trx_type == "B") {

		if (abs(ptf_qty) > trx_qty) {
			new_prz <- ptf_prz
		} else {
			new_prz <- trx_prz
		}

	}

	return(new_prz)

}


#' @describeIn updates Update the portfolio datetime of the traded asset.
#' @export
dtt_update <- function(ptf_qty, ptf_dtt, trx_qty, trx_dtt, trx_type) {

	# short positions are allowed but qty have to be negative

	# cases:
	# 0) ptf_qty + trx_qty = 0         =>  new_dtt = trx_dtt
	# 1) ptf_qty = 0                   =>  new_dtt = trx_dtt
	# 2) ptf_qty > 0 & trx_type = B    =>  new_dtt = ptf_dtt
	# 3) ptf_qty < 0 & trx_type = S    =>  new_dtt = ptf_dtt

	# 4) ptf_qty > 0 & trx_type = S
	#  4.1) if ptf_qty > abs(trx_qty)  =>  new_dtt = ptf_dtt
	#  4.2) if ptf_qty < abs(trx_qty)  =>  new_dtt = trx_dtt

	# 5) ptf_qty < 0 & trx_type = B
	#  5.1) if abs(ptf_qty) > trx_qty  =>  new_dtt = ptf_dtt
	#  5.2) if abs(ptf_qty) < trx_qty  =>  new_dtt = trx_dtt

	diff_qty <- ptf_qty + trx_qty


	if (diff_qty == 0) {

		new_dtt <- trx_dtt

	} else if (ptf_qty == 0) {

		new_dtt <- trx_dtt

	} else if (ptf_qty > 0 && trx_type == "B") {

		new_dtt <- ptf_dtt

	} else if (ptf_qty < 0 && trx_type == "S") {

		new_dtt <- ptf_dtt

	} else if (ptf_qty > 0 && trx_type == "S") {

		if (diff_qty > 0) {
			new_dtt <- ptf_dtt
		} else {
			new_dtt <- trx_dtt
		}

	} else if (ptf_qty < 0 && trx_type == "B") {

		if (diff_qty < 0) {
			new_dtt <- ptf_dtt
		} else {
			new_dtt <- trx_dtt
		}

	}

	return(new_dtt)

}


# add functions update_quantity and update_portfolio ++++++++++++++++++++++++++++++++++++++++++++++


#' @describeIn updates Update the realized and paper gains and losses
#'   results with the results obtained on the last occurred transaction.
#' @export
results_update <- function(df_to_update, df_new_info, method = "all") {

	df_to_update <- arrange(df_to_update, asset)
	df_new_info <- arrange(df_new_info, asset)
	assets <- df_new_info$asset

	# replace the df_to_update values corresponding to the assets present into df_new_info
	# if values are NA, then they are simply replaced with the new values of df_new_info
	# count columns are updated as the element-wise sum
	# total columns are updated as the element-wise sum
	# value columns are updated as the element-wise mean
	# (if values are 0 in the value columns, then they are simply replaced with the newest)

	if (method == "count") {

		df_to_update[df_to_update$asset %in% assets,] <-
			df_to_update[df_to_update$asset %in% assets,] %>%
			mutate(
				RG_count = case_when(is.na(RG_count) ~ df_new_info$RG_count,
														 TRUE ~ RG_count + df_new_info$RG_count),
				RL_count = case_when(is.na(RL_count) ~ df_new_info$RL_count,
														 TRUE ~ RL_count + df_new_info$RL_count),
				PG_count = case_when(is.na(PG_count) ~ df_new_info$PG_count,
														 TRUE ~ PG_count + df_new_info$PG_count),
				PL_count = case_when(is.na(PL_count) ~ df_new_info$PL_count,
														 TRUE ~ PL_count + df_new_info$PL_count)
			)

	} else if (method == "total") {

		df_to_update[df_to_update$asset %in% assets,] <-
			df_to_update[df_to_update$asset %in% assets,] %>%
			mutate(
				RG_total = case_when(is.na(RG_total) ~ df_new_info$RG_total,
														 TRUE ~ RG_total + df_new_info$RG_total),
				RL_total = case_when(is.na(RL_total) ~ df_new_info$RL_total,
														 TRUE ~ RL_total + df_new_info$RL_total),
				PG_total = case_when(is.na(PG_total) ~ df_new_info$PG_total,
														 TRUE ~ PG_total + df_new_info$PG_total),
				PL_total = case_when(is.na(PL_total) ~ df_new_info$PL_total,
														 TRUE ~ PL_total + df_new_info$PL_total)
			)

	} else if (method == "value") {

		df_to_update[df_to_update$asset %in% assets,] <-
			df_to_update[df_to_update$asset %in% assets,] %>%
			mutate(
				RG_value = case_when(is.na(RG_value) ~ df_new_info$RG_value,
														 TRUE ~ RG_value + df_new_info$RG_value),
				RL_value = case_when(is.na(RL_value) ~ df_new_info$RL_value,
														 TRUE ~ RL_value + df_new_info$RL_value),
				PG_value = case_when(is.na(PG_value) ~ df_new_info$PG_value,
														 TRUE ~ PG_value + df_new_info$PG_value),
				PL_value = case_when(is.na(PL_value) ~ df_new_info$PL_value,
														 TRUE ~ PL_value + df_new_info$PL_value)
			)
		# mutate(
		#   RG_value = case_when(is.na(RG_value) ~ df_new_info$RG_value,
		#                        TRUE ~ ewise_mean(RG_value, df_new_info$RG_value, zero.rm = TRUE)),
		#   RL_value = case_when(is.na(RL_value) ~ df_new_info$RL_value,
		#                        TRUE ~ ewise_mean(RL_value, df_new_info$RL_value, zero.rm = TRUE)),
		#   PG_value = case_when(is.na(PG_value) ~ df_new_info$PG_value,
		#                        TRUE ~ ewise_mean(PG_value, df_new_info$PG_value, zero.rm = TRUE)),
		#   PL_value = case_when(is.na(PL_value) ~ df_new_info$PL_value,
		#                        TRUE ~ ewise_mean(PL_value, df_new_info$PL_value, zero.rm = TRUE))
		#   )

	} else if (method == "duration") {

		df_to_update[df_to_update$asset %in% assets,] <-
			df_to_update[df_to_update$asset %in% assets,] %>%
			mutate(
				RG_duration = case_when(is.na(RG_duration) ~ df_new_info$RG_duration,
																TRUE ~ RG_duration + df_new_info$RG_duration),
				RL_duration = case_when(is.na(RL_duration) ~ df_new_info$RL_duration,
																TRUE ~ RL_duration + df_new_info$RL_duration),
				PG_duration = case_when(is.na(PG_duration) ~ df_new_info$PG_duration,
																TRUE ~ PG_duration + df_new_info$PG_duration),
				PL_duration = case_when(is.na(PL_duration) ~ df_new_info$PL_duration,
																TRUE ~ PL_duration + df_new_info$PL_duration)
			)

	} else { # method == "all"

		df_to_update[df_to_update$asset %in% assets,] <-
			df_to_update[df_to_update$asset %in% assets,] %>%
			mutate(
				RG_count = case_when(is.na(RG_count) ~ df_new_info$RG_count,
														 TRUE ~ RG_count + df_new_info$RG_count),
				RL_count = case_when(is.na(RL_count) ~ df_new_info$RL_count,
														 TRUE ~ RL_count + df_new_info$RL_count),
				PG_count = case_when(is.na(PG_count) ~ df_new_info$PG_count,
														 TRUE ~ PG_count + df_new_info$PG_count),
				PL_count = case_when(is.na(PL_count) ~ df_new_info$PL_count,
														 TRUE ~ PL_count + df_new_info$PL_count),

				RG_total = case_when(is.na(RG_total) ~ df_new_info$RG_total,
														 TRUE ~ RG_total + df_new_info$RG_total),
				RL_total = case_when(is.na(RL_total) ~ df_new_info$RL_total,
														 TRUE ~ RL_total + df_new_info$RL_total),
				PG_total = case_when(is.na(PG_total) ~ df_new_info$PG_total,
														 TRUE ~ PG_total + df_new_info$PG_total),
				PL_total = case_when(is.na(PL_total) ~ df_new_info$PL_total,
														 TRUE ~ PL_total + df_new_info$PL_total),

				RG_value = case_when(is.na(RG_value) ~ df_new_info$RG_value,
														 TRUE ~ RG_value + df_new_info$RG_value),
				RL_value = case_when(is.na(RL_value) ~ df_new_info$RL_value,
														 TRUE ~ RL_value + df_new_info$RL_value),
				PG_value = case_when(is.na(PG_value) ~ df_new_info$PG_value,
														 TRUE ~ PG_value + df_new_info$PG_value),
				PL_value = case_when(is.na(PL_value) ~ df_new_info$PL_value,
														 TRUE ~ PL_value + df_new_info$PL_value),

				RG_duration = case_when(is.na(RG_duration) ~ df_new_info$RG_duration,
																TRUE ~ RG_duration + df_new_info$RG_duration),
				RL_duration = case_when(is.na(RL_duration) ~ df_new_info$RL_duration,
																TRUE ~ RL_duration + df_new_info$RL_duration),
				PG_duration = case_when(is.na(PG_duration) ~ df_new_info$PG_duration,
																TRUE ~ PG_duration + df_new_info$PG_duration),
				PL_duration = case_when(is.na(PL_duration) ~ df_new_info$PL_duration,
																TRUE ~ PL_duration + df_new_info$PL_duration)
			)

	}

	return(df_to_update)

}


#' @describeIn updates Update the realized and paper gains and losses
#'   results averaging the total value by the number of transactions
#'   for each asset.
#' @export
#  new name => update_expectedvalue +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
meanvalue_compute <- function(df_to_update, df_numtrx) {

	df_to_update <- arrange(df_to_update, "asset")
	df_numtrx <- arrange(df_numtrx, "asset")

	weights <- df_numtrx[["ntrx"]]

	df_to_update[, "RG_value"] <- df_to_update[, "RG_value"] / weights
	df_to_update[, "RL_value"] <- df_to_update[, "RL_value"] / weights
	df_to_update[, "PG_value"] <- df_to_update[, "PG_value"] / weights
	df_to_update[, "PL_value"] <- df_to_update[, "PL_value"] / weights

	return(df_to_update)

}

