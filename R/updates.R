#' @name updates
#'
#' @title Update investor's portfolio or realized and paper results
#'
#' @description Unpdate investor's portfolio or realized and paper results
#'
#' @param portfolio_price Numeric value. The portfolio price of the traded asset.
#' @param portfolio_quantity Numeric value. The portfolio quantity of the traded asset.
#' @param portfolio_datetime POSIXct value. The date-time of the traded asset.
#' @inheritParams realized_compute
#' @inheritParams paper_compute
#' @inheritParams gains_and_losses
#' @param realized_and_paper Data frame containing the realized and paper gains and
#'   losses results to be updated.
#' @param new_realized_and_paper Data frame containing the realized and paper gains and
#'   losses results related to the new transaction.
#' @param num_transaction_assets Data frame containing the number of transactions for
#'   each asset traded by the investor.
#'
#'
#' @return
#'   The described functions have different return behaviours.
#'
#'   \describe{
#'     \item{\code{update_price}}{returns a numeric value of the new portfolio
#'       price of the traded asset.}
#'     \item{\code{update_datetime}}{returns a datetime value of the new portfolio
#'       datetime of the traded asset.}
#'     \item{\code{update_realized_and_paper}}{returns a [tibble][tibble::tibble-package]
#'       containing the realized and paper gains and losses with the updated
#'       values.}
#'     \item{\code{update_expectedvalue}}{returns a [tibble][tibble::tibble-package]
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
#' @seealso \code{\link{portfolio_compute}}, \code{\link{gains_and_losses}}
NULL


#' @describeIn updates Update the portfolio price of the traded asset.
#' @export
update_price <- function(portfolio_quantity,
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
update_datetime <- function(portfolio_quantity,
											      portfolio_datetime,
											      transaction_quantity,
											      transaction_datetime,
											      transaction_type) {

	# short positions are allowed but qty have to be negative

	# cases:
	# 0) portfolio_quantity + transaction_quantity = 0    =>  new_dtt = transaction_datetime
	# 1) portfolio_quantity = 0                           =>  new_dtt = transaction_datetime
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


#' @describeIn updates Update the portfolio quantity of the traded asset.
#' @export
update_quantity <- function(portfolio_quantity,
											      transaction_quantity) {

	# the portfolio quantity of the traded asset has to be updated just
	# summing up the transaction quantity
	# if the transaction type is a sell, than the transaction
	# quantity is negative
	new_qty <- portfolio_quantity + transaction_quantity
	return(new_qty)

}


#' @describeIn updates Update the portfolio with the new quantity, price
#'   and datetime of the traded asset.
#' @export
update_portfolio <- function(portfolio,
														 transaction_asset,
														 transaction_quantity,
														 transaction_price,
														 transaction_datetime,
														 transaction_type) {

	# qty, prz and dtt of transaction asset already into portfolio
	ptf_qty <- portfolio[portfolio$asset == transaction_asset, ]$quantity
	ptf_prz <- portfolio[portfolio$asset == transaction_asset, ]$price
	ptf_dtt <- portfolio[portfolio$asset == transaction_asset, ]$datetime

	if (is.na(ptf_qty)) {
		# if qty is NA (initial condition), simply update the portfolio
		# with the values of qty, prz and dtt of the transaction
		portfolio[portfolio$asset == transaction_asset, ][["quantity"]] <- transaction_quantity
		portfolio[portfolio$asset == transaction_asset, ][["price"]] <- transaction_price
		portfolio[portfolio$asset == transaction_asset, ][["datetime"]] <- transaction_datetime
	} else {
		# else sum the qtys
		portfolio[portfolio$asset == transaction_asset, ][["quantity"]] <-
			update_quantity(ptf_qty, transaction_quantity)
		# and adjust the przs based on conditions
		portfolio[portfolio$asset == transaction_asset, ][["price"]] <-
			update_price(ptf_qty, ptf_prz, transaction_quantity, transaction_price, transaction_type)
		portfolio[portfolio$asset == transaction_asset, ][["datetime"]] <-
			update_datetime(ptf_qty, ptf_dtt, transaction_quantity, transaction_datetime, transaction_type)
	}

	return(portfolio)

}


#' @describeIn updates Update the realized and paper gains and losses
#'   results with the results obtained on the last occurred transaction.
#' @export
update_realized_and_paper <- function(realized_and_paper,
													            new_realized_and_paper,
													            method = "all") {

	assets <- new_realized_and_paper$asset
	realized_and_paper_filtered <- realized_and_paper[realized_and_paper$asset %in% assets,]
	realized_and_paper_filtered <- realized_and_paper_filtered[order(
		factor(realized_and_paper_filtered$asset, levels = assets),
		method = "radix"),]

	# replace the realized_and_paper values corresponding to the assets present into new_realized_and_paper
	# if values are NA, then they are simply replaced with the new values of new_realized_and_paper
	# count columns are updated as the element-wise sum
	# total columns are updated as the element-wise sum
	# value columns are updated as the element-wise mean
	# (if values are 0 in the value columns, then they are simply replaced with the newest)

	if (method == "count") {

		realized_and_paper_filtered[["RG_count"]] <-
			ifelse(is.na(realized_and_paper_filtered[["RG_count"]]), new_realized_and_paper$RG_count,
									 realized_and_paper_filtered[["RG_count"]] + new_realized_and_paper$RG_count)
		realized_and_paper_filtered[["RL_count"]] <-
			ifelse(is.na(realized_and_paper_filtered[["RL_count"]]), new_realized_and_paper$RL_count,
						 realized_and_paper_filtered[["RL_count"]] + new_realized_and_paper$RL_count)
		realized_and_paper_filtered[["PG_count"]] <-
			ifelse(is.na(realized_and_paper_filtered[["PG_count"]]), new_realized_and_paper$PG_count,
						 realized_and_paper_filtered[["PG_count"]] + new_realized_and_paper$PG_count)
		realized_and_paper_filtered[["PL_count"]] <-
			ifelse(is.na(realized_and_paper_filtered[["PL_count"]]), new_realized_and_paper$PL_count,
						 realized_and_paper_filtered[["PL_count"]] + new_realized_and_paper$PL_count)

	} else if (method == "total") {

		realized_and_paper_filtered[["RG_total"]] <-
			ifelse(is.na(realized_and_paper_filtered[["RG_total"]]), new_realized_and_paper$RG_total,
						 realized_and_paper_filtered[["RG_total"]] + new_realized_and_paper$RG_total)
		realized_and_paper_filtered[["RL_total"]] <-
			ifelse(is.na(realized_and_paper_filtered[["RL_total"]]), new_realized_and_paper$RL_total,
						 realized_and_paper_filtered[["RL_total"]] + new_realized_and_paper$RL_total)
		realized_and_paper_filtered[["PG_total"]] <-
			ifelse(is.na(realized_and_paper_filtered[["PG_total"]]), new_realized_and_paper$PG_total,
						 realized_and_paper_filtered[["PG_total"]] + new_realized_and_paper$PG_total)
		realized_and_paper_filtered[["PL_total"]] <-
			ifelse(is.na(realized_and_paper_filtered[["PL_total"]]), new_realized_and_paper$PL_total,
						 realized_and_paper_filtered[["PL_total"]] + new_realized_and_paper$PL_total)

	} else if (method == "value") {

		realized_and_paper_filtered[["RG_value"]] <-
			ifelse(is.na(realized_and_paper_filtered[["RG_value"]]), new_realized_and_paper$RG_value,
						 realized_and_paper_filtered[["RG_value"]] + new_realized_and_paper$RG_value)
		realized_and_paper_filtered[["RL_value"]] <-
			ifelse(is.na(realized_and_paper_filtered[["RL_value"]]), new_realized_and_paper$RL_value,
						 realized_and_paper_filtered[["RL_value"]] + new_realized_and_paper$RL_value)
		realized_and_paper_filtered[["PG_value"]] <-
			ifelse(is.na(realized_and_paper_filtered[["PG_value"]]), new_realized_and_paper$PG_value,
						 realized_and_paper_filtered[["PG_value"]] + new_realized_and_paper$PG_value)
		realized_and_paper_filtered[["PL_value"]] <-
			ifelse(is.na(realized_and_paper_filtered[["PL_value"]]), new_realized_and_paper$PL_value,
						 realized_and_paper_filtered[["PL_value"]] + new_realized_and_paper$PL_value)
		# dplyr::mutate(realized_and_paper
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

		realized_and_paper_filtered[["RG_duration"]] <-
			ifelse(is.na(realized_and_paper_filtered[["RG_duration"]]), new_realized_and_paper$RG_duration,
						 realized_and_paper_filtered[["RG_duration"]] + new_realized_and_paper$RG_duration)
		realized_and_paper_filtered[["RL_duration"]] <-
			ifelse(is.na(realized_and_paper_filtered[["RL_duration"]]), new_realized_and_paper$RL_duration,
						 realized_and_paper_filtered[["RL_duration"]] + new_realized_and_paper$RL_duration)
		realized_and_paper_filtered[["PG_duration"]] <-
			ifelse(is.na(realized_and_paper_filtered[["PG_duration"]]), new_realized_and_paper$PG_duration,
						 realized_and_paper_filtered[["PG_duration"]] + new_realized_and_paper$PG_duration)
		realized_and_paper_filtered[["PL_duration"]] <-
			ifelse(is.na(realized_and_paper_filtered[["PL_duration"]]), new_realized_and_paper$PL_duration,
						 realized_and_paper_filtered[["PL_duration"]] + new_realized_and_paper$PL_duration)

	} else {# method == "all"

		realized_and_paper_filtered[["RG_count"]] <-
			ifelse(is.na(realized_and_paper_filtered[["RG_count"]]), new_realized_and_paper$RG_count,
						 realized_and_paper_filtered[["RG_count"]] + new_realized_and_paper$RG_count)
		realized_and_paper_filtered[["RL_count"]] <-
			ifelse(is.na(realized_and_paper_filtered[["RL_count"]]), new_realized_and_paper$RL_count,
						 realized_and_paper_filtered[["RL_count"]] + new_realized_and_paper$RL_count)
		realized_and_paper_filtered[["PG_count"]] <-
			ifelse(is.na(realized_and_paper_filtered[["PG_count"]]), new_realized_and_paper$PG_count,
						 realized_and_paper_filtered[["PG_count"]] + new_realized_and_paper$PG_count)
		realized_and_paper_filtered[["PL_count"]] <-
			ifelse(is.na(realized_and_paper_filtered[["PL_count"]]), new_realized_and_paper$PL_count,
						 realized_and_paper_filtered[["PL_count"]] + new_realized_and_paper$PL_count)

		realized_and_paper_filtered[["RG_total"]] <-
			ifelse(is.na(realized_and_paper_filtered[["RG_total"]]), new_realized_and_paper$RG_total,
						 realized_and_paper_filtered[["RG_total"]] + new_realized_and_paper$RG_total)
		realized_and_paper_filtered[["RL_total"]] <-
			ifelse(is.na(realized_and_paper_filtered[["RL_total"]]), new_realized_and_paper$RL_total,
						 realized_and_paper_filtered[["RL_total"]] + new_realized_and_paper$RL_total)
		realized_and_paper_filtered[["PG_total"]] <-
			ifelse(is.na(realized_and_paper_filtered[["PG_total"]]), new_realized_and_paper$PG_total,
						 realized_and_paper_filtered[["PG_total"]] + new_realized_and_paper$PG_total)
		realized_and_paper_filtered[["PL_total"]] <-
			ifelse(is.na(realized_and_paper_filtered[["PL_total"]]), new_realized_and_paper$PL_total,
						 realized_and_paper_filtered[["PL_total"]] + new_realized_and_paper$PL_total)

		realized_and_paper_filtered[["RG_value"]] <-
			ifelse(is.na(realized_and_paper_filtered[["RG_value"]]), new_realized_and_paper$RG_value,
						 realized_and_paper_filtered[["RG_value"]] + new_realized_and_paper$RG_value)
		realized_and_paper_filtered[["RL_value"]] <-
			ifelse(is.na(realized_and_paper_filtered[["RL_value"]]), new_realized_and_paper$RL_value,
						 realized_and_paper_filtered[["RL_value"]] + new_realized_and_paper$RL_value)
		realized_and_paper_filtered[["PG_value"]] <-
			ifelse(is.na(realized_and_paper_filtered[["PG_value"]]), new_realized_and_paper$PG_value,
						 realized_and_paper_filtered[["PG_value"]] + new_realized_and_paper$PG_value)
		realized_and_paper_filtered[["PL_value"]] <-
			ifelse(is.na(realized_and_paper_filtered[["PL_value"]]), new_realized_and_paper$PL_value,
						 realized_and_paper_filtered[["PL_value"]] + new_realized_and_paper$PL_value)

		realized_and_paper_filtered[["RG_duration"]] <-
			ifelse(is.na(realized_and_paper_filtered[["RG_duration"]]), new_realized_and_paper$RG_duration,
						 realized_and_paper_filtered[["RG_duration"]] + new_realized_and_paper$RG_duration)
		realized_and_paper_filtered[["RL_duration"]] <-
			ifelse(is.na(realized_and_paper_filtered[["RL_duration"]]), new_realized_and_paper$RL_duration,
						 realized_and_paper_filtered[["RL_duration"]] + new_realized_and_paper$RL_duration)
		realized_and_paper_filtered[["PG_duration"]] <-
			ifelse(is.na(realized_and_paper_filtered[["PG_duration"]]), new_realized_and_paper$PG_duration,
						 realized_and_paper_filtered[["PG_duration"]] + new_realized_and_paper$PG_duration)
		realized_and_paper_filtered[["PL_duration"]] <-
			ifelse(is.na(realized_and_paper_filtered[["PL_duration"]]), new_realized_and_paper$PL_duration,
						 realized_and_paper_filtered[["PL_duration"]] + new_realized_and_paper$PL_duration)

	}

	realized_and_paper <- dplyr::rows_update(
		realized_and_paper,
		realized_and_paper_filtered,
		by = c("investor", "asset")
	)

	return(realized_and_paper)

}


#' @describeIn updates Update the realized and paper gains and losses
#'   results averaging the total value by the number of transactions
#'   for each asset.
#' @export
#  new name => update_expectedvalue +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
update_expectedvalue <- function(realized_and_paper,
															   num_transaction_assets) {

	weights <- num_transaction_assets[["numtrx"]]

	realized_and_paper[["RG_value"]] <- realized_and_paper[["RG_value"]] / weights
	realized_and_paper[["RL_value"]] <- realized_and_paper[["RL_value"]] / weights
	realized_and_paper[["PG_value"]] <- realized_and_paper[["PG_value"]] / weights
	realized_and_paper[["PL_value"]] <- realized_and_paper[["PL_value"]] / weights

	return(realized_and_paper)

}

