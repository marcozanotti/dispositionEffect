#' @name paper_compute
#'
#' @title Papers' estimation
#'
#' @description Papers' estimation
#'
#' @param portfolio_quantity Numeric vector. The portfolio quantities of assets into the
#'   investor's portfolio.
#' @param portfolio_price Numeric vector. The portfolio prices of assets into the
#'   investor's portfolio.
#' @param market_price Numeric vector. The market prices of assets into the
#'   investor's portfolio.
#' @param datetime_difference Numeric value of time difference between the previous_datetime
#'   and the transaction_datetime, computed through \code{\link{difftime_financial}}.
#'   If NULL, then previous_datetime and transaction_datetime must be specified.
#' @param previous_datetime POSIXct value. The date-time of the last transaction
#'   performed by the investor.
#' @param transaction_datetime POSIXct value. The date-time at which the transaction
#'   is going to occur.
#' @param assets Character vector. The name of assets into the investor's
#'   portfolio but the traded asset.
#' @param allow_short Logical. If TRUE short positions are allowed, otherwise only
#'   long positions are allowed.
#' @param method Character string. The method used to compute papers.
#'   Allowed values are "count", "total", "value", "duration" and "all".
#'
#' @return
#'   The described functions have different return behaviours.
#'
#'   \describe{
#'     \item{\code{paper_compute}}{returns a [tibble][tibble::tibble-package]
#'       containing the values of paper gains and paper losses computed by
#'       means of the chosen method on each portfolio assets.}
#'     \item{\code{paper_count}}{returns a named vector containing the values
#'       of paper gains and paper losses computed using the count method.}
#'     \item{\code{paper_total}}{returns a named vector containing the values
#'       of paper gains and paper losses computed using the total method.}
#'     \item{\code{paper_value}}{returns a named vector containing the values
#'       of paper gains and paper losses computed using the value method.}
#'     \item{\code{paper_duration}}{returns a named vector containing the values
#'       of paper gains and paper losses computed using the duration method.}
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
#' @seealso \code{\link{realized_compute}}, \code{\link{gains_and_losses}}
NULL


#' @describeIn paper_compute Computation of paper gains and paper losses as
#'   simple counts.
#' @export
paper_count <- function(portfolio_quantity,
												portfolio_price,
												market_price,
												allow_short = FALSE) {

	prz_diff <- market_price - portfolio_price # price difference

	if (allow_short) {
		if (portfolio_quantity > 0 && prz_diff > 0) { # Long - Paper Gain
			res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 1, "PL_count" = 0)
		} else if (portfolio_quantity > 0 && prz_diff < 0) { # Long - Paper Loss
			res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 1)
		} else if (portfolio_quantity < 0 && prz_diff > 0) { # Short - Paper Loss
			res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 1)
		} else if (portfolio_quantity < 0 && prz_diff < 0) { # Short - Paper Gain
			res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 1, "PL_count" = 0)
		} else {# if portfolio_quantity = 0 or prz_diff = 0
			res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
		}
	} else {
		if (portfolio_quantity > 0 && prz_diff > 0) { # Long - Paper Gain
			res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 1, "PL_count" = 0)
		} else if (portfolio_quantity > 0 && prz_diff < 0) { # Long - Paper Loss
			res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 1)
		} else {# if portfolio_quantity = 0 or prz_diff = 0
			res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
		}
	}

	return(res)

}


#' @describeIn paper_compute Computation of paper gains and paper losses as
#'   simple quantity of assets.
#' @export
paper_total <- function(portfolio_quantity,
												portfolio_price,
												market_price,
												allow_short = FALSE) {

	prz_diff <- market_price - portfolio_price # price difference

	if (allow_short) {
		if (portfolio_quantity > 0 && prz_diff > 0) { # Long - Paper Gain
			res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = portfolio_quantity, "PL_total" = 0)
		} else if (portfolio_quantity > 0 && prz_diff < 0) { # Long - Paper Loss
			res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = portfolio_quantity)
		} else if (portfolio_quantity < 0 && prz_diff > 0) { # Short - Paper Loss
			res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = abs(portfolio_quantity))
		} else if (portfolio_quantity < 0 && prz_diff < 0) { # Short - Paper Gain
			res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = abs(portfolio_quantity), "PL_total" = 0)
		} else {# if portfolio_quantity = 0 or prz_diff = 0
			res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
		}
	} else {
		if (portfolio_quantity > 0 && prz_diff > 0) { # Long - Paper Gain
			res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = portfolio_quantity, "PL_total" = 0)
		} else if (portfolio_quantity > 0 && prz_diff < 0) { # Long - Paper Loss
			res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = portfolio_quantity)
		} else {# if portfolio_quantity = 0 or prz_diff = 0 or portfolio_quantity < 0 (short)
			res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
		}
	}

	return(res)

}


#' @describeIn paper_compute Computation of paper gains and paper losses as
#'   simple expected return of assets.
#' @export
paper_value <- function(portfolio_quantity,
												portfolio_price,
												market_price,
												allow_short = FALSE) {

	prz_diff <- market_price - portfolio_price # price difference
	Er <- prz_diff / portfolio_price # asset expected return

	if (allow_short) {
		if (portfolio_quantity > 0 && prz_diff > 0) { # Long - Paper Gain
			res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = Er, "PL_value" = 0)
		} else if (portfolio_quantity > 0 && prz_diff < 0) { # Long - Paper Loss
			res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = Er)
		} else if (portfolio_quantity < 0 && prz_diff > 0) { # Short - Paper Loss
			res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = -Er)
		} else if (portfolio_quantity < 0 && prz_diff < 0) { # Short - Paper Gain
			res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = -Er, "PL_value" = 0)
		} else {# if portfolio_quantity = 0 or prz_diff = 0
			res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
		}
	} else {
		if (portfolio_quantity > 0 && prz_diff > 0) { # Long - Paper Gain
			res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = Er, "PL_value" = 0)
		} else if (portfolio_quantity > 0 && prz_diff < 0) { # Long - Paper Loss
			res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = Er)
		} else {# if portfolio_quantity = 0 or prz_diff = 0 or portfolio_quantity < 0 (short)
			res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
		}
	}

	return(res)

}


#' @describeIn paper_compute Computation of paper gains and paper losses as
#'   simple financial duration.
#' @export
paper_duration <- function(portfolio_quantity,
													 portfolio_price,
													 market_price,
													 datetime_difference = NULL,
													 previous_datetime = NULL,
													 transaction_datetime = NULL,
													 allow_short = FALSE) {

	prz_diff <- market_price - portfolio_price # price difference
	if (is.null(datetime_difference)) {
		dtt_diff <- difftime_financial(previous_datetime, transaction_datetime)
	} else {
		dtt_diff <- datetime_difference
	}


	if (allow_short) {
		if (portfolio_quantity > 0 && prz_diff > 0) { # Long - Paper Gain
			res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = dtt_diff, "PL_duration" = 0)
		} else if (portfolio_quantity > 0 && prz_diff < 0) { # Long - Paper Loss
			res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = dtt_diff)
		} else if (portfolio_quantity < 0 && prz_diff > 0) { # Short - Paper Loss
			res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = dtt_diff)
		} else if (portfolio_quantity < 0 && prz_diff < 0) { # Short - Paper Gain
			res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = dtt_diff, "PL_duration" = 0)
		} else {# if portfolio_quantity = 0 or prz_diff = 0
			res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
		}
	} else {
		if (portfolio_quantity > 0 && prz_diff > 0) { # Long - Paper Gain
			res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = dtt_diff, "PL_duration" = 0)
		} else if (portfolio_quantity > 0 && prz_diff < 0) { # Long - Paper Loss
			res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = dtt_diff)
		} else {# if portfolio_quantity = 0 or prz_diff = 0
			res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
		}
	}

	return(res)

}


#' @describeIn paper_compute Wrapper that calls other paper_. functions to
#'   compute paper gains and paper losses based on the chosen method.
#' @export
paper_compute <- function(portfolio_quantity,
													portfolio_price,
													market_price,
													previous_datetime,
													transaction_datetime,
													assets,
													allow_short = FALSE,
													method = "all") {

	if (method == "count") {

		pgl_count <- purrr::pmap(
			list(portfolio_quantity, portfolio_price, market_price),
			paper_count,
			allow_short
		)
		res_df <- as.data.frame(do.call(rbind, pgl_count))
		res_df[["asset"]] <- assets
		res_df <- res_df[c(ncol(res_df), 1:(ncol(res_df) - 1))]

	} else if (method == "total") {

		pgl_total <- purrr::pmap(
			list(portfolio_quantity, portfolio_price, market_price),
			paper_total,
			allow_short
		)
		res_df <- as.data.frame(do.call(rbind, pgl_total))
		res_df[["asset"]] <- assets
		res_df <- res_df[c(ncol(res_df), 1:(ncol(res_df) - 1))]

	} else if (method == "value") {

		pgl_value <- purrr::pmap(
			list(portfolio_quantity, portfolio_price, market_price),
			paper_value,
			allow_short
		)
		res_df <- as.data.frame(do.call(rbind, pgl_value))
		res_df[["asset"]] <- assets
		res_df <- res_df[c(ncol(res_df), 1:(ncol(res_df) - 1))]

	} else if (method == "duration") {

		dtt_diff <- difftime_financial(previous_datetime, transaction_datetime)
		pgl_duration <- purrr::pmap(
			list(portfolio_quantity, portfolio_price, market_price),
			paper_duration,
			datetime_difference = dtt_diff, allow_short = allow_short
		)
		res_df <- as.data.frame(do.call(rbind, pgl_duration))
		res_df[["asset"]] <- assets
		res_df <- res_df[c(ncol(res_df), 1:(ncol(res_df) - 1))]

	} else {# method == "all"

		pgl_count <- purrr::pmap(
			list(portfolio_quantity, portfolio_price, market_price),
			paper_count,
			allow_short
		)
		pgl_total <- purrr::pmap(
			list(portfolio_quantity, portfolio_price, market_price),
			paper_total,
			allow_short
		)
		pgl_value <- purrr::pmap(
			list(portfolio_quantity, portfolio_price, market_price),
			paper_value,
			allow_short
		)
		dtt_diff <- difftime_financial(previous_datetime, transaction_datetime)
		pgl_duration <- purrr::pmap(
			list(portfolio_quantity, portfolio_price, market_price),
			paper_duration,
			datetime_difference = dtt_diff, allow_short = allow_short
		)
		res_df <- dplyr::bind_cols(
			as.data.frame(do.call(rbind, pgl_count)),
			as.data.frame(do.call(rbind, pgl_total)),
			as.data.frame(do.call(rbind, pgl_value)),
			as.data.frame(do.call(rbind, pgl_duration)),
		)
		res_df[["asset"]] <- assets
		res_df <- res_df[c(ncol(res_df), 1:(ncol(res_df) - 1))]

	}

	return(res_df)

}

