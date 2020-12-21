#' @name paper_compute
#'
#' @title Papers' estimation
#'
#' @description Papers' estimation
#'
#' @details
#'
#' @param portfolio_quantity Numeric vector. The portfolio quantities of assets into the
#'   investor's portfolio.
#' @param portfolio_price Numeric vector. The portfolio prices of assets into the
#'   investor's portfolio.
#' @param market_price Numeric vector. The market prices of assets into the
#'   investor's portfolio.
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

	if (!is.numeric(portfolio_quantity) ||
			!is.numeric(portfolio_price) ||
			!is.numeric(market_price)) {
		stop("Arguments must be numeric.", call. = FALSE)
	}

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

	if (!is.numeric(portfolio_quantity) ||
			!is.numeric(portfolio_price) ||
			!is.numeric(market_price)) {
		stop("Arguments must be numeric.", call. = FALSE)
	}

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

	if (!is.numeric(portfolio_quantity) ||
			!is.numeric(portfolio_price) ||
			!is.numeric(market_price)) {
		stop("Arguments must be numeric.", call. = FALSE)
	}

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
													 previous_datetime,
													 transaction_datetime,
													 allow_short = FALSE) {

	if (!is.numeric(portfolio_quantity) ||
			!is.numeric(portfolio_price) ||
			!is.numeric(market_price)) {
		stop("Arguments *qty and *prz must be numeric.", call. = FALSE)
	}

	if (!lubridate::is.POSIXct(previous_datetime) ||
			!lubridate::is.POSIXct(transaction_datetime)) {
		stop("Arguments *dtt must be POSIXct.", call. = FALSE)
	}

	prz_diff <- market_price - portfolio_price # price difference
	# dtt_diff <- difftime(transaction_datetime, previous_datetime, units = "days") %>% # duration
	#   as.numeric() # to avoid conversion errors
	dtt_diff <- financial_difftime(previous_datetime, transaction_datetime)
	# by default the duration returned is in days, conversion is left to the user

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

		# compute Paper Gain and Paper Loss with paper_count()
		pgl_count <- purrr::pmap(list(portfolio_quantity, portfolio_price, market_price), paper_count, allow_short)
		names(pgl_count) <- assets
		# convert results to a df
		res_df <- do.call(rbind, pgl_count) %>%
			as.data.frame() %>%
			tibble::rownames_to_column(var = "asset")

	} else if (method == "total") {

		# compute Paper Gain and Paper Loss with paper_total()
		pgl_total <- purrr::pmap(list(portfolio_quantity, portfolio_price, market_price), paper_total, allow_short)
		names(pgl_total) <- assets
		# convert results to a df
		res_df <- do.call(rbind, pgl_total) %>%
			as.data.frame() %>%
			tibble::rownames_to_column(var = "asset")

	} else if (method == "value") {

		# compute Paper Gain and Paper Loss values with paper_value()
		pgl_value <- purrr::pmap(list(portfolio_quantity, portfolio_price, market_price), paper_value, allow_short)
		names(pgl_value) <- assets
		# convert results to a df
		res_df <- do.call(rbind, pgl_value) %>%
			as.data.frame() %>%
			tibble::rownames_to_column(var = "asset")

	} else if (method == "duration") {

		# compute Paper Gain and Paper Loss duration with paper_duration()
		pgl_duration <- purrr::pmap(list(portfolio_quantity, portfolio_price, market_price), paper_duration,
												 previous_datetime, transaction_datetime, allow_short)
		names(pgl_duration) <- assets
		# convert results to a df
		res_df <- do.call(rbind, pgl_duration) %>%
			as.data.frame() %>%
			tibble::rownames_to_column(var = "asset")

	} else {# method == "all"

		# compute Paper Gain and Paper Loss with all functions and wrap-up results
		pgl_count <- purrr::pmap(list(portfolio_quantity, portfolio_price, market_price), paper_count, allow_short)
		pgl_total <- purrr::pmap(list(portfolio_quantity, portfolio_price, market_price), paper_total, allow_short)
		pgl_value <- purrr::pmap(list(portfolio_quantity, portfolio_price, market_price), paper_value, allow_short)
		pgl_duration <- purrr::pmap(list(portfolio_quantity, portfolio_price, market_price), paper_duration,
												             previous_datetime, transaction_datetime, allow_short)
		names(pgl_count) <- names(pgl_total) <- names(pgl_value) <- names(pgl_duration) <- assets
		# convert results to a df
		res_df <- cbind(do.call(rbind, pgl_count) %>% as.data.frame(), # as.data.frame() because as_tibble() removes rownames
										do.call(rbind, pgl_total) %>% as.data.frame(),
										do.call(rbind, pgl_value) %>% as.data.frame(),
										do.call(rbind, pgl_duration) %>% as.data.frame()) %>%
			tibble::rownames_to_column(var = "asset")

	}

	res_df <- tibble::as_tibble(res_df)
	return(res_df)

}

