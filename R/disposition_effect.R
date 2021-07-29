#' @name disposition_effect
#'
#' @title Disposition Effect
#'
#' @description Compute the disposition effect and the disposition
#'   difference.
#'
#' @details
#'   The disposition effect is defined as
#'   \eqn{DE = (Realized Gain / (Realized Gain - Paper Gain)) -
#'        (Realized Loss / (Realized Loss + Paper Loss))}
#'
#'   The disposition difference is defined as
#'   \eqn{DD = Realized Gain - |Realized Loss|}
#'   or
#'   \eqn{DD = Paper Gain - |Paper Loss|}
#'
#' @param realized_gains Numeric vector (or scalar) containing realized gains
#'   values.
#' @param paper_gains Numeric vector (or scalar) containing paper gains
#'   values.
#' @param realized_losses Numeric vector (or scalar) containing realized losses
#'   values.
#' @param paper_losses Numeric vector (or scalar) containing paper losses
#'   values.
#' @param gains Numeric vector (or scalar) containing gains.
#' @param losses Numeric vector (or scalar) containing losses.
#' @param gainslosses Data frame, the portfolio of the investor containing the
#'   realized and paper gains and losses results (as those obtained via
#'   \code{\link{portfolio_compute}}).
#' @param dispdiff_value Logical, if TRUE the disposition difference on the
#'   "value" method is computed. Default to disposition effect (FALSE).
#' @param aggregate_fun Function to use to aggregate results.
#'   Default to \code{NULL}, that is no aggregation is performed and the
#'   results of each asset are shown.
#' @param ... Further arguments to be passed to the aggregate function.
#' @param de_timeseries Data frame, the time series of disposition effects.
#'
#' @return Numeric vector (or scalar) with the value(s) of disposition
#'   effect(s) or disposition difference(s).
#'
NULL


#' @describeIn disposition_effect Compute the disposition effect
#' @export
disposition_effect <- function(realized_gains, paper_gains, realized_losses, paper_losses) {

	gains <- realized_gains / (realized_gains + paper_gains)
	losses <- abs(realized_losses / (realized_losses + paper_losses))

	gains[is.nan(gains)] <- 0
	losses[is.nan(losses)] <- 0

	de <- gains - losses

	return(de)

}


#' @describeIn disposition_effect Compute the disposition difference
#' @export
disposition_difference <- function(gains, losses) {

	dd <- gains - abs(losses)
	return(dd)

}


#' @describeIn disposition_effect Compute the disposition effect directly on
#'   the investor's portfolio containing realized and paper gains and losses
#'   results.
#' @export
disposition_compute <- function(gainslosses, dispdiff_value = FALSE, aggregate_fun = NULL, ...) {

	res <- NULL

	count <- any(grepl("count", names(gainslosses)))
	total <- any(grepl("total", names(gainslosses)))
	value <- any(grepl("value", names(gainslosses)))
	duration <- any(grepl("duration", names(gainslosses)))

	if (!count & !total & !value & !duration) {
		# if no columns contain count | total | value | duration
		stop("No columns containing 'count', 'total', 'value' or 'duration'.")

	} else {

		if (count) {
			de_count <- disposition_effect(
				gainslosses$RG_count,
				gainslosses$PG_count,
				gainslosses$RL_count,
			  gainslosses$PL_count
			)
			res$DE_count <- de_count
		}
		if (total) {
			de_total <- disposition_effect(
				gainslosses$RG_total,
				gainslosses$PG_total,
				gainslosses$RL_total,
				gainslosses$PL_total
			)
			res$DE_total <- de_total
		}
		if (value) {
			if (dispdiff_value) {
				dd_value <- disposition_difference(
					gainslosses$RG_value,
					gainslosses$RL_value
				)
				res$DD_value <- dd_value
			} else {
				de_value <- disposition_effect(
					gainslosses$RG_value,
					gainslosses$PG_value,
					gainslosses$RL_value,
					gainslosses$PL_value
				)
				res$DE_value <- de_value
			}
		}
		if (duration) {
			dd_duration <- disposition_difference(
				gainslosses$RG_duration,
				gainslosses$RL_duration
			)
			res$DD_duration <- dd_duration
		}

	}

	if (!is.null(aggregate_fun)) {
		res <- purrr::map_df(res, aggregate_fun, ...)
		final_res <- cbind(dplyr::select(gainslosses[1, ], !!rlang::sym("investor")), res)
	} else  {
		final_res <- cbind(gainslosses[, c("investor", "asset")], res)
	}

	return(final_res)

}


#' @describeIn disposition_effect Compute the time series disposition effect
#'   on the gains and losses results.
#' @export
disposition_compute_ts <- function(gainslosses, aggregate_fun = NULL, ...) {

	count <- any(grepl("count", names(gainslosses)))
	value <- any(grepl("value", names(gainslosses)))

	if (!count & !value) {
		# if no columns contain count | total | value | duration
		stop("No columns containing 'count' or 'value'.")

	} else {

		if (count & value) {
			de_count <- disposition_effect(
				gainslosses$RG_count,
				gainslosses$PG_count,
				gainslosses$RL_count,
				gainslosses$PL_count
			)
			de_value <- disposition_effect(
				gainslosses$RG_value,
				gainslosses$PG_value,
				gainslosses$RL_value,
				gainslosses$PL_value
			)
			res <- data.frame("DE_count" = de_count, "DE_value" = de_value)
		} else	if (count) {
			de_count <- disposition_effect(
				gainslosses$RG_count,
				gainslosses$PG_count,
				gainslosses$RL_count,
				gainslosses$PL_count
			)
			res <- data.frame("DE_count" = de_count)
		} else {
			de_value <- disposition_effect(
				gainslosses$RG_value,
				gainslosses$PG_value,
				gainslosses$RL_value,
				gainslosses$PL_value
			)
			res <- data.frame("DE_value" = de_value)
		}

	}

	if (!is.null(aggregate_fun)) {
		final_res <- as.data.frame(purrr::map(res, aggregate_fun, ...))
	} else  {
		final_res <- cbind(gainslosses[, "asset"], res)
		names(final_res)[1] <- "asset"
	}

	return(final_res)

}


#' @describeIn disposition_effect Wrapper that returns the most important
#'   summary statistics related to the disposition effect.
#' @export
disposition_summary <- function(gainslosses, dispdiff_value = FALSE) {

	res <- dplyr::bind_rows(
		disposition_compute(gainslosses, dispdiff_value, min, na.rm = TRUE),
		disposition_compute(gainslosses, dispdiff_value, stats::quantile, probs = .25, na.rm = TRUE, names = FALSE),
		disposition_compute(gainslosses, dispdiff_value, stats::median, na.rm = TRUE),
		disposition_compute(gainslosses, dispdiff_value, stats::quantile, probs = .75, na.rm = TRUE, names = FALSE),
		disposition_compute(gainslosses, dispdiff_value, mean, na.rm = TRUE),
		disposition_compute(gainslosses, dispdiff_value, max, na.rm = TRUE),
		disposition_compute(gainslosses, dispdiff_value, stats::sd, na.rm = TRUE)
	) %>%
		dplyr::mutate(stat = c("Min", "Q1", "Median", "Q3", "Mean", "Max", "StDev"), .after = "investor")

	return(res)

}

#' @describeIn disposition_effect Wrapper that returns the most important
#'   summary statistics related to the time series disposition effect.
#' @export
disposition_summary_ts <- function(de_timeseries) {

	df_tmp <- dplyr::select(de_timeseries, dplyr::matches("D(E|D)")) # allows also DD
	res <- dplyr::bind_rows(
		purrr::map(df_tmp, min, na.rm = TRUE),
		purrr::map(df_tmp, stats::quantile, probs = .25, na.rm = TRUE, names = FALSE),
		purrr::map(df_tmp, stats::median, na.rm = TRUE),
		purrr::map(df_tmp, stats::quantile, probs = .75, na.rm = TRUE, names = FALSE),
		purrr::map(df_tmp, mean, na.rm = TRUE),
		purrr::map(df_tmp, max, na.rm = TRUE),
		purrr::map(df_tmp, stats::sd, na.rm = TRUE)
	) %>%
		dplyr::mutate(
			investor = de_timeseries$investor[1],
			stat = c("Min", "Q1", "Median", "Q3", "Mean", "Max", "StDev"),
			.before = dplyr::everything()
		) %>%
		as.data.frame()

	return(res)

}
