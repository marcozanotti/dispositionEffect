#' @name disposition_effect
#'
#' @title Disposition Effect
#'
#' @description Compute the disposition effect in a vectorized way.
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
#' @param portfolio Data frame, the portfolio of the investor containing the
#'   realized and paper gains and losses results (as those obtained via
#'   \code{\link{portfolio_compute}}).
#' @param aggregate_fun Function to use to aggregate results.
#'   Default to \code{NULL}, that is no aggregation is performed and the
#'   results of each asset are shown.
#' @param plot Logical. If TRUE some useful graphs of the disposition effect
#'   results are plotted.
#'
#' @return Numeric vector (or scalar) with the value(s) of disposition
#'   effect(s) or disposition difference(s).
#'   A [tibble][tibble::tibble-package] is returned by ................
#'
#' @author L. Mazzucchelli & M. Zanotti
#'
#' @references H. Shefrin & M. Statman, 1985
#'
#' @examples
#'   data(portfolio_results)
#'
#'   # Disposition effect on count for each traded asset
#'   de <- disposition_effect(portfolio_results$RG_count,
#'                            portfolio_results$PG_count,
#'                            portfolio_results$RL_count,
#'                            portfolio_results$PL_count)
#'   names(de) <- portfolio_results$asset
#'
#'   # Average disposition effect of the investor
#'   mean(de)
#'
#'   # Realized difference on value for each asset
#'   disposition_difference(portfolio_results$RG_value, portfolio_results$RL_value)
#'
#'   # Paper difference on value for each asset
#'   disposition_difference(portfolio_results$PG_value, portfolio_results$PL_value)
#'
NULL


#' @describeIn disposition_effect Compute the disposition effect as defined
#'   by L. Mazzucchelli et al.
#' @export
disposition_effect <- function(realized_gains, paper_gains, realized_losses, paper_losses) {

	gains <- realized_gains / (realized_gains + paper_gains)
	losses <- abs( realized_losses / (realized_losses + paper_losses) )

	gains[is.nan(gains)] <- 0
	losses[is.nan(losses)] <- 0

	de <- gains - losses

	return(de)

}


#' @describeIn disposition_effect Compute the disposition difference as defined
#'   by L. Mazzucchelli et al.
#' @export
disposition_difference <- function(gains, losses) {

	dd <- gains - abs(losses)
	return(dd)

}


#' @describeIn disposition_effect Compute the disposition effect directly on
#'   the investor's portfolio containing realized and paper gains and losses
#'   results.
#' @export
disposition_compute <- function(portfolio, aggregate_fun = NULL) {

	res <- dplyr::select(portfolio, "investor", "asset")

	count <- any(grepl("count", names(portfolio)))
	total <- any(grepl("total", names(portfolio)))
	value <- any(grepl("value", names(portfolio)))
	duration <- any(grepl("duration", names(portfolio)))

	if (!count | !total | !value | !duration) {
		# if no columns contain count | total | value | duration
		stop("No columns containing 'count', 'total', 'value' or 'duration'.")

	} else {

		if (count) {
			de_count <- disposition_effect(portfolio$RG_count,
																		 portfolio$PG_count,
																		 portfolio$RL_count,
																		 portfolio$PL_count)
			res <- dplyr::mutate(res, DE_count = de_count)
		}
		if (total) {
			de_total <- disposition_effect(portfolio$RG_total,
																		 portfolio$PG_total,
																		 portfolio$RL_total,
																		 portfolio$PL_total)
			res <- dplyr::mutate(res, DE_total = de_total)
		}
		if (value) {
			dd_value <- disposition_difference(portfolio$RG_value,
																				 portfolio$RL_value)
			res <- dplyr::mutate(res, DD_value = dd_value)
		}
		if (duration) {
			dd_duration <- disposition_difference(portfolio$RG_duration,
																						portfolio$RL_duration)
			res <- dplyr::mutate(res, DD_duration = dd_duration)
		}

	}

	if (!is.null(aggregate_fun)) {
		res <- res %>%
			dplyr::group_by(!!rlang::sym("investor")) %>%
			dplyr::summarise(dplyr::across(dplyr::contains("_"), .fns = aggregate_fun),
											 .groups = "drop")
	}

	return(res)

}


#' @describeIn disposition_effect Wrapper that returns the most important
#'   summary statistics related to the disposition effect.
#' @export
disposition_summary <- function(portfolio, plot = FALSE) {

	de <- disposition_compute(portfolio)
	de_aggr <- dplyr::bind_rows(disposition_compute(portfolio, function(x) mean(x, na.rm = TRUE)),
															disposition_compute(portfolio, function(x) stats::median(x, na.rm = TRUE)),
															disposition_compute(portfolio, function(x) min(x, na.rm = TRUE)),
															disposition_compute(portfolio, function(x) max(x, na.rm = TRUE))) %>%
		dplyr::mutate(stat = c("Mean", "Median", "Min", "Max"), .after = "investor")
	de_summary <- list(
		"Disposition Effects by Assets" = de[, -1],
		"Summary Stastistics"           = de_aggr[, -1]
	)

	cat(paste("\n\nInvestor", portfolio$investor[1], "\n\n"))
	cat(knitr::kable(de_summary, digits = 3))

	if (plot) {
		# new function that creates ggplot graphs based on count, total, value and duration
	}

	res <- list("de" = de, "stats" = de_aggr)
	return(invisible(res))

}

