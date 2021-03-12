#' @name initializers
#'
#' @title Initializers functions
#'
#' @description Initializers functions
#'
#' @param investor Character string. The name of the investor.
#' @param assets Character vector. The names of the assets traded by
#'   the investor.
#' @param datetimes POSIXct vector. The datetimes of the investors'
#'   transactions.
#' @inheritParams paper_compute
#'
#' @return Empty \code{data.frame} of either investor's
#'   portfolio, investor's realized and paper gains and losses results,
#'   or time series Disposition Effect based on the chosen method.
#'
#' @author L. Mazzucchelli & M. Zanotti
#'
#' @keywords internal
NULL


#' @describeIn initializers Empty \code{data.frame}
#'   of investor's portfolio.
initializer_portfolio <- function(investor, assets) {

	portfolio <- data.frame(
		investor = investor,
		asset = assets,
		quantity = rep(NA_real_, length(assets)),
		price = rep(NA_real_, length(assets)),
		datetime = as.POSIXct(rep(NA, length(assets)))
	)

	return(portfolio)

}


#' @describeIn initializers Empty \code{data.frame}
#'   of investor's realized and paper gains and losses results based on
#'   the chosen method.
initializer_realized_and_paper <- function(investor, assets, method = "all") {

	if (method == "count") {
		results_df <- data.frame(
			investor = investor,
			asset = assets,
			RG_count = rep(NA_real_, length(assets)),
			RL_count = rep(NA_real_, length(assets)),
			PG_count = rep(NA_real_, length(assets)),
			PL_count = rep(NA_real_, length(assets))
		)
	} else if (method == "total") {
		results_df <- data.frame(
			investor = investor,
			asset = assets,
			RG_total = rep(NA_real_, length(assets)),
			RL_total = rep(NA_real_, length(assets)),
			PG_total = rep(NA_real_, length(assets)),
			PL_total = rep(NA_real_, length(assets))
		)
	} else if (method == "value") {
		results_df <- data.frame(
			investor = investor,
			asset = assets,
			RG_value = rep(NA_real_, length(assets)),
			RL_value = rep(NA_real_, length(assets)),
			PG_value = rep(NA_real_, length(assets)),
			PL_value = rep(NA_real_, length(assets))
		)
	} else if (method == "duration") {
		results_df <- data.frame(
			investor = investor,
			asset = assets,
			RG_duration = rep(NA_real_, length(assets)),
			RL_duration = rep(NA_real_, length(assets)),
			PG_duration = rep(NA_real_, length(assets)),
			PL_duration = rep(NA_real_, length(assets))
		)
	} else if (method == "all") {
		results_df <- data.frame(
			investor = investor,
			asset = assets,
			RG_count = rep(NA_real_, length(assets)),
			RL_count = rep(NA_real_, length(assets)),
			PG_count = rep(NA_real_, length(assets)),
			PL_count = rep(NA_real_, length(assets)),
			RG_total = rep(NA_real_, length(assets)),
			RL_total = rep(NA_real_, length(assets)),
			PG_total = rep(NA_real_, length(assets)),
			PL_total = rep(NA_real_, length(assets)),
			RG_value = rep(NA_real_, length(assets)),
			RL_value = rep(NA_real_, length(assets)),
			PG_value = rep(NA_real_, length(assets)),
			PL_value = rep(NA_real_, length(assets)),
			RG_duration = rep(NA_real_, length(assets)),
			RL_duration = rep(NA_real_, length(assets)),
			PG_duration = rep(NA_real_, length(assets)),
			PL_duration = rep(NA_real_, length(assets))
		)
	} else {# method == "none"
		results_df <- NULL
	}

	return(results_df)

}


#' @describeIn initializers Empty \code{data.frame}
#'   of investor's time series Disposition Effect based on
#'   the chosen method.
initializer_timeseries_DE <- function(investor, datetimes, method = "all") {

	if (method == "count") {
		ts_df <- data.frame(
			investor = investor,
			datetime = datetimes,
			DEts_count = rep(NA_real_, length(datetimes)),
			DETs_count = rep(NA_real_, length(datetimes))
		)
	} else if (method == "value") {
		ts_df <- data.frame(
			investor = investor,
			datetime = datetimes,
			DDts_value = rep(NA_real_, length(datetimes)),
			DDTs_value = rep(NA_real_, length(datetimes))
		)
	} else if (method == "all") {
		ts_df <- data.frame(
			investor = investor,
			datetime = datetimes,
			DEts_count = rep(NA_real_, length(datetimes)),
			DETs_count = rep(NA_real_, length(datetimes)),
			DDts_value = rep(NA_real_, length(datetimes)),
			DDTs_value = rep(NA_real_, length(datetimes))
		)
	} else {# method == "none"
		ts_df <- NULL
	}

	return(ts_df)

}
