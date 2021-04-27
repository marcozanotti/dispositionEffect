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
#'   of investor's time series disposition effect based on
#'   the chosen method.
initializer_timeseries_DE <- function(investor, assets, datetimes, method) {

	if (is.null(assets)) {

		if (method == "count") {
			ts_df <- data.frame(
				investor = investor,
				datetime = datetimes,
				DETs_count = rep(NA_real_, length(datetimes)),
				DEts_count = rep(NA_real_, length(datetimes))
			)
		} else if (method == "value") {
			ts_df <- data.frame(
				investor = investor,
				datetime = datetimes,
				DETs_value = rep(NA_real_, length(datetimes)),
				DEts_value = rep(NA_real_, length(datetimes))
			)
		} else {# method all
			ts_df <- data.frame(
				investor = investor,
				datetime = datetimes,
				DETs_count = rep(NA_real_, length(datetimes)),
				DEts_count = rep(NA_real_, length(datetimes)),
				DETs_value = rep(NA_real_, length(datetimes)),
				DEts_value = rep(NA_real_, length(datetimes))
			)
		}

	} else {# if assets is not NULL

		if (method == "count") {

			assets_nms <- sort(
				paste(assets,	c(rep("DETs_count", length(assets)), rep("DEts_count", length(assets))), sep = "_"),
				method = "radix"
			)
			ts_df <- as.data.frame(matrix(NA_real_, length(datetimes), 2 + 2 + length(assets_nms)))
			names(ts_df) <- c("investor", "datetime", "DETs_count", "DEts_count", assets_nms)
			ts_df$investor <- investor
			ts_df$datetime <- datetimes

		} else if (method == "value") {

			assets_nms <- sort(
				paste(assets,	c(rep("DETs_value", length(assets)), rep("DEts_value", length(assets))), sep = "_"),
				method = "radix"
			)
			ts_df <- as.data.frame(matrix(NA_real_, length(datetimes), 2 + 2 + length(assets_nms)))
			names(ts_df) <- c("investor", "datetime", "DETs_value", "DEts_value", assets_nms)
			ts_df$investor <- investor
			ts_df$datetime <- datetimes

		} else {# method all

			assets_nms <- c(
				sort(
					paste(assets,	c(rep("DETs_count", length(assets)), rep("DEts_count", length(assets))), sep = "_"),
					method = "radix"
				),
				sort(
					paste(assets,	c(rep("DETs_value", length(assets)), rep("DEts_value", length(assets))), sep = "_"),
					method = "radix"
				)
			)
			ts_df <- as.data.frame(matrix(NA_real_, length(datetimes), 2 + 4 + length(assets_nms)))
			names(ts_df) <- c("investor", "datetime", "DETs_count", "DEts_count", "DETs_value", "DEts_value", assets_nms)
			ts_df$investor <- investor
			ts_df$datetime <- datetimes

		}

	}

	return(ts_df)

}
