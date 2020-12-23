#' @name initializers
#'
#' @title Initializers functions
#'
#' @description Initializers functions
#'
#' @param investor Character string. The name of the investor.
#' @param assets Character vector. The names of the assets traded by
#'   the investor.
#' @inheritParams paper_compute
#'
#' @return Empty [tibble][tibble::tibble-package] of either investor's
#'   portfolio or investor's realized and paper gains and losses results
#'   based on the chosen method.
#'
#' @author L. Mazzucchelli & M. Zanotti
#'
#' @keywords internal
NULL


#' @describeIn initializers Empty [tibble][tibble::tibble-package]
#'   of investor's portfolio.
initializer_portfolio <- function(investor,
																	assets) {

	portfolio <- tibble::tibble(investor = investor,
											asset = assets,
											quantity = as.numeric(rep(NA, length(assets))),
											price = as.numeric(rep(NA, length(assets))),
											datetime = as.POSIXct(rep(NA, length(assets))))

	return(portfolio)

}


#' @describeIn initializers Empty [tibble][tibble::tibble-package]
#'   of investor's realized and paper gains and losses results based on
#'   the chosen method.
initializer_realized_and_paper <- function(investor,
																assets,
																method = "all") {

	if (method == "count") {
		results_df <- tibble::tibble(investor = investor,
												 asset = assets,
												 RG_count = as.numeric(rep(NA, length(assets))),
												 RL_count = as.numeric(rep(NA, length(assets))),
												 PG_count = as.numeric(rep(NA, length(assets))),
												 PL_count = as.numeric(rep(NA, length(assets))))
	} else if (method == "total") {
		results_df <- tibble::tibble(investor = investor,
												 asset = assets,
												 RG_total = as.numeric(rep(NA, length(assets))),
												 RL_total = as.numeric(rep(NA, length(assets))),
												 PG_total = as.numeric(rep(NA, length(assets))),
												 PL_total = as.numeric(rep(NA, length(assets))))
	} else if (method == "value") {
		results_df <- tibble::tibble(investor = investor,
												 asset = assets,
												 RG_value = as.numeric(rep(NA, length(assets))),
												 RL_value = as.numeric(rep(NA, length(assets))),
												 PG_value = as.numeric(rep(NA, length(assets))),
												 PL_value = as.numeric(rep(NA, length(assets))))
	} else if (method == "duration") {
		results_df <- tibble::tibble(investor = investor,
												 asset = assets,
												 RG_duration = as.numeric(rep(NA, length(assets))),
												 RL_duration = as.numeric(rep(NA, length(assets))),
												 PG_duration = as.numeric(rep(NA, length(assets))),
												 PL_duration = as.numeric(rep(NA, length(assets))))
	} else if (method == "all") {
		results_df <- tibble::tibble(investor = investor,
												 asset = assets,
												 RG_count = as.numeric(rep(NA, length(assets))),
												 RL_count = as.numeric(rep(NA, length(assets))),
												 PG_count = as.numeric(rep(NA, length(assets))),
												 PL_count = as.numeric(rep(NA, length(assets))),
												 RG_total = as.numeric(rep(NA, length(assets))),
												 RL_total = as.numeric(rep(NA, length(assets))),
												 PG_total = as.numeric(rep(NA, length(assets))),
												 PL_total = as.numeric(rep(NA, length(assets))),
												 RG_value = as.numeric(rep(NA, length(assets))),
												 RL_value = as.numeric(rep(NA, length(assets))),
												 PG_value = as.numeric(rep(NA, length(assets))),
												 PL_value = as.numeric(rep(NA, length(assets))),
												 RG_duration = as.numeric(rep(NA, length(assets))),
												 RL_duration = as.numeric(rep(NA, length(assets))),
												 PG_duration = as.numeric(rep(NA, length(assets))),
												 PL_duration = as.numeric(rep(NA, length(assets))))
	} else {# method == "none"
		results_df <- NULL
	}

	return(results_df)

}
