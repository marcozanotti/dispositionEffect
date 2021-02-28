#' @name checks
#'
#' @title Checks functions
#'
#' @description Functions to perform checks on arguments.
#'
#' @param df Data frame
#' @param input_values Input to check against target.
#' @param target_values Target to check for in input.
#'
#' @return A character string containing an error message or NULL.
#'
#' @keywords internal
NULL


#' @describeIn checks Check consistency of gains and losses "values" results.
check_gainloss <- function(df) {

	rg <- df$RG_value < 0
	rl <- df$RL_value > 0
	pg <- df$PG_value < 0
	pl <- df$PL_value > 0

	msg <- vector("character", 4)

	if (any(rg)) {
		msg[1] <- paste("RG value < 0 for asset(s)", paste(df$asset[rg], collapse = ", "), "\n")
	}

	if (any(rl)) {
		msg[2] <- paste("RL value > 0 for asset(s)", paste(df$asset[rl], collapse = ", "), "\n")
	}

	if (any(pg)) {
		msg[3] <- paste("PG value < 0 for asset(s)", paste(df$asset[pg], collapse = ", "), "\n")
	}

	if (any(pl)) {
		msg[4] <- paste("PL value > 0 for asset(s)", paste(df$asset[pl], collapse = ", "), "\n")
	}

	res_msg <- paste(msg, collapse = "")
	if (res_msg == "") {
		res_msg <- NULL
	}

	return(res_msg)

}


#' @describeIn checks Check consistency of market prices "values" results.
check_market_prices <- function(input_values, target_values) {

	idx <- target_values %in% input_values

	if (all(idx)) {
		res_msg <- NULL
	} else {
		res_msg <- paste("No market prices available for asset(s)", paste(target_values[!idx], collapse = ", "), "\n")

	}

	return(res_msg)

}
