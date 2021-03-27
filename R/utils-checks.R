#' @name checks
#'
#' @title Checks functions
#'
#' @description Functions to perform checks on arguments.
#'
#' @param df Data frame
#' @param input_values Input to check against target.
#' @param target_values Target to check for in input.
#' @param no_exception Logical. If TRUE different values of inputs from targets
#'   are not allowed to exist.
#' @param weak_target Logical. If TRUE inputs do not need to contain all the
#'    targets.
#'
#' @keywords internal
NULL


#' @describeIn checks Check consistency of values against targets and vice versa.
check_values <- function(input_values, target_values, no_exception = FALSE, weak_target = FALSE) {

	tar_idx <- target_values %in% input_values
	inp_idx <- input_values %!in% target_values

	if (weak_target) {
		tar_test <- any(tar_idx)
	} else {
		tar_test <- all(tar_idx)
	}

	if (no_exception) {

		if (tar_test & !any(inp_idx)) {
			res <- list("target" = NULL, "input" = NULL)
		} else if (tar_test & any(inp_idx)) {
			res <- list("target" = NULL, "input" = input_values[inp_idx])
		} else if (!tar_test & any(inp_idx)) {
			res <- list("target" = target_values[!tar_idx], "input" = input_values[inp_idx])
		} else {
			res <- list("target" = target_values[!tar_idx], "input" = NULL)
		}

	} else {

		if (tar_test) {
			res <- NULL
		} else {
			res <- target_values[!tar_idx]
		}

	}

	return(res)

}


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
