#' @name checks
#'
#' @title Checks functions
#'
#' @description Functions to perform checks on arguments.
#'
#' @param df Data frame
#' @param df_name Chr
#' @param df_names Chr
#' @param target_names Chr
#' @param input_name Chr
#' @param input_values Chr
#' @param target_values Chr
#' @param identical Lgl
#' @param input_types Chr
#' @param target_types Chr
#' @param multiple Lgl
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

	msg <- vector("list", 4)

	if (any(rg)) {
		msg[[1]] <- paste("RG value < 0 for asset(s)",
											paste(df$asset[rg], collapse = ", "), "\n")
	}

	if (any(rl)) {
		msg[[2]] <- paste("RL value > 0 for asset(s)",
											paste(df$asset[rl], collapse = ", "), "\n")
	}

	if (any(pg)) {
		msg[[3]] <- paste("PG value < 0 for asset(s)",
											paste(df$asset[pg], collapse = ", "), "\n")
	}

	if (any(pl)) {
		msg[[4]] <- paste("PL value > 0 for asset(s)",
											paste(df$asset[pl], collapse = ", "), "\n")
	}

	res_msg <- paste(purrr::compact(msg), collapse = "")
	if (res_msg == "") {
		res_msg <- NULL
	}

	return(res_msg)

}


#' @describeIn checks Check consistency of data frame variables' names.
check_df_names <- function(df_name, df_names, target_names) {

	test <- identical(target_names, df_names)
	if (!test) {
		not_names <- target_names[which(target_names %!in% df_names)]
		bad_names <- df_names[which(df_names %!in% target_names)]
		msg <- glue::glue("`{df_name}` must be a data.frame containing columns
                      `{target_names}`.\n
                      Can't find column(s) `{not_names}` in `.names({df_name})`.\n
                      Possibly misspelled column(s) `{bad_names}`?",
											target_names = glue::glue_collapse(target_names, sep = "`, `", last = "` and `"),
											not_names = glue::glue_collapse(not_names, sep = "`, `", last = "` and `"),
											bad_names = glue::glue_collapse(bad_names, sep = "`, `", last = "` and `"))
	} else {
		msg <- NULL
	}

	return(msg)

}


#' @describeIn checks Check consistency of values.
check_values <- function(input_name, input_values, target_values, identical = FALSE) {

	if (identical) {

		test <- identical(target_values, input_values)
		if (!test) {
			msg <- glue::glue("`{input_name}` must contain all
                        `{target_values}`.",
												target_values = glue::glue_collapse(target_values, sep = "`, `", last = "` or `"))
		} else {
			msg <- NULL
		}

	} else {

		test <- which(target_values %in% input_values)
		if (!length(test)) {
			msg <- glue::glue("`{input_name}` must be one of
                        `{target_values}`.",
												target_values = glue::glue_collapse(target_values, sep = "`, `", last = "` or `"))
		} else {
			msg <- NULL
		}

	}

	return(msg)

}


#' @describeIn checks Check consistency of variables' types.
check_var_types <- function(input_name, input_types, target_types, multiple = TRUE) {

	if (multiple) {

		input_types <- input_types[order(names(input_types))]
		target_types <- target_types[order(names(target_types))]

		test <- identical(target_types, input_types)
		if (!test) {
			not_types <- target_types[which(target_types %!in% input_types)]
			msg <- glue::glue("`{input_name}` elements must be of types
                        `{target_types}`.\n
                        Can't find `{not_types}` in `.map({input_name}, class)`.",
												target_types = glue::glue_collapse(target_types, sep = "`, `", last = "` and `"),
												not_types = glue::glue_collapse(not_types, sep = "`, `", last = "` and `"))
		} else {
			msg <- NULL
		}

	} else {

		test <- identical(target_types, input_types)
		if (!test) {
			not_types <- target_types[which(target_types %!in% input_types)]
			msg <- glue::glue("`{input_name}` must be of type `{target_types}`.\n
                        Can't find `{not_types}` in `.class({input_name})`.",
												target_types = glue::glue_collapse(target_types, sep = "`, `", last = "` and `"),
												not_types = glue::glue_collapse(not_types, sep = "`, `", last = "` and `"))
		} else {
			msg <- NULL
		}

	}

	return(msg)

}

