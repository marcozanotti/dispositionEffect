#' @name utils
#'
#' @title Utilities
#'
#' @description Utilities funcions
#'
#' @param x,y Numeric vectors of the same length.
#' @param zero.substitute Logical. If TRUE zero values are substituted with
#'   corresponding non-missing values whether possible.
#' @inheritParams base::mean
#'
#' @keywords internal
NULL


#' @describeIn utils Element-wise mean calculation.
ewise_mean <- function(x, y, na.rm = FALSE, zero.substitute = FALSE) {

	x <- as.numeric(x)
	y <- as.numeric(y)

	if (zero.substitute) {

		ewm <- dplyr::case_when(
			x == 0 & y == 0 ~ 0,
			x == 0 & y != 0 ~ y,
			x != 0 & y == 0 ~ x,
			TRUE ~ purrr::map2_dbl(x, y, ~ mean(c(.x,.y), na.rm = na.rm))
		)

	} else {

		ewm <- purrr::map2_dbl(x, y, ~ mean(c(.x,.y), na.rm = na.rm))

	}

	return(ewm)

}
