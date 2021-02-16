#' @name utils
#'
#' @title Utilities
#'
#' @description Functions that help the computations.
#'
#' @param x,y Numeric vectors of the same length.
#' @param zero.substitute Logical. If TRUE zero values are substituted with
#'   corresponding non-missing values whether possible.
#' @inheritParams base::mean
#'
#' @return Different return behaviours.
#'
#' @author L. Mazzucchelli & M. Zanotti
#'
#' @references H. Shefrin & M. Statman, 1985
#'
#' @examples
#'   \dontrun{
#'   x <- 1:10
#'   y <- 99:90
#'   ewise_mean(x, y)
#'
#'   x[1] <- NA
#'   ewise_mean(x, y, na.rm = TRUE)
#'
#'   x[1] <- 0
#'   ewise_mean(x, y, zero.substitute = TRUE)
#'   }
#'
NULL


#' @describeIn utils Element-wise mean calculation.
#' @keywords internal
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


