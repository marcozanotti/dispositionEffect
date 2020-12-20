#' @name %>%
#'
#' @title Pipe operator
#'
#' @description See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @usage lhs \%>\% rhs
#'
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
NULL


#' @name negative_match
#'
#' @title Negative match operator
#'
#' @description See \code{base::\link[base:match]{\%in\%}} for details on the
#'   match operator.
#'   The negative match operator is simply the negation of the match operator
#'   obtained via the \code{base::\link[base:Negate]{Negate}} function.
#'
#' @usage lhs \%!in\% rhs
#'
#' @rdname negative_match
#' @keywords internal
#' @export
`%!in%` <- Negate(`%in%`)


#' @title Element-wise mean calculation
#'
#' @description The function allows for the calculation of the mean on two
#'   vectors element-wise.
#'
#' @param x,y Numeric vectors of the same length.
#' @param zero.substitute Logical. If TRUE zero values are substituted with
#'   corresponding non-missing values whether possible.
#' @inheritParams base::mean
#'
#' @return Numeric vector of averages.
#'
#' @examples
#'   x <- 1:10
#'   y <- 99:90
#'   ewise_mean(x, y)
#'
#'   x[1] <- NA
#'   ewise_mean(x, y, na.rm = TRUE)
#'
#'   x[1] <- 0
#'   ewise_mean(x, y, zero.substitute = TRUE)
#'
#' @rdname elementwise_mean
#' @keywords internal
#' @export
ewise_mean <- function(x, y, na.rm = FALSE, zero.substitute = FALSE) {

	x <- as.numeric(x)
	y <- as.numeric(y)

	if (zero.substitute) {

		ewm <- dplyr::case_when(x == 0 & y == 0 ~ 0,
														x == 0 & y != 0 ~ y,
														x != 0 & y == 0 ~ x,
														TRUE ~ purrr::map2_dbl(x, y, ~ mean(c(.x,.y), na.rm = na.rm)))

	} else {

		ewm <- purrr::map2_dbl(x, y, ~ mean(c(.x,.y), na.rm = na.rm))

	}

	return(ewm)

}

