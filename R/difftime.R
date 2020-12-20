#' @name difference_in_time
#'
#' @title Difference in time
#'
#' @description Difference in time
#'
#' @details
#'
#' @param from POSIXct for the initial date.
#' @param to POSIXct for the final date.
#' @param pre_market numeric or character representing the hour of the day at
#'   which the pre-market begins.
#' @param after_market numeric or character representing the hour of the day at
#'   which the after-market ends.
#' @param time_threshold character in the format "value units" indicating the
#'   time threshold at which the computed financial difftime has to be evaluated
#'   (for instance "05 mins" or "20 hours").
#'   The allowed units are "secs", "mins", "hours", "days" and "weeks"
#'   (See \code{base::\link[base:difftime]{difftime}}).
#'
#' @return
#'   \describe{
#'     \item{\code{financial_difftime}}{returns a numeric value in hours.
#'       To convert in a different time unit use
#'       \code{base::\link[base:as.numeric]{as.numeric}} and specify the
#'       \code{units} argument (units = c("secs", "mins", "hours", "days",
#'       "weeks")).}
#'     \item{\code{compare_difftime}}{returns a character string whose possible
#'       values are "greater" or "smaller", indicating whether the
#'       \code{financial_difftime} evaluated between the two dates is greater or
#'       smaller than the chosen threshold.}
#'   }
#'
#' @examples
#'   from <- as.POSIXct("2020-01-01 09:00:00", tz = "UTC")
#'   to <- as.POSIXct("2020-01-01 18:00:00", tz = "UTC")
#'   difftime(to, from)
#'   as.numeric(difftime(to, from))
#'   financial_difftime(from, to)
#'
#'   from <- as.POSIXct("2020-01-01 09:00:00", tz = "UTC")
#'   to <- as.POSIXct("2020-01-02 18:00:00", tz = "UTC")
#'   difftime(to, from)
#'   as.numeric(difftime(to, from), units = "hours")
#'   financial_difftime(from, to)
#'   financial_difftime(from, to, after_market = 19) # Italian market
#'
#'   compare_difftime(from, to, "5 mins")
#'   compare_difftime(from, to, "5 hours")
#'   compare_difftime(from, to, "5 days")
#'
#' @author L. Mazzucchelli & M. Zanotti
#'
#' @references H. Shefrin & M. Statman, 1985
#'
#' @seealso \code{base::\link[base:difftime]{difftime}}
NULL


#' @describeIn difference_in_time Extension of the
#'   \code{base::\link[base:difftime]{difftime}} function to calculate the
#'   actual financial difftime taking into account pre and after market
#'   moments and financial markets closing days.
#' @export
financial_difftime <- function(from, to, pre_market = 08, after_market = 22) {

	upp_from <- as.POSIXct(paste0(as.Date(from), " ", after_market, ":00:00")) # upper bound first day
	low_to <- as.POSIXct(paste0(as.Date(to), " ", pre_market, ":00:00")) # lower bound last day
	if (from > upp_from) {
		from <- upp_from
	}
	if (to < low_to) {
		to <- low_to
	}

	if (as.Date(from) == as.Date(to)) {
		# if same date, then simple difftime by days
		res <- difftime(to, from, units = "hours") %>% as.numeric()
	} else {
		# if different dates, then new difftime
		s <- seq(as.Date(from), as.Date(to), by = "days") %>%
			lubridate::wday(week_start = 1)
		len <- which(s %in% 1:5) %>% length() - 2 # num working days -2 (first and last)
		h <- after_market - pre_market # financial working hours in a day (from 8.00 to 19.00)
		res <- len * h # total financial working hours between the two dates

		res <- res + difftime(upp_from, from, units = "hours") %>% as.numeric()
		res <- res + difftime(to, low_to, units = "hours") %>% as.numeric()
	}

	return(res) # return result in hours

}


#' @describeIn difference_in_time Comparison of the \code{financial_difftime}
#'   with respect to a given time threshold.
#' @export
compare_difftime <- function(from, to, time_threshold = "0 mins") {
	# units = c("secs", "mins", "hours", "days", "weeks")

	units <- stringr::str_split(time_threshold, "\\s")[[1]][2]
	value <- stringr::str_split(time_threshold, "\\s")[[1]][1] %>% as.numeric()

	dtt_diff <- financial_difftime(from, to) %>%
		as.difftime(units = "hours") %>% # financial_difftime returns in hours
		as.numeric(units = units)

	if (dtt_diff >= value) {
		res <- "greater"
	} else {
		res <- "smaller"
	}

	return(res)

}

