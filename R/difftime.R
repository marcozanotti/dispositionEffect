#' @name difference_in_time
#'
#' @title Difference in time
#'
#' @description Difference in time
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
#'     \item{\code{difftime_financial}}{returns a numeric value in hours.
#'       To convert in a different time unit use
#'       \code{base::\link[base:as.numeric]{as.numeric}} and specify the
#'       \code{units} argument (units = c("secs", "mins", "hours", "days",
#'       "weeks")).}
#'     \item{\code{difftime_compare}}{returns a character string whose possible
#'       values are "greater" or "smaller", indicating whether the
#'       \code{difftime_financial} evaluated between the two dates is greater or
#'       smaller than the chosen threshold.}
#'   }
#'
#' @examples
#'   from <- as.POSIXct("2020-01-01 09:00:00", tz = "UTC")
#'   to <- as.POSIXct("2020-01-01 18:00:00", tz = "UTC")
#'   difftime(to, from)
#'   as.numeric(difftime(to, from))
#'   difftime_financial(from, to)
#'
#'   from <- as.POSIXct("2020-01-01 09:00:00", tz = "UTC")
#'   to <- as.POSIXct("2020-01-02 18:00:00", tz = "UTC")
#'   difftime(to, from)
#'   as.numeric(difftime(to, from), units = "hours")
#'   difftime_financial(from, to)
#'   difftime_financial(from, to, after_market = 19) # Italian market
#'
#'   difftime_compare(from, to, "5 mins")
#'   difftime_compare(from, to, "5 hours")
#'   difftime_compare(from, to, "5 days")
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
difftime_financial <- function(from,
															 to,
															 pre_market = 08,
															 after_market = 22,
															 units = "hours") {

	from_date <- as.Date(from)
	to_date <- as.Date(to)
	upp_from <- paste0(from_date, " ", after_market, ":00:00") # upper bound first day
	low_to <- paste0(to_date, " ", pre_market, ":00:00") # lower bound last day

	if (from > upp_from) {
		from <- upp_from
	}
	if (to < low_to) {
		to <- low_to
	}

	if (from_date == to_date) {
		# if same date, then simple difftime by days
		res <- as.numeric(difftime(to, from, units = units))
	} else {
		# if different dates, then new difftime
		s <- lubridate::wday(seq(from_date, to_date, by = "days"), week_start = 1)
		len <- sum(s %in% 1:5) - 2 # num working days -2 (first and last)
		h <- after_market - pre_market # financial working hours in a day (from 8.00 to 19.00)
		res <- as.numeric(
			len * h + # total financial working "hours" (units) between the two dates
				difftime(upp_from, from, units = units) +
				difftime(to, low_to, units = units)
		)
	}

	return(res) # return result in hours

}


#' @describeIn difference_in_time Comparison of the \code{difftime_financial}
#'   with respect to a given time threshold.
#' @export
difftime_compare <- function(from,
														 to,
														 time_threshold = "0 mins") {
	# units = c("secs", "mins", "hours", "days", "weeks")

	if (length(strsplit(time_threshold, "\\s")[[1]]) <= 1) {
		stop(paste("Please correctly specify the time_threshold argument. Possibly a space is missing."))
	}

	chrs <- unlist(strsplit(time_threshold, "\\s"))
	units <- chrs[2]
	value <- as.numeric(chrs[1])

	dtt_diff <- difftime_financial(from, to, units = units)

	if (dtt_diff >= value) {
		res <- "greater"
	} else {
		res <- "smaller"
	}

	return(res)

}

