#' @name meanrev_compute
#'
#' @title Mean-Reversion Compute
#'
#' @description Computation of the mean-reversion analysis allowing
#'   to the identification of follower/contrarian behaviours on assets.
#'
#' @inheritParams portfolio_compute
#' @param initial_portfolio The investor's initial portfolio as computed by
#'   means of \code{\link{portfolio_compute}} with \code{method = "none"}.
#'   Default to NULL when no transactions where made by the investor in the
#'   period before the mean-reversion analysis.
#' @param trend Character, indicating whether the mean reversion analysis is
#'   performed on a "positive" or "negative" trend.
#' @param verbose Logical allowing to control for the function's verbosity.
#'
#' @return A data frame containing the investor's
#'   portfolio with mean-reversion information.
#'   Variables \code{follower} and \code{contrarian} count how many times each
#'   asset has followed the trend or not.
#'   Variable \code{investor_type} shows aggregately whether each asset should
#'   be considered as follower, contrarian or indefinite.#'
#'
#' @seealso \code{\link{portfolio_compute}}
#'
#' @export
meanrev_compute <- function(
	portfolio_transactions,
	initial_portfolio = NULL,
	trend = "positive",
	verbose = FALSE,
	progress = FALSE
) {

	# defensive programming
	# check initial_portfolio column names
	if (!is.null(initial_portfolio)) {
		trg <- c("investor", "asset", "quantity")
		chk <- check_values(names(initial_portfolio), trg)
		if (!is.null(chk)) {
			stop(paste0("initial_portfolio must contain columns '", paste(trg, collapse = "', '"), "'.\n",
									"Can't find column(s) '", paste(chk, collapse = "', '"), "'. Possibly misspelled column names?\n"),
					 call. = FALSE)
		}
	}
	# check portfolio_transactions column names
	trg <- c("investor", "type", "asset", "quantity", "price", "datetime")
	chk <- check_values(names(portfolio_transactions), trg)
	if (!is.null(chk)) {
		stop(paste0("portfolio_transactions must contain columns '", paste(trg, collapse = "', '"), "'.\n",
								"Can't find column(s) '", paste(chk, collapse = "', '"), "'. Possibly misspelled column names?\n"),
				 call. = FALSE)
	}
	# check portfolio_transactions column type values
	trg <- c("B", "S")
	chk <- check_values(unique(portfolio_transactions$type), trg, no_exception = TRUE, weak_target = TRUE)
	if (!is.null(chk$target) | !is.null(chk$input)) {
		stop(paste0("Column type of portfolio_transactions should contain 'B' or 'S' only.\n"), call. = FALSE)
	}


	# global parameters
	investor_id <- portfolio_transactions$investor[1]
	investor_assets <- sort(unique(portfolio_transactions$asset), method = "radix")
	investor_initial_assets <- sort(unique(initial_portfolio$asset), method = "radix")

	# investor's initial portfolio (portfolio at time 0)
	if (!is.null(initial_portfolio)) {
		meanrev_portfolio <- initializer_portfolio(investor_id, investor_assets) %>%
			dplyr::select("investor", "asset", "quantity") %>%
			dplyr::mutate("follower" = 0, "contrarian" = 0) %>%
			dplyr::filter(!(!!rlang::sym("asset") %in% investor_initial_assets)) %>% # keep only those assets not in the initial portfolio
			dplyr::bind_rows( # bind with the initial portfolio
				initial_portfolio %>%
					dplyr::select("investor", "asset", "quantity") %>%
					dplyr::mutate("follower" = 0, "contrarian" = 0)
			) %>%
			dplyr::arrange(!!rlang::sym("asset"))
	} else {
		meanrev_portfolio <- initializer_portfolio(investor_id, investor_assets) %>%
			dplyr::select("investor", "asset", "quantity") %>%
			dplyr::mutate("follower" = 0, "contrarian" = 0)
	}

	# progress bar
	if (progress) {
		# initialize progress bar
		pb <- progress::progress_bar$new(
			format = ":current  [:bar] :percent in :elapsed\n\n",
			total = nrow(portfolio_transactions),
			clear = FALSE, width = 60, show_after = 0
		)
		pb$tick(0)
	}


	for (i in seq_len(nrow(portfolio_transactions))) {

		# extract scalars (trx = transaction)
		trx_type <- portfolio_transactions[i, ]$type # trx type
		trx_asset <- portfolio_transactions[i, ]$asset # trx asset
		trx_qty <- portfolio_transactions[i, ]$quantity # trx quantity

		# if it's a sell transaction then consider qty as negative
		if (trx_type == "S") {
			trx_qty <- trx_qty * -1L
		}

		# update the portfolio
		if (verbose) message(paste0("Updating mean-reversion portfolio.. (", trx_asset, " asset)"))
		meanrev_portfolio <- update_meanrev_portfolio(meanrev_portfolio, trx_asset, trx_qty, trx_type, trend)

		if (verbose) message("Done!")
		if (progress) {
			pb$tick()
		} # update progress bar

		rm(trx_type, trx_asset, trx_qty)

	} # close loop


	meanrev_portfolio <- meanrev_portfolio %>%
		dplyr::mutate(
			"investor_type" = dplyr::case_when(
				!!rlang::sym("follower") > 0 & !!rlang::sym("contrarian") == 0 ~ "follower",
				!!rlang::sym("follower") == 0 & !!rlang::sym("contrarian") > 0 ~ "contrarian",
				TRUE ~ "indefinite"
			))

	return(meanrev_portfolio) # return the updated portfolio

}
