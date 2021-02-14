#' @name realized_compute
#'
#' @title Realized estimation
#'
#' @description Realized estimation
#'
#' @param portfolio_quantity Numeric vector. The portfolio quantities of assets into the
#'   investor's portfolio.
#' @param portfolio_price Numeric vector. The portfolio prices of assets into the
#'   investor's portfolio.
#' @param transaction_quantity Numeric value. The quantity of the traded asset.
#' @param transaction_price Numeric value. The market price of the traded asset.
#' @param transaction_type Character string. Either "B" = buy or "S" = sell.
#' @param previous_transaction_datetime POSIXct value. The portfolio date-time related to the
#'   last transaction of the traded asset.
#' @param transaction_asset Character string. The name of the traded asset.
#' @param realized_only Logical. If TRUE only realized gains and realized
#'   losses are computed. Otherwise also paper gains and paper losses on excess
#'   quantity of the traded asset are computed.
#' @inheritParams paper_compute
#'
#' @return
#'   The described functions have different return behaviours.
#'
#'   \describe{
#'     \item{\code{realized_compute}}{returns a [tibble][tibble::tibble-package]
#'       containing the values of realized and paper gains and losses computed by
#'       means of the chosen method on each portfolio assets.}
#'     \item{\code{realized_count}}{returns a named vector containing the values
#'       of realized and paper gains and losses computed using the count method.}
#'     \item{\code{realized_total}}{returns a named vector containing the values
#'       of realized and paper gains and losses computed using the total method.}
#'     \item{\code{realized_value}}{returns a named vector containing the values
#'       of realized and paper gains and losses computed using the value method.}
#'     \item{\code{realized_duration}}{returns a named vector containing the values
#'       of realized and paper gains and losses computed using the duration method.}
#'     \item{\code{realized_empty}}{returns a named vector containing empty values
#'       of realized and paper gains and losses computed using the chosen method.}
#'   }
#'
#'   In particular:
#'
#'   \describe{
#'     \item{\code{RG_"method"}}{contains Realized Gains results.}
#'     \item{\code{RL_"method"}}{contains Realized Losses results.}
#'     \item{\code{PG_"method"}}{contains Paper Gains results.}
#'     \item{\code{PL_"method"}}{contains Paper Losses results.}
#'   }
#'
#' @author L. Mazzucchelli & M. Zanotti
#'
#' @references H. Shefrin & M. Statman, 1985
#'
#' @seealso \code{\link{paper_compute}}, \code{\link{gains_and_losses}}
NULL


#' @describeIn realized_compute Computation, as simple counts, of realized
#'   gains and realized losses of the traded asset.
#' @export
realized_count <- function(portfolio_quantity,
													 portfolio_price,
													 transaction_quantity,
													 transaction_price,
													 transaction_type,
													 allow_short = FALSE,
													 realized_only = FALSE) {

	qty_diff <- portfolio_quantity + transaction_quantity # quantity difference (if transaction_type == "S" then portfolio_quantity < 0)
	prz_diff <- transaction_price - portfolio_price # price difference


	if (realized_only) {


		if (allow_short) {



			if (portfolio_quantity > 0 && transaction_type == "B") { # Long + No Realized


				if (prz_diff > 0) { # Paper Gain
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
				} else if (prz_diff < 0) { # Paper Loss
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
				} else {# Nothing
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
				}


			} else if (portfolio_quantity > 0 && transaction_type == "S") { # Long - Realized


				if (prz_diff > 0) { # Realized Gain
					if (qty_diff > 0) { # + Paper Gain
						res <- c("RG_count" = 1, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
					} else {# + nothing
						res <- c("RG_count" = 1, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
					}
				} else if (prz_diff < 0) { # Realized Loss
					if (qty_diff > 0) { # + Paper Loss
						res <- c("RG_count" = 0, "RL_count" = 1, "PG_count" = 0, "PL_count" = 0)
					} else {# + nothing
						res <- c("RG_count" = 0, "RL_count" = 1, "PG_count" = 0, "PL_count" = 0)
					}
				} else {# Nothing
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
				}


			} else if (portfolio_quantity < 0 && transaction_type == "S") { # Short + No Realized


				if (prz_diff > 0) { # Paper Loss
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
				} else if (prz_diff < 0) { # Paper Gain
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
				} else {# Nothing
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
				}


			} else if (portfolio_quantity < 0 && transaction_type == "B") { # Short - Realized


				if (prz_diff > 0) { # Realized Loss
					if (qty_diff < 0) { # + Paper Loss
						res <- c("RG_count" = 0, "RL_count" = 1, "PG_count" = 0, "PL_count" = 0)
					} else {# + nothing
						res <- c("RG_count" = 0, "RL_count" = 1, "PG_count" = 0, "PL_count" = 0)
					}
				} else if (prz_diff < 0) { # Realized Gain
					if (qty_diff < 0) { # + Paper Gain
						res <- c("RG_count" = 1, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
					} else {# + nothing
						res <- c("RG_count" = 1, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
					}
				} else {# Nothing
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
				}


			} else {# if portfolio_quantity == 0, then nothing
				res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
			}



		} else {# allow_short = FALSE



			if (portfolio_quantity > 0 && transaction_type == "B") { # Long + No Realized


				if (prz_diff > 0) { # Paper Gain
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
				} else if (prz_diff < 0) { # Paper Loss
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
				} else {# Nothing
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
				}


			} else if (portfolio_quantity > 0 && transaction_type == "S") { # Long - Realized


				if (prz_diff > 0) { # Realized Gain
					if (qty_diff > 0) { # + Paper Gain
						res <- c("RG_count" = 1, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
					} else {# + nothing
						res <- c("RG_count" = 1, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
					}
				} else if (prz_diff < 0) { # Realized Loss
					if (qty_diff > 0) { # + Paper Loss
						res <- c("RG_count" = 0, "RL_count" = 1, "PG_count" = 0, "PL_count" = 0)
					} else {# + nothing
						res <- c("RG_count" = 0, "RL_count" = 1, "PG_count" = 0, "PL_count" = 0)
					}
				} else {# Nothing
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
				}


			} else {# if portfolio_quantity <= 0, then nothing
				res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
			}



		}



	} else {# not realized_only



		if (allow_short) {



			if (portfolio_quantity > 0 && transaction_type == "B") { # Long + No Realized


				if (prz_diff > 0) { # Paper Gain
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 1, "PL_count" = 0)
				} else if (prz_diff < 0) { # Paper Loss
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 1)
				} else {# Nothing
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
				}


			} else if (portfolio_quantity > 0 && transaction_type == "S") { # Long - Realized


				if (prz_diff > 0) { # Realized Gain
					if (qty_diff > 0) { # + Paper Gain
						res <- c("RG_count" = 1, "RL_count" = 0, "PG_count" = 1, "PL_count" = 0)
					} else {# + nothing
						res <- c("RG_count" = 1, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
					}
				} else if (prz_diff < 0) { # Realized Loss
					if (qty_diff > 0) { # + Paper Loss
						res <- c("RG_count" = 0, "RL_count" = 1, "PG_count" = 0, "PL_count" = 1)
					} else {# + nothing
						res <- c("RG_count" = 0, "RL_count" = 1, "PG_count" = 0, "PL_count" = 0)
					}
				} else {# Nothing
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
				}


			} else if (portfolio_quantity < 0 && transaction_type == "S") { # Short + No Realized


				if (prz_diff > 0) { # Paper Loss
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 1)
				} else if (prz_diff < 0) { # Paper Gain
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 1, "PL_count" = 0)
				} else {# Nothing
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
				}


			} else if (portfolio_quantity < 0 && transaction_type == "B") { # Short - Realized


				if (prz_diff > 0) { # Realized Loss
					if (qty_diff < 0) { # + Paper Loss
						res <- c("RG_count" = 0, "RL_count" = 1, "PG_count" = 0, "PL_count" = 1)
					} else {# + nothing
						res <- c("RG_count" = 0, "RL_count" = 1, "PG_count" = 0, "PL_count" = 0)
					}
				} else if (prz_diff < 0) { # Realized Gain
					if (qty_diff < 0) { # + Paper Gain
						res <- c("RG_count" = 1, "RL_count" = 0, "PG_count" = 1, "PL_count" = 0)
					} else {# + nothing
						res <- c("RG_count" = 1, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
					}
				} else {# Nothing
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
				}


			} else {# if portfolio_quantity == 0, then nothing
				res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
			}



		} else {# allow_short = FALSE



			if (portfolio_quantity > 0 && transaction_type == "B") { # Long + No Realized


				if (prz_diff > 0) { # Paper Gain
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 1, "PL_count" = 0)
				} else if (prz_diff < 0) { # Paper Loss
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 1)
				} else {# Nothing
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
				}


			} else if (portfolio_quantity > 0 && transaction_type == "S") { # Long - Realized


				if (prz_diff > 0) { # Realized Gain
					if (qty_diff > 0) { # + Paper Gain
						res <- c("RG_count" = 1, "RL_count" = 0, "PG_count" = 1, "PL_count" = 0)
					} else {# + nothing
						res <- c("RG_count" = 1, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
					}
				} else if (prz_diff < 0) { # Realized Loss
					if (qty_diff > 0) { # + Paper Loss
						res <- c("RG_count" = 0, "RL_count" = 1, "PG_count" = 0, "PL_count" = 1)
					} else {# + nothing
						res <- c("RG_count" = 0, "RL_count" = 1, "PG_count" = 0, "PL_count" = 0)
					}
				} else {# Nothing
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
				}


			} else {# if portfolio_quantity <= 0, then nothing
				res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
			}



		}



	}

	return(res)

}


#' @describeIn realized_compute Computation, as simple quantity, of realized
#'   gains and realized losses of the traded asset.
#' @export
realized_total <- function(portfolio_quantity,
													 portfolio_price,
													 transaction_quantity,
													 transaction_price,
													 transaction_type,
													 allow_short = FALSE,
													 realized_only = FALSE) {


	qty_diff <- portfolio_quantity + transaction_quantity # quantity difference (if transaction_type == "S" then portfolio_quantity < 0)
	prz_diff <- transaction_price - portfolio_price # price difference


	if (realized_only) {



		if (allow_short) {



			if (portfolio_quantity > 0 && transaction_type == "B") { # Long + No Realized


				if (prz_diff > 0) { # Paper Gain
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				} else if (prz_diff < 0) { # Paper Loss
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				} else {# Nothing
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				}


			} else if (portfolio_quantity > 0 && transaction_type == "S") { # Long - Realized


				if (prz_diff > 0) { # Realized Gain
					if (qty_diff > 0) { # + Paper Gain
						res <- c("RG_total" = abs(transaction_quantity), "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
					} else {# + nothing
						res <- c("RG_total" = abs(transaction_quantity) + qty_diff, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
					}
				} else if (prz_diff < 0) { # Realized Loss
					if (qty_diff > 0) { # + Paper Loss
						res <- c("RG_total" = 0, "RL_total" = abs(transaction_quantity), "PG_total" = 0, "PL_total" = 0)
					} else {# + nothing
						res <- c("RG_total" = 0, "RL_total" = abs(transaction_quantity) + qty_diff, "PG_total" = 0, "PL_total" = 0)
					}
				} else {# Nothing
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				}


			} else if (portfolio_quantity < 0 && transaction_type == "S") { # Short + No Realized


				if (prz_diff > 0) { # Paper Loss
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				} else if (prz_diff < 0) { # Paper Gain
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				} else {# Nothing
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				}


			} else if (portfolio_quantity < 0 && transaction_type == "B") { # Short - Realized


				if (prz_diff > 0) { # Realized Loss
					if (qty_diff < 0) { # + Paper Loss
						res <- c("RG_total" = 0, "RL_total" = transaction_quantity, "PG_total" = 0, "PL_total" = 0)
					} else {# + nothing
						res <- c("RG_total" = 0, "RL_total" = transaction_quantity - qty_diff, "PG_total" = 0, "PL_total" = 0)
					}
				} else if (prz_diff < 0) { # Realized Gain
					if (qty_diff < 0) { # + Paper Gain
						res <- c("RG_total" = transaction_quantity, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
					} else {# + nothing
						res <- c("RG_total" = transaction_quantity - qty_diff, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
					}
				} else {# Nothing
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				}


			} else {# if portfolio_quantity == 0, then nothing
				res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
			}



		} else {# allow_short = FALSE



			if (portfolio_quantity > 0 && transaction_type == "B") { # Long + No Realized


				if (prz_diff > 0) { # Paper Gain
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				} else if (prz_diff < 0) { # Paper Loss
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				} else {# Nothing
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				}


			} else if (portfolio_quantity > 0 && transaction_type == "S") { # Long - Realized


				if (prz_diff > 0) { # Realized Gain
					if (qty_diff > 0) { # + Paper Gain
						res <- c("RG_total" = abs(transaction_quantity), "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
					} else {# + nothing
						res <- c("RG_total" = abs(transaction_quantity) + qty_diff, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
					}
				} else if (prz_diff < 0) { # Realized Loss
					if (qty_diff > 0) { # + Paper Loss
						res <- c("RG_total" = 0, "RL_total" = abs(transaction_quantity), "PG_total" = 0, "PL_total" = 0)
					} else {# + nothing
						res <- c("RG_total" = 0, "RL_total" = abs(transaction_quantity) + qty_diff, "PG_total" = 0, "PL_total" = 0)
					}
				} else {# Nothing
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				}


			} else {# if portfolio_quantity <= 0, then nothing
				res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
			}



		}



	} else {# not realized_olny



		if (allow_short) {



			if (portfolio_quantity > 0 && transaction_type == "B") { # Long + No Realized


				if (prz_diff > 0) { # Paper Gain
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = portfolio_quantity, "PL_total" = 0)
				} else if (prz_diff < 0) { # Paper Loss
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = portfolio_quantity)
				} else {# Nothing
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				}


			} else if (portfolio_quantity > 0 && transaction_type == "S") { # Long - Realized


				if (prz_diff > 0) { # Realized Gain
					if (qty_diff > 0) { # + Paper Gain
						res <- c("RG_total" = abs(transaction_quantity), "RL_total" = 0, "PG_total" = qty_diff, "PL_total" = 0)
					} else {# + nothing
						res <- c("RG_total" = abs(transaction_quantity) + qty_diff, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
					}
				} else if (prz_diff < 0) { # Realized Loss
					if (qty_diff > 0) { # + Paper Loss
						res <- c("RG_total" = 0, "RL_total" = abs(transaction_quantity), "PG_total" = 0, "PL_total" = qty_diff)
					} else {# + nothing
						res <- c("RG_total" = 0, "RL_total" = abs(transaction_quantity) + qty_diff, "PG_total" = 0, "PL_total" = 0)
					}
				} else {# Nothing
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				}


			} else if (portfolio_quantity < 0 && transaction_type == "S") { # Short + No Realized


				if (prz_diff > 0) { # Paper Loss
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = abs(portfolio_quantity))
				} else if (prz_diff < 0) { # Paper Gain
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = abs(portfolio_quantity), "PL_total" = 0)
				} else {# Nothing
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				}


			} else if (portfolio_quantity < 0 && transaction_type == "B") { # Short - Realized


				if (prz_diff > 0) { # Realized Loss
					if (qty_diff < 0) { # + Paper Loss
						res <- c("RG_total" = 0, "RL_total" = transaction_quantity, "PG_total" = 0, "PL_total" = abs(qty_diff))
					} else {# + nothing
						res <- c("RG_total" = 0, "RL_total" = transaction_quantity - qty_diff, "PG_total" = 0, "PL_total" = 0)
					}
				} else if (prz_diff < 0) { # Realized Gain
					if (qty_diff < 0) { # + Paper Gain
						res <- c("RG_total" = transaction_quantity, "RL_total" = 0, "PG_total" = abs(qty_diff), "PL_total" = 0)
					} else {# + nothing
						res <- c("RG_total" = transaction_quantity - qty_diff, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
					}
				} else {# Nothing
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				}


			} else {# if portfolio_quantity == 0, then nothing
				res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
			}



		} else {# allow_short = FALSE



			if (portfolio_quantity > 0 && transaction_type == "B") { # Long + No Realized


				if (prz_diff > 0) { # Paper Gain
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = portfolio_quantity, "PL_total" = 0)
				} else if (prz_diff < 0) { # Paper Loss
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = portfolio_quantity)
				} else {# Nothing
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				}


			} else if (portfolio_quantity > 0 && transaction_type == "S") { # Long - Realized


				if (prz_diff > 0) { # Realized Gain
					if (qty_diff > 0) { # + Paper Gain
						res <- c("RG_total" = abs(transaction_quantity), "RL_total" = 0, "PG_total" = qty_diff, "PL_total" = 0)
					} else {# + nothing
						res <- c("RG_total" = abs(transaction_quantity) + qty_diff, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
					}
				} else if (prz_diff < 0) { # Realized Loss
					if (qty_diff > 0) { # + Paper Loss
						res <- c("RG_total" = 0, "RL_total" = abs(transaction_quantity), "PG_total" = 0, "PL_total" = qty_diff)
					} else {# + nothing
						res <- c("RG_total" = 0, "RL_total" = abs(transaction_quantity) + qty_diff, "PG_total" = 0, "PL_total" = 0)
					}
				} else {# Nothing
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				}


			} else {# if portfolio_quantity <= 0, then nothing
				res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
			}



		}



	}

	return(res)

}


#' @describeIn realized_compute Computation, as simple expected return, of
#'   realized gains and realized losses of the traded asset.
#' @export
realized_value <- function(portfolio_quantity,
													 portfolio_price,
													 transaction_quantity,
													 transaction_price,
													 transaction_type,
													 allow_short = FALSE, realized_only = FALSE) {

	qty_diff <- portfolio_quantity + transaction_quantity # quantity difference (if transaction_type == "S" then transaction_quantity < 0)
	prz_diff <- transaction_price - portfolio_price # price difference
	Er <- prz_diff / portfolio_price # transaction_asset expected return


	if (realized_only) {



		if (allow_short) {



			if (portfolio_quantity > 0 && transaction_type == "B") { # Long + No Realized


				if (prz_diff > 0) { # Paper Gain
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
				} else if (prz_diff < 0) { # Paper Loss
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
				} else {# Nothing
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
				}


			} else if (portfolio_quantity > 0 && transaction_type == "S") { # Long - Realized


				if (prz_diff > 0) { # Realized Gain
					if (qty_diff > 0) { # + Paper Gain
						res <- c("RG_value" = Er, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
					} else {# + nothing
						res <- c("RG_value" = Er, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
					}
				} else if (prz_diff < 0) { # Realized Loss
					if (qty_diff > 0) { # + Paper Loss
						res <- c("RG_value" = 0, "RL_value" = Er, "PG_value" = 0, "PL_value" = 0)
					} else {# + nothing
						res <- c("RG_value" = 0, "RL_value" = Er, "PG_value" = 0, "PL_value" = 0)
					}
				} else {# Nothing
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
				}


			} else if (portfolio_quantity < 0 && transaction_type == "S") { # Short + No Realized


				if (prz_diff > 0) { # Paper Loss
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
				} else if (prz_diff < 0) { # Paper Gain
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
				} else {# Nothing
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
				}


			} else if (portfolio_quantity < 0 && transaction_type == "B") { # Short - Realized


				if (prz_diff > 0) { # Realized Loss
					if (qty_diff < 0) { # + Paper Loss
						res <- c("RG_value" = 0, "RL_value" = Er, "PG_value" = 0, "PL_value" = 0)
					} else {# + nothing
						res <- c("RG_value" = 0, "RL_value" = Er, "PG_value" = 0, "PL_value" = 0)
					}
				} else if (prz_diff < 0) { # Realized Gain
					if (qty_diff < 0) { # + Paper Gain
						res <- c("RG_value" = -Er, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
					} else {# + nothing
						res <- c("RG_value" = -Er, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
					}
				} else {# Nothing
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
				}


			} else {# if portfolio_quantity == 0, then nothing
				res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
			}



		} else {# allow_short = FALSE



			if (portfolio_quantity > 0 && transaction_type == "B") { # Long + No Realized


				if (prz_diff > 0) { # Paper Gain
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
				} else if (prz_diff < 0) { # Paper Loss
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
				} else {# Nothing
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
				}


			} else if (portfolio_quantity > 0 && transaction_type == "S") { # Long - Realized


				if (prz_diff > 0) { # Realized Gain
					if (qty_diff > 0) { # + Paper Gain
						res <- c("RG_value" = Er, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
					} else {# + nothing
						res <- c("RG_value" = Er, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
					}
				} else if (prz_diff < 0) { # Realized Loss
					if (qty_diff > 0) { # + Paper Loss
						res <- c("RG_value" = 0, "RL_value" = Er, "PG_value" = 0, "PL_value" = 0)
					} else {# + nothing
						res <- c("RG_value" = 0, "RL_value" = Er, "PG_value" = 0, "PL_value" = 0)
					}
				} else {# Nothing
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
				}


			} else {# if portfolio_quantity <= 0, then nothing
				res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
			}



		}



	} else {# not realized_only



		if (allow_short) {



			if (portfolio_quantity > 0 && transaction_type == "B") { # Long + No Realized


				if (prz_diff > 0) { # Paper Gain
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = Er, "PL_value" = 0)
				} else if (prz_diff < 0) { # Paper Loss
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = Er)
				} else {# Nothing
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
				}


			} else if (portfolio_quantity > 0 && transaction_type == "S") { # Long - Realized


				if (prz_diff > 0) { # Realized Gain
					if (qty_diff > 0) { # + Paper Gain
						res <- c("RG_value" = Er, "RL_value" = 0, "PG_value" = Er, "PL_value" = 0)
					} else {# + nothing
						res <- c("RG_value" = Er, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
					}
				} else if (prz_diff < 0) { # Realized Loss
					if (qty_diff > 0) { # + Paper Loss
						res <- c("RG_value" = 0, "RL_value" = Er, "PG_value" = 0, "PL_value" = Er)
					} else {# + nothing
						res <- c("RG_value" = 0, "RL_value" = Er, "PG_value" = 0, "PL_value" = 0)
					}
				} else {# Nothing
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
				}


			} else if (portfolio_quantity < 0 && transaction_type == "S") { # Short + No Realized


				if (prz_diff > 0) { # Paper Loss
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = Er)
				} else if (prz_diff < 0) { # Paper Gain
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = -Er, "PL_value" = 0)
				} else {# Nothing
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
				}


			} else if (portfolio_quantity < 0 && transaction_type == "B") { # Short - Realized


				if (prz_diff > 0) { # Realized Loss
					if (qty_diff < 0) { # + Paper Loss
						res <- c("RG_value" = 0, "RL_value" = Er, "PG_value" = 0, "PL_value" = Er)
					} else {# + nothing
						res <- c("RG_value" = 0, "RL_value" = Er, "PG_value" = 0, "PL_value" = 0)
					}
				} else if (prz_diff < 0) { # Realized Gain
					if (qty_diff < 0) { # + Paper Gain
						res <- c("RG_value" = -Er, "RL_value" = 0, "PG_value" = -Er, "PL_value" = 0)
					} else {# + nothing
						res <- c("RG_value" = -Er, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
					}
				} else {# Nothing
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
				}


			} else {# if portfolio_quantity == 0, then nothing
				res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
			}



		} else {# allow_short = FALSE



			if (portfolio_quantity > 0 && transaction_type == "B") { # Long + No Realized


				if (prz_diff > 0) { # Paper Gain
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = Er, "PL_value" = 0)
				} else if (prz_diff < 0) { # Paper Loss
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = Er)
				} else {# Nothing
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
				}


			} else if (portfolio_quantity > 0 && transaction_type == "S") { # Long - Realized


				if (prz_diff > 0) { # Realized Gain
					if (qty_diff > 0) { # + Paper Gain
						res <- c("RG_value" = Er, "RL_value" = 0, "PG_value" = Er, "PL_value" = 0)
					} else {# + nothing
						res <- c("RG_value" = Er, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
					}
				} else if (prz_diff < 0) { # Realized Loss
					if (qty_diff > 0) { # + Paper Loss
						res <- c("RG_value" = 0, "RL_value" = Er, "PG_value" = 0, "PL_value" = Er)
					} else {# + nothing
						res <- c("RG_value" = 0, "RL_value" = Er, "PG_value" = 0, "PL_value" = 0)
					}
				} else {# Nothing
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
				}


			} else {# if portfolio_quantity <= 0, then nothing
				res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
			}



		}



	}

	return(res)

}


#' @describeIn realized_compute Computation, as simple financial duration, of
#'   realized gains and realized losses of the traded asset.
#' @export
realized_duration <- function(portfolio_quantity,
															portfolio_price,
															transaction_quantity,
															transaction_price,
															transaction_type,
															previous_transaction_datetime,
															previous_datetime,
															transaction_datetime,
															allow_short = FALSE,
															realized_only = FALSE) {

	qty_diff <- portfolio_quantity + transaction_quantity # quantity difference (if transaction_type == "S" then transaction_quantity < 0)
	prz_diff <- transaction_price - portfolio_price # price difference
	dtt_diff <- difftime_financial(previous_datetime, transaction_datetime)
	dtt_diff0 <- difftime_financial(previous_transaction_datetime, transaction_datetime)


	if (realized_only) {



		if (allow_short) {



			if (portfolio_quantity > 0 && transaction_type == "B") { # Long + No Realized


				if (prz_diff > 0) { # Paper Gain
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
				} else if (prz_diff < 0) { # Paper Loss
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
				} else {# Nothing
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
				}


			} else if (portfolio_quantity > 0 && transaction_type == "S") { # Long - Realized


				if (prz_diff > 0) { # Realized Gain
					if (qty_diff > 0) { # + Paper Gain
						res <- c("RG_duration" = dtt_diff0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
					} else {# + nothing
						res <- c("RG_duration" = dtt_diff0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
					}
				} else if (prz_diff < 0) { # Realized Loss
					if (qty_diff > 0) { # + Paper Loss
						res <- c("RG_duration" = 0, "RL_duration" = dtt_diff0, "PG_duration" = 0, "PL_duration" = 0)
					} else {# + nothing
						res <- c("RG_duration" = 0, "RL_duration" = dtt_diff0, "PG_duration" = 0, "PL_duration" = 0)
					}
				} else {# Nothing
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
				}


			} else if (portfolio_quantity < 0 && transaction_type == "S") { # Short + No Realized


				if (prz_diff > 0) { # Paper Loss
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
				} else if (prz_diff < 0) { # Paper Gain
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
				} else {# Nothing
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
				}


			} else if (portfolio_quantity < 0 && transaction_type == "B") { # Short - Realized


				if (prz_diff > 0) { # Realized Loss
					if (qty_diff < 0) { # + Paper Loss
						res <- c("RG_duration" = 0, "RL_duration" = dtt_diff0, "PG_duration" = 0, "PL_duration" = 0)
					} else {# + nothing
						res <- c("RG_duration" = 0, "RL_duration" = dtt_diff0, "PG_duration" = 0, "PL_duration" = 0)
					}
				} else if (prz_diff < 0) { # Realized Gain
					if (qty_diff < 0) { # + Paper Gain
						res <- c("RG_duration" = dtt_diff0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
					} else {# + nothing
						res <- c("RG_duration" = dtt_diff0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
					}
				} else {# Nothing
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
				}


			} else {# if portfolio_quantity == 0, then nothing
				res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
			}



		} else {# allow_short = FALSE



			if (portfolio_quantity > 0 && transaction_type == "B") { # Long + No Realized


				if (prz_diff > 0) { # Paper Gain
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
				} else if (prz_diff < 0) { # Paper Loss
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
				} else {# Nothing
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
				}


			} else if (portfolio_quantity > 0 && transaction_type == "S") { # Long - Realized


				if (prz_diff > 0) { # Realized Gain
					if (qty_diff > 0) { # + Paper Gain
						res <- c("RG_duration" = dtt_diff0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
					} else {# + nothing
						res <- c("RG_duration" = dtt_diff0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
					}
				} else if (prz_diff < 0) { # Realized Loss
					if (qty_diff > 0) { # + Paper Loss
						res <- c("RG_duration" = 0, "RL_duration" = dtt_diff0, "PG_duration" = 0, "PL_duration" = 0)
					} else {# + nothing
						res <- c("RG_duration" = 0, "RL_duration" = dtt_diff0, "PG_duration" = 0, "PL_duration" = 0)
					}
				} else {# Nothing
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
				}


			} else {# if portfolio_quantity <= 0, then nothing
				res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
			}



		}



	} else {# not realized_only



		if (allow_short) {



			if (portfolio_quantity > 0 && transaction_type == "B") { # Long + No Realized


				if (prz_diff > 0) { # Paper Gain
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = dtt_diff, "PL_duration" = 0)
				} else if (prz_diff < 0) { # Paper Loss
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = dtt_diff)
				} else {# Nothing
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
				}


			} else if (portfolio_quantity > 0 && transaction_type == "S") { # Long - Realized


				if (prz_diff > 0) { # Realized Gain
					if (qty_diff > 0) { # + Paper Gain
						res <- c("RG_duration" = dtt_diff0, "RL_duration" = 0, "PG_duration" = dtt_diff, "PL_duration" = 0)
					} else {# + nothing
						res <- c("RG_duration" = dtt_diff0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
					}
				} else if (prz_diff < 0) { # Realized Loss
					if (qty_diff > 0) { # + Paper Loss
						res <- c("RG_duration" = 0, "RL_duration" = dtt_diff0, "PG_duration" = 0, "PL_duration" = dtt_diff)
					} else {# + nothing
						res <- c("RG_duration" = 0, "RL_duration" = dtt_diff0, "PG_duration" = 0, "PL_duration" = 0)
					}
				} else {# Nothing
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
				}


			} else if (portfolio_quantity < 0 && transaction_type == "S") { # Short + No Realized


				if (prz_diff > 0) { # Paper Loss
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = dtt_diff)
				} else if (prz_diff < 0) { # Paper Gain
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = dtt_diff, "PL_duration" = 0)
				} else {# Nothing
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
				}


			} else if (portfolio_quantity < 0 && transaction_type == "B") { # Short - Realized


				if (prz_diff > 0) { # Realized Loss
					if (qty_diff < 0) { # + Paper Loss
						res <- c("RG_duration" = 0, "RL_duration" = dtt_diff0, "PG_duration" = 0, "PL_duration" = dtt_diff)
					} else {# + nothing
						res <- c("RG_duration" = 0, "RL_duration" = dtt_diff0, "PG_duration" = 0, "PL_duration" = 0)
					}
				} else if (prz_diff < 0) { # Realized Gain
					if (qty_diff < 0) { # + Paper Gain
						res <- c("RG_duration" = dtt_diff0, "RL_duration" = 0, "PG_duration" = dtt_diff, "PL_duration" = 0)
					} else {# + nothing
						res <- c("RG_duration" = dtt_diff0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
					}
				} else {# Nothing
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
				}


			} else {# if portfolio_quantity == 0, then nothing
				res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
			}



		} else {# allow_short = FALSE



			if (portfolio_quantity > 0 && transaction_type == "B") { # Long + No Realized


				if (prz_diff > 0) { # Paper Gain
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = dtt_diff, "PL_duration" = 0)
				} else if (prz_diff < 0) { # Paper Loss
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = dtt_diff)
				} else {# Nothing
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
				}


			} else if (portfolio_quantity > 0 && transaction_type == "S") { # Long - Realized


				if (prz_diff > 0) { # Realized Gain
					if (qty_diff > 0) { # + Paper Gain
						res <- c("RG_duration" = dtt_diff0, "RL_duration" = 0, "PG_duration" = dtt_diff, "PL_duration" = 0)
					} else {# + nothing
						res <- c("RG_duration" = dtt_diff0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
					}
				} else if (prz_diff < 0) { # Realized Loss
					if (qty_diff > 0) { # + Paper Loss
						res <- c("RG_duration" = 0, "RL_duration" = dtt_diff0, "PG_duration" = 0, "PL_duration" = dtt_diff)
					} else {# + nothing
						res <- c("RG_duration" = 0, "RL_duration" = dtt_diff0, "PG_duration" = 0, "PL_duration" = 0)
					}
				} else {# Nothing
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
				}


			} else {# if portfolio_quantity <= 0, then nothing
				res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
			}



		}



	}

	return(res)

}


#' @describeIn realized_compute Wrapper that calls other realized_. functions to
#'   compute realized gains and realized losses of the traded asset based on the
#'   chosen method.
#' @export
realized_compute <- function(portfolio_quantity,
														 portfolio_price,
														 transaction_quantity,
														 transaction_price,
														 transaction_type,
														 previous_transaction_datetime,
														 previous_datetime,
														 transaction_datetime,
														 transaction_asset,
														 allow_short = FALSE,
														 realized_only = FALSE,
														 method = "all") {

	if (method == "count") {

		rgl_count <- realized_count(
			portfolio_quantity,
			portfolio_price,
			transaction_quantity,
			transaction_price,
			transaction_type,
			allow_short,
			realized_only
		)
		res_df <- tibble::as_tibble(c("asset" = transaction_asset, as.list(rgl_count)))

	} else if (method == "total") {

		rgl_total <- realized_total(
			portfolio_quantity,
			portfolio_price,
			transaction_quantity,
			transaction_price,
			transaction_type,
			allow_short,
			realized_only
		)
		res_df <- tibble::as_tibble(c("asset" = transaction_asset, as.list(rgl_total)))

	} else if (method == "value") {

		rgl_value <- realized_value(
			portfolio_quantity,
			portfolio_price,
			transaction_quantity,
			transaction_price,
			transaction_type,
			allow_short,
			realized_only
		)
		res_df <- tibble::as_tibble(c("asset" = transaction_asset, as.list(rgl_value)))

	} else if (method == "duration") {

		rgl_duration <- realized_duration(
			portfolio_quantity,
			portfolio_price,
			transaction_quantity,
			transaction_price,
			transaction_type,
			previous_transaction_datetime,
			previous_datetime,
			transaction_datetime,
			allow_short,
			realized_only
		)
		res_df <- tibble::as_tibble(c("asset" = transaction_asset, as.list(rgl_duration)))

	} else {# method == "all"

		rgl_count <- realized_count(
			portfolio_quantity,
			portfolio_price,
			transaction_quantity,
			transaction_price,
			transaction_type,
			allow_short,
			realized_only
		)
		rgl_total <- realized_total(
			portfolio_quantity,
			portfolio_price,
			transaction_quantity,
			transaction_price,
			transaction_type,
			allow_short,
			realized_only
		)
		rgl_value <- realized_value(
			portfolio_quantity,
			portfolio_price,
			transaction_quantity,
			transaction_price,
			transaction_type,
			allow_short,
			realized_only
		)
		rgl_duration <- realized_duration(
			portfolio_quantity,
			portfolio_price,
			transaction_quantity,
			transaction_price,
			transaction_type,
			previous_transaction_datetime,
			previous_datetime,
			transaction_datetime,
			allow_short,
			realized_only
		)
		res_df <- tibble::as_tibble(
			c("asset" = transaction_asset,
			  as.list(rgl_count),
			  as.list(rgl_total),
			  as.list(rgl_value),
			  as.list(rgl_duration)
			)
		)

	}

	return(res_df)

}


#' @describeIn realized_compute Simple function to obtain empty results for
#'   realized and paper computations based on the chosen method.
#' @export
realized_empty <- function(transaction_asset, method = "all") {

	if (method == "count") {

		rgl_count <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
		res_df <- tibble::as_tibble(c("asset" = transaction_asset, as.list(rgl_count)))

	} else if (method == "total") {

		rgl_total <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
		res_df <- tibble::as_tibble(c("asset" = transaction_asset, as.list(rgl_total)))

	} else if (method == "value") {

		rgl_value <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
		res_df <- tibble::as_tibble(c("asset" = transaction_asset, as.list(rgl_value)))

	} else if (method == "duration") {

		rgl_duration <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
		res_df <- tibble::as_tibble(c("asset" = transaction_asset, as.list(rgl_duration)))

	} else {# method == "all"

		rgl_count <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
		rgl_total <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
		rgl_value <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
		rgl_duration <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
		res_df <- tibble::as_tibble(
			c("asset" = transaction_asset,
				as.list(rgl_count),
				as.list(rgl_total),
			  as.list(rgl_value),
			  as.list(rgl_duration)
			)
		)

	}

	return(res_df)

}

