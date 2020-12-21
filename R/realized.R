#' @name realized_compute
#'
#' @title Realized estimation
#'
#' @description Realized estimation
#'
#' @details
#'
#' @param ptf_qty Numeric vector. The portfolio quantities of assets into the
#'   investor's portfolio.
#' @param ptf_prz Numeric vector. The portfolio prices of assets into the
#'   investor's portfolio.
#' @param trx_qty Numeric value. The quantity of the traded asset.
#' @param trx_prz Numeric value. The market price of the traded asset.
#' @param trx_type Character string. Either "B" = buy or "S" = sell.
#' @param initial_dtt POSIXct value. The portfolio date-time related to the
#'   last transaction of the traded asset.
#' @param trx_asset Character string. The name of the traded asset.
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
realized_count <- function(ptf_qty, ptf_prz, trx_qty, trx_prz, trx_type,
													 allow_short = FALSE, realized_only = FALSE) {

	if (!is.numeric(ptf_qty) || !is.numeric(ptf_prz) ||
			!is.numeric(trx_qty) || !is.numeric(trx_prz)) {
		stop("Arguments *qty and *prz must be numeric.", call. = FALSE)
	}

	qty_diff <- ptf_qty + trx_qty # quantity difference (if trx_type == "S" then ptf_qty < 0)
	prz_diff <- trx_prz - ptf_prz # price difference


	if (realized_only) {


		if (allow_short) {



			if (ptf_qty > 0 && trx_type == "B") { # Long + No Realized


				if (prz_diff > 0) { # Paper Gain
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
				} else if (prz_diff < 0) { # Paper Loss
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
				} else {# Nothing
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
				}


			} else if (ptf_qty > 0 && trx_type == "S") { # Long - Realized


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


			} else if (ptf_qty < 0 && trx_type == "S") { # Short + No Realized


				if (prz_diff > 0) { # Paper Loss
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
				} else if (prz_diff < 0) { # Paper Gain
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
				} else {# Nothing
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
				}


			} else if (ptf_qty < 0 && trx_type == "B") { # Short - Realized


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


			} else {# if ptf_qty == 0, then nothing
				res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
			}



		} else {# allow_short = FALSE



			if (ptf_qty > 0 && trx_type == "B") { # Long + No Realized


				if (prz_diff > 0) { # Paper Gain
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
				} else if (prz_diff < 0) { # Paper Loss
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
				} else {# Nothing
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
				}


			} else if (ptf_qty > 0 && trx_type == "S") { # Long - Realized


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


			} else {# if ptf_qty <= 0, then nothing
				res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
			}



		}



	} else {# not realized_only



		if (allow_short) {



			if (ptf_qty > 0 && trx_type == "B") { # Long + No Realized


				if (prz_diff > 0) { # Paper Gain
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 1, "PL_count" = 0)
				} else if (prz_diff < 0) { # Paper Loss
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 1)
				} else {# Nothing
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
				}


			} else if (ptf_qty > 0 && trx_type == "S") { # Long - Realized


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


			} else if (ptf_qty < 0 && trx_type == "S") { # Short + No Realized


				if (prz_diff > 0) { # Paper Loss
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 1)
				} else if (prz_diff < 0) { # Paper Gain
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 1, "PL_count" = 0)
				} else {# Nothing
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
				}


			} else if (ptf_qty < 0 && trx_type == "B") { # Short - Realized


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


			} else {# if ptf_qty == 0, then nothing
				res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
			}



		} else {# allow_short = FALSE



			if (ptf_qty > 0 && trx_type == "B") { # Long + No Realized


				if (prz_diff > 0) { # Paper Gain
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 1, "PL_count" = 0)
				} else if (prz_diff < 0) { # Paper Loss
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 1)
				} else {# Nothing
					res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
				}


			} else if (ptf_qty > 0 && trx_type == "S") { # Long - Realized


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


			} else {# if ptf_qty <= 0, then nothing
				res <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
			}



		}



	}

	return(res)

}


#' @describeIn realized_compute Computation, as simple quantity, of realized
#'   gains and realized losses of the traded asset.
#' @export
realized_total <- function(ptf_qty, ptf_prz, trx_qty, trx_prz, trx_type,
													 allow_short = FALSE, realized_only = FALSE) {

	if (!is.numeric(ptf_qty) || !is.numeric(ptf_prz) ||
			!is.numeric(trx_qty) || !is.numeric(trx_prz)) {
		stop("Arguments *qty and *prz must be numeric.", call. = FALSE)
	}

	qty_diff <- ptf_qty + trx_qty # quantity difference (if trx_type == "S" then ptf_qty < 0)
	prz_diff <- trx_prz - ptf_prz # price difference


	if (realized_only) {



		if (allow_short) {



			if (ptf_qty > 0 && trx_type == "B") { # Long + No Realized


				if (prz_diff > 0) { # Paper Gain
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				} else if (prz_diff < 0) { # Paper Loss
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				} else {# Nothing
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				}


			} else if (ptf_qty > 0 && trx_type == "S") { # Long - Realized


				if (prz_diff > 0) { # Realized Gain
					if (qty_diff > 0) { # + Paper Gain
						res <- c("RG_total" = abs(trx_qty), "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
					} else {# + nothing
						res <- c("RG_total" = abs(trx_qty) + qty_diff, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
					}
				} else if (prz_diff < 0) { # Realized Loss
					if (qty_diff > 0) { # + Paper Loss
						res <- c("RG_total" = 0, "RL_total" = abs(trx_qty), "PG_total" = 0, "PL_total" = 0)
					} else {# + nothing
						res <- c("RG_total" = 0, "RL_total" = abs(trx_qty) + qty_diff, "PG_total" = 0, "PL_total" = 0)
					}
				} else {# Nothing
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				}


			} else if (ptf_qty < 0 && trx_type == "S") { # Short + No Realized


				if (prz_diff > 0) { # Paper Loss
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				} else if (prz_diff < 0) { # Paper Gain
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				} else {# Nothing
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				}


			} else if (ptf_qty < 0 && trx_type == "B") { # Short - Realized


				if (prz_diff > 0) { # Realized Loss
					if (qty_diff < 0) { # + Paper Loss
						res <- c("RG_total" = 0, "RL_total" = trx_qty, "PG_total" = 0, "PL_total" = 0)
					} else {# + nothing
						res <- c("RG_total" = 0, "RL_total" = trx_qty - qty_diff, "PG_total" = 0, "PL_total" = 0)
					}
				} else if (prz_diff < 0) { # Realized Gain
					if (qty_diff < 0) { # + Paper Gain
						res <- c("RG_total" = trx_qty, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
					} else {# + nothing
						res <- c("RG_total" = trx_qty - qty_diff, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
					}
				} else {# Nothing
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				}


			} else {# if ptf_qty == 0, then nothing
				res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
			}



		} else {# allow_short = FALSE



			if (ptf_qty > 0 && trx_type == "B") { # Long + No Realized


				if (prz_diff > 0) { # Paper Gain
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				} else if (prz_diff < 0) { # Paper Loss
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				} else {# Nothing
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				}


			} else if (ptf_qty > 0 && trx_type == "S") { # Long - Realized


				if (prz_diff > 0) { # Realized Gain
					if (qty_diff > 0) { # + Paper Gain
						res <- c("RG_total" = abs(trx_qty), "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
					} else {# + nothing
						res <- c("RG_total" = abs(trx_qty) + qty_diff, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
					}
				} else if (prz_diff < 0) { # Realized Loss
					if (qty_diff > 0) { # + Paper Loss
						res <- c("RG_total" = 0, "RL_total" = abs(trx_qty), "PG_total" = 0, "PL_total" = 0)
					} else {# + nothing
						res <- c("RG_total" = 0, "RL_total" = abs(trx_qty) + qty_diff, "PG_total" = 0, "PL_total" = 0)
					}
				} else {# Nothing
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				}


			} else {# if ptf_qty <= 0, then nothing
				res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
			}



		}



	} else {# not realized_olny



		if (allow_short) {



			if (ptf_qty > 0 && trx_type == "B") { # Long + No Realized


				if (prz_diff > 0) { # Paper Gain
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = ptf_qty, "PL_total" = 0)
				} else if (prz_diff < 0) { # Paper Loss
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = ptf_qty)
				} else {# Nothing
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				}


			} else if (ptf_qty > 0 && trx_type == "S") { # Long - Realized


				if (prz_diff > 0) { # Realized Gain
					if (qty_diff > 0) { # + Paper Gain
						res <- c("RG_total" = abs(trx_qty), "RL_total" = 0, "PG_total" = qty_diff, "PL_total" = 0)
					} else {# + nothing
						res <- c("RG_total" = abs(trx_qty) + qty_diff, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
					}
				} else if (prz_diff < 0) { # Realized Loss
					if (qty_diff > 0) { # + Paper Loss
						res <- c("RG_total" = 0, "RL_total" = abs(trx_qty), "PG_total" = 0, "PL_total" = qty_diff)
					} else {# + nothing
						res <- c("RG_total" = 0, "RL_total" = abs(trx_qty) + qty_diff, "PG_total" = 0, "PL_total" = 0)
					}
				} else {# Nothing
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				}


			} else if (ptf_qty < 0 && trx_type == "S") { # Short + No Realized


				if (prz_diff > 0) { # Paper Loss
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = abs(ptf_qty))
				} else if (prz_diff < 0) { # Paper Gain
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = abs(ptf_qty), "PL_total" = 0)
				} else {# Nothing
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				}


			} else if (ptf_qty < 0 && trx_type == "B") { # Short - Realized


				if (prz_diff > 0) { # Realized Loss
					if (qty_diff < 0) { # + Paper Loss
						res <- c("RG_total" = 0, "RL_total" = trx_qty, "PG_total" = 0, "PL_total" = abs(qty_diff))
					} else {# + nothing
						res <- c("RG_total" = 0, "RL_total" = trx_qty - qty_diff, "PG_total" = 0, "PL_total" = 0)
					}
				} else if (prz_diff < 0) { # Realized Gain
					if (qty_diff < 0) { # + Paper Gain
						res <- c("RG_total" = trx_qty, "RL_total" = 0, "PG_total" = abs(qty_diff), "PL_total" = 0)
					} else {# + nothing
						res <- c("RG_total" = trx_qty - qty_diff, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
					}
				} else {# Nothing
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				}


			} else {# if ptf_qty == 0, then nothing
				res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
			}



		} else {# allow_short = FALSE



			if (ptf_qty > 0 && trx_type == "B") { # Long + No Realized


				if (prz_diff > 0) { # Paper Gain
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = ptf_qty, "PL_total" = 0)
				} else if (prz_diff < 0) { # Paper Loss
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = ptf_qty)
				} else {# Nothing
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				}


			} else if (ptf_qty > 0 && trx_type == "S") { # Long - Realized


				if (prz_diff > 0) { # Realized Gain
					if (qty_diff > 0) { # + Paper Gain
						res <- c("RG_total" = abs(trx_qty), "RL_total" = 0, "PG_total" = qty_diff, "PL_total" = 0)
					} else {# + nothing
						res <- c("RG_total" = abs(trx_qty) + qty_diff, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
					}
				} else if (prz_diff < 0) { # Realized Loss
					if (qty_diff > 0) { # + Paper Loss
						res <- c("RG_total" = 0, "RL_total" = abs(trx_qty), "PG_total" = 0, "PL_total" = qty_diff)
					} else {# + nothing
						res <- c("RG_total" = 0, "RL_total" = abs(trx_qty) + qty_diff, "PG_total" = 0, "PL_total" = 0)
					}
				} else {# Nothing
					res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
				}


			} else {# if ptf_qty <= 0, then nothing
				res <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
			}



		}



	}

	return(res)

}


#' @describeIn realized_compute Computation, as simple expected return, of
#'   realized gains and realized losses of the traded asset.
#' @export
realized_value <- function(ptf_qty, ptf_prz, trx_qty, trx_prz, trx_type,
													 allow_short = FALSE, realized_only = FALSE) {

	if (!is.numeric(ptf_qty) || !is.numeric(ptf_prz) ||
			!is.numeric(trx_qty) || !is.numeric(trx_prz)) {
		stop("Arguments *qty and *prz must be numeric.", call. = FALSE)
	}

	qty_diff <- ptf_qty + trx_qty # quantity difference (if trx_type == "S" then trx_qty < 0)
	prz_diff <- trx_prz - ptf_prz # price difference
	Er <- prz_diff / ptf_prz # trx_asset expected return


	if (realized_only) {



		if (allow_short) {



			if (ptf_qty > 0 && trx_type == "B") { # Long + No Realized


				if (prz_diff > 0) { # Paper Gain
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
				} else if (prz_diff < 0) { # Paper Loss
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
				} else {# Nothing
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
				}


			} else if (ptf_qty > 0 && trx_type == "S") { # Long - Realized


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


			} else if (ptf_qty < 0 && trx_type == "S") { # Short + No Realized


				if (prz_diff > 0) { # Paper Loss
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
				} else if (prz_diff < 0) { # Paper Gain
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
				} else {# Nothing
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
				}


			} else if (ptf_qty < 0 && trx_type == "B") { # Short - Realized


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


			} else {# if ptf_qty == 0, then nothing
				res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
			}



		} else {# allow_short = FALSE



			if (ptf_qty > 0 && trx_type == "B") { # Long + No Realized


				if (prz_diff > 0) { # Paper Gain
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
				} else if (prz_diff < 0) { # Paper Loss
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
				} else {# Nothing
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
				}


			} else if (ptf_qty > 0 && trx_type == "S") { # Long - Realized


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


			} else {# if ptf_qty <= 0, then nothing
				res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
			}



		}



	} else {# not realized_only



		if (allow_short) {



			if (ptf_qty > 0 && trx_type == "B") { # Long + No Realized


				if (prz_diff > 0) { # Paper Gain
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = Er, "PL_value" = 0)
				} else if (prz_diff < 0) { # Paper Loss
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = Er)
				} else {# Nothing
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
				}


			} else if (ptf_qty > 0 && trx_type == "S") { # Long - Realized


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


			} else if (ptf_qty < 0 && trx_type == "S") { # Short + No Realized


				if (prz_diff > 0) { # Paper Loss
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = Er)
				} else if (prz_diff < 0) { # Paper Gain
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = -Er, "PL_value" = 0)
				} else {# Nothing
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
				}


			} else if (ptf_qty < 0 && trx_type == "B") { # Short - Realized


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


			} else {# if ptf_qty == 0, then nothing
				res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
			}



		} else {# allow_short = FALSE



			if (ptf_qty > 0 && trx_type == "B") { # Long + No Realized


				if (prz_diff > 0) { # Paper Gain
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = Er, "PL_value" = 0)
				} else if (prz_diff < 0) { # Paper Loss
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = Er)
				} else {# Nothing
					res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
				}


			} else if (ptf_qty > 0 && trx_type == "S") { # Long - Realized


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


			} else {# if ptf_qty <= 0, then nothing
				res <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
			}



		}



	}

	return(res)

}


#' @describeIn realized_compute Computation, as simple financial duration, of
#'   realized gains and realized losses of the traded asset.
#' @export
realized_duration <- function(ptf_qty, ptf_prz, trx_qty, trx_prz, trx_type,
															initial_dtt, last_dtt, trx_dtt,
															allow_short = FALSE, realized_only = FALSE) {

	if (!is.numeric(ptf_qty) || !is.numeric(ptf_prz) ||
			!is.numeric(trx_qty) || !is.numeric(trx_prz)) {
		stop("Arguments *qty and *prz must be numeric.", call. = FALSE)
	}

	if (!lubridate::is.POSIXct(initial_dtt) ||
			!lubridate::is.POSIXct(last_dtt) ||
			!lubridate::is.POSIXct(trx_dtt)) {
		stop("Arguments *dtt must be POSIXct.", call. = FALSE)
	}

	qty_diff <- ptf_qty + trx_qty # quantity difference (if trx_type == "S" then trx_qty < 0)
	prz_diff <- trx_prz - ptf_prz # price difference
	# dtt_diff <- difftime(trx_dtt, last_dtt, units = "days") %>% # duration of paper gain/loss
	# as.numeric() # to avoid conversion errors
	dtt_diff <- financial_difftime(last_dtt, trx_dtt)
	# dtt_diff0 <- difftime(trx_dtt, initial_dtt, units = "days") %>% # duration of realized gain/loss
	#   as.numeric() # to avoid conversion errors
	dtt_diff0 <- financial_difftime(initial_dtt, trx_dtt)
	# by default the duration returned is in days, conversion is left to the user


	if (realized_only) {



		if (allow_short) {



			if (ptf_qty > 0 && trx_type == "B") { # Long + No Realized


				if (prz_diff > 0) { # Paper Gain
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
				} else if (prz_diff < 0) { # Paper Loss
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
				} else {# Nothing
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
				}


			} else if (ptf_qty > 0 && trx_type == "S") { # Long - Realized


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


			} else if (ptf_qty < 0 && trx_type == "S") { # Short + No Realized


				if (prz_diff > 0) { # Paper Loss
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
				} else if (prz_diff < 0) { # Paper Gain
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
				} else {# Nothing
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
				}


			} else if (ptf_qty < 0 && trx_type == "B") { # Short - Realized


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


			} else {# if ptf_qty == 0, then nothing
				res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
			}



		} else {# allow_short = FALSE



			if (ptf_qty > 0 && trx_type == "B") { # Long + No Realized


				if (prz_diff > 0) { # Paper Gain
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
				} else if (prz_diff < 0) { # Paper Loss
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
				} else {# Nothing
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
				}


			} else if (ptf_qty > 0 && trx_type == "S") { # Long - Realized


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


			} else {# if ptf_qty <= 0, then nothing
				res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
			}



		}



	} else {# not realized_only



		if (allow_short) {



			if (ptf_qty > 0 && trx_type == "B") { # Long + No Realized


				if (prz_diff > 0) { # Paper Gain
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = dtt_diff, "PL_duration" = 0)
				} else if (prz_diff < 0) { # Paper Loss
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = dtt_diff)
				} else {# Nothing
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
				}


			} else if (ptf_qty > 0 && trx_type == "S") { # Long - Realized


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


			} else if (ptf_qty < 0 && trx_type == "S") { # Short + No Realized


				if (prz_diff > 0) { # Paper Loss
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = dtt_diff)
				} else if (prz_diff < 0) { # Paper Gain
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = dtt_diff, "PL_duration" = 0)
				} else {# Nothing
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
				}


			} else if (ptf_qty < 0 && trx_type == "B") { # Short - Realized


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


			} else {# if ptf_qty == 0, then nothing
				res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
			}



		} else {# allow_short = FALSE



			if (ptf_qty > 0 && trx_type == "B") { # Long + No Realized


				if (prz_diff > 0) { # Paper Gain
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = dtt_diff, "PL_duration" = 0)
				} else if (prz_diff < 0) { # Paper Loss
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = dtt_diff)
				} else {# Nothing
					res <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
				}


			} else if (ptf_qty > 0 && trx_type == "S") { # Long - Realized


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


			} else {# if ptf_qty <= 0, then nothing
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
realized_compute <- function(ptf_qty, ptf_prz, trx_qty, trx_prz, trx_type,
														 initial_dtt, last_dtt, trx_dtt, trx_asset,
														 allow_short = FALSE, realized_only = FALSE,
														 method = "all") {

	if (method == "count") {

		# compute Realized and Paper Gain and Loss with realized_count()
		rgl_count <- realized_count(ptf_qty, ptf_prz, trx_qty, trx_prz, trx_type, allow_short, realized_only)
		# convert results to a df
		res_df <- c("asset" = trx_asset, as.list(rgl_count)) %>%
			tibble::as_tibble()

	} else if (method == "total") {

		# compute Realized and Paper Gain and Loss with realized_total()
		rgl_total <- realized_total(ptf_qty, ptf_prz, trx_qty, trx_prz, trx_type, allow_short, realized_only)
		# convert results to a df
		res_df <- c("asset" = trx_asset, as.list(rgl_total)) %>%
			tibble::as_tibble()

	} else if (method == "value") {

		# compute Realized and Paper Gain and Loss with realized_value()
		rgl_value <- realized_value(ptf_qty, ptf_prz, trx_qty, trx_prz, trx_type, allow_short, realized_only)
		res_df <- c("asset" = trx_asset, as.list(rgl_value)) %>%
			tibble::as_tibble()

	} else if (method == "duration") {

		# compute Realized and Paper Gain and Loss with realized_value()
		rgl_duration <- realized_duration(ptf_qty, ptf_prz, trx_qty, trx_prz, trx_type,
																			initial_dtt, last_dtt, trx_dtt, allow_short, realized_only)
		res_df <- c("asset" = trx_asset, as.list(rgl_duration)) %>%
			tibble::as_tibble()

	} else {# method == "all"

		# compute Realized and Paper Gain and Loss with all functions and wrap-up results
		rgl_count <- realized_count(ptf_qty, ptf_prz, trx_qty, trx_prz, trx_type, allow_short, realized_only)
		rgl_total <- realized_total(ptf_qty, ptf_prz, trx_qty, trx_prz, trx_type, allow_short, realized_only)
		rgl_value <- realized_value(ptf_qty, ptf_prz, trx_qty, trx_prz, trx_type, allow_short, realized_only)
		rgl_duration <- realized_duration(ptf_qty, ptf_prz, trx_qty, trx_prz, trx_type,
																			initial_dtt, last_dtt, trx_dtt, allow_short, realized_only)
		# convert results to a df
		res_df <- c("asset" = trx_asset,
								as.list(rgl_count),
								as.list(rgl_total),
								as.list(rgl_value),
								as.list(rgl_duration)) %>%
			tibble::as_tibble()

	}

	return(res_df)

}


#' @describeIn realized_compute Simple function to obtain empty results for
#'   realized and paper computations based on the chosen method.
#' @export
realized_empty <- function(trx_asset, method = "all") {

	if (method == "count") {

		rgl_count <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
		res_df <- c("asset" = trx_asset, as.list(rgl_count)) %>%
			tibble::as_tibble()

	} else if (method == "total") {

		rgl_total <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
		res_df <- c("asset" = trx_asset, as.list(rgl_total)) %>%
			tibble::as_tibble()

	} else if (method == "value") {

		rgl_value <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
		res_df <- c("asset" = trx_asset, as.list(rgl_value)) %>%
			tibble::as_tibble()

	} else if (method == "duration") {

		rgl_duration <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
		res_df <- c("asset" = trx_asset, as.list(rgl_duration)) %>%
			tibble::as_tibble()

	} else {# method == "all"

		rgl_count <- c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0)
		rgl_total <- c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0)
		rgl_value <- c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0)
		rgl_duration <- c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0)
		# convert results to a df
		res_df <- c("asset" = trx_asset,
								as.list(rgl_count),
								as.list(rgl_total),
								as.list(rgl_value),
								as.list(rgl_duration)) %>%
			tibble::as_tibble()

	}

	return(res_df)

}

