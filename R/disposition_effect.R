#' @name disposition_effect
#'
#' @title Disposition Effect
#'
#' @description Compute the disposition effect in a vectorized way.
#'
#' @details
#'   The disposition effect is defined as
#'   \eqn{DE = (Realized Gain / (Realized Gain - Paper Gain)) -
#'        (Realized Loss / (Realized Loss + Paper Loss))}
#'
#'   The disposition difference is defined as
#'   \eqn{DD = Realized Gain - |Realized Loss|}
#'   or
#'   \eqn{DD = Paper Gain - |Paper Loss|}
#'
#' @param realized_gains Numeric vector (or scalar) containing realized gains
#'   values.
#' @param paper_gains Numeric vector (or scalar) containing paper gains
#'   values.
#' @param realized_losses Numeric vector (or scalar) containing realized losses
#'   values.
#' @param paper_losses Numeric vector (or scalar) containing paper losses
#'   values.
#' @param gains Numeric vector (or scalar) containing gains.
#' @param losses Numeric vector (or scalar) containing losses.
#'
#' @return Numeric vector (or scalar) with the value(s) of disposition
#'   effect(s) or disposition difference(s).
#'
#' @author L. Mazzucchelli & M. Zanotti
#'
#' @references H. Shefrin & M. Statman, 1985
#'
#' @examples
#'   data(portfolio_results)
#'
#'   # Disposition effect on count for each traded asset
#'   de <- disposition_effect(portfolio_results$RG_count,
#'                            portfolio_results$PG_count,
#'                            portfolio_results$RL_count,
#'                            portfolio_results$PL_count)
#'   names(de) <- portfolio_results$asset
#'
#'   # Average disposition effect of the investor
#'   mean(de)
#'
#'   # Realized difference on value for each asset
#'   disposition_difference(portfolio_results$RG_value, portfolio_results$RL_value)
#'
#'   # Paper difference on value for each asset
#'   disposition_difference(portfolio_results$PG_value, portfolio_results$PL_value)
#'
NULL


#' @describeIn disposition_effect Compute the disposition effect as defined
#'   by L. Mazzucchelli et al.
#' @export
disposition_effect <- function(realized_gains, paper_gains, realized_losses, paper_losses) {

	gains <- realized_gains / (realized_gains + paper_gains)
	losses <- abs( realized_losses / (realized_losses + paper_losses) )

	gains[is.nan(gains)] <- 0
	losses[is.nan(losses)] <- 0

	de <- gains - losses

	return(de)

}


#' @describeIn disposition_effect Compute the disposition difference as defined
#'   by L. Mazzucchelli et al.
#' @export
disposition_difference <- function(gains, losses) {

	dd <- gains - abs(losses)
	return(dd)

}

