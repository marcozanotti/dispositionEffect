#' @name dispositionEffect-package
#'
#' @title dispositionEffect: behavioral Analysis on Financial Data
#'
#' @description The dispositionEffect package allows to perform different
#'   types of behavioral analysis using financial market and experimental
#'   data.
#'   The analysis of disposition effect, portfolio-driven disposition
#'   effect, and time series disposition effect can be performed with
#'   four different implemented methods.
#'
#' @section Main functions:
#'
#'   * `portfolio_compute` is a wrapper function that compute realized and
#'   paper gains and losses from the investor's transactions and the market
#'   prices of the traded assets and updates the investor's portfolio
#'
#'   * `gains_losses` is the core function of the package. It performs all
#'   the necessary calculations and can be used for real-time processing
#'   (it is intended for advanced users only)
#'
#'   * `disposition_effect` Compute the disposition effect based on
#'   realized and paper gains and losses
#'
#'   * `disposition_difference` Compute the disposition difference
#'   based on realized gains and losses
#'
#'   * `disposition_compute`and `disposition_summary`interfaces that allow
#'   to easily compute disposition effect and summary statistics.
#'
#' @author L. Mazzucchelli & M. Zanotti
#'
#' @references
#'
#' * An, Li and Engelberg, Joseph and Henriksson, Matthew and Wang, Baolian and Williams,
#' Jared, 2019, "The Portfolio-Driven Disposition Effect".
#'
#' * Filippin, Mazzucchelli and Zanotti, 2021, "An analysis of the short selling impact on the disposition effect extended in the portfolio framework" (working paper).
#'
#' * Mazzucchelli, 2021, "An Analysis of Short Selling and Volatility Impact on the Disposition Effect" (working paper).
#'
#' * Odean, Terrance, 1998, "Are investors reluctant to realize their losses?" Journal of Finance 53:5, 1775-98.
#'
#' * Sakaguchi, Hiroaki and Stewart, Neil and Walasek, Lukasz, 2019, "The Disposition Effect Varies with Portfolio
#' Composition Because People Take Gain-Loss-Domain-Level Sell Decisions".
#'
#' * Shefrin, Hersh, and Meir Statman, 1985, "The disposition to sell winners too early and ride losers too long",
#' Journal of Finance 40:3, 777-90.
#'
#' * Weber, Martin, and Colin F. Camerer, 1998, "The disposition effect in securities trading: An experimental analysis",
#' Journal of Economic Behavior and Organization 33:2, 167-84.
#'
#' @docType package
NULL
