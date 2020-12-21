#' @title Sample investor financial transactions
#'
#' @description A sample dataset containing 19 transactions over time.
#'
#' @format A data frame with 19 rows and 6 variables:
#' \describe{
#'   \item{investor}{id of the investor}
#'   \item{type}{binary variable indicating the type of operation, B = buy and S = sell}
#'   \item{asset}{id of the traded asset}
#'   \item{quantity}{quantity of the traded asset}
#'   \item{price}{market price of the traded asset}
#'   \item{datetime}{timestamp of the operation}
#' }
#'
#' @source \url{https://www.directa.it/index-ita.html}
"investor"


#' Market prices of assets traded by the sample investor
#'
#' A sample dataset containing 6895 market prices of 5 different assets over time.
#'
#' @format A data frame with 6895 rows and 4 variables:
#' \describe{
#'   \item{asset}{id of the asset}
#'   \item{datetime}{timestamp of market price}
#'   \item{price}{market price of the asset}
#' }
#'
#' @source \url{https://www.directa.it/index-ita.html}
"marketprices"
