#' Client 4273N financial transactions
#'
#' A sample dataset containing 19 transactions over time.
#'
#' @format A data frame with 19 rows and 6 variables:
#' \describe{
#'   \item{client}{id of the investor}
#'   \item{type}{binary variable indicating the type of operation, B = buy and S = sell}
#'   \item{asset}{id of the traded asset}
#'   \item{qty}{quantity of the traded asset}
#'   \item{prz}{market price of the traded asset}
#'   \item{datetime}{timestamp of the operation}
#' }
#'
#' @source \url{https://www.directa.it/index-ita.html}
"client_4273N"


#' Market prices of assets traded by client 4273N
#'
#' A sample dataset containing 6895 market prices of 5 different assets over time.
#'
#' @format A data frame with 6895 rows and 4 variables:
#' \describe{
#'   \item{asset}{id of the asset}
#'   \item{datetime}{timestamp of market price}
#'   \item{prz}{market price of the asset}
#' }
#'
#' @source \url{https://www.directa.it/index-ita.html}
"marketprices_4273N"
