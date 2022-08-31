#' Stocks market data
#'
#' Market data for sample of stocks with hour frequency for period from
#' 2004-01-01 to 2021-10-20. Data was generated through FMP cloud Prep API.
#'
#' @docType data
#'
#' @usage data(stocks)
#'
#' @format An object of class \code{"data.frame"}
#' \describe{
#'  \item{datetime}{Date and time}
#'  \item{open}{Open price}
#'  \item{high}{High price}
#'  \item{low}{Low price}
#'  \item{close}{Close price}
#'  \item{volume}{Volumme}
#' }
#'
#' @references Data was collected using FMP Cloud Prep API
#' @keywords datasets
#' @examples
#'
#' data(stocks)
#' head(stocks)
"stocks"
