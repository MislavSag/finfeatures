#' SPY market data
#'
#' Market data for SPY with hour frequency for period from 2004-01-01 to 2021-10-20.
#' Data was generated through FMP cloud Prep API.
#'
#' @docType data
#'
#' @usage data(spy_hour)
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
#' data(spy_hour)
#' head(spy_hour)
"spy_hour"
