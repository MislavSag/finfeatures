#' @title RollingBinomialTrend Class
#'
#' @description
#' Function calculates trend and p-value from  binomialtrend package
#'
#' @export
#' @examples
#' data(spy_hour)
#' OhlcvInstance = Ohlcv$new(spy_hour, date_col = "datetime")
#' RollingBinomialTrendInit = RollingBinomialTrend$new(windows = 200,
#'                                                     workers = 1L,
#'                                                     at = c(300, 500),
#'                                                     lag = 0L)
#' x = RollingBinomialTrendInit$get_rolling_features(OhlcvInstance)
#' head(x)
RollingBinomialTrend = R6::R6Class(
  "RollingBinomialTrend",
  inherit = RollingGeneric,

  public = list(

    #' @description
    #' Create a new RollingBinomialTrend object.
    #'
    #' @param windows Vector of windows that will be applied on features.
    #' @param workers Number of threads.
    #' @param lag Argument lag in runner package.
    #' @param at Argument at in runner package.
    #'
    #' @return A new `RollingBinomialTrend` object.
    initialize = function(windows,
                          workers,
                          lag,
                          at) {

      # super initialize from RollingGeneric
      super$initialize(
        windows,
        workers,
        lag,
        at,
        private$packages
      )
    },

    #' @description
    #' Function calculates radf values from exuber package on rolling window.
    #'
    #' @param x Ohlcv object.
    #' @param window Rolling window lengths.
    #' @param price_col Prcie column in Ohlcv
    #' @param params Vector of parameters
    #'
    #' @return Calculate rolling radf features from exuber package.
    rolling_function = function(x, window, price_col, params) {

      # calculate arima forecasts
      y <- na.omit(x[, get(price_col)])
      y <- binomialtrend::binomialtrend(y)

      results <- c(y$parameter, y$p.value)

      names(results) <- c("trend", "p-value")
      results <- as.data.table(as.list(results))
      colnames(results) <- paste(colnames(results), window, sep = "_")
      return(results)
    }
  ),

  private = list(
    packages = "binomialtrend",
    params = data.frame(x = 1)
  )
)
