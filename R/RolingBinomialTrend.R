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
#'                                             workers = 1L,
#'                                             at = c(300, 500),
#'                                             lag = 0L,
#'                                             na_pad = TRUE,
#'                                             simplify = FALSE)
#' x = RollingBinomialTrendInit$get_rolling_features(OhlcvInstance)
#' head(x)
#' # multiple windows and parallel
# RollingBinomialTrendInit = RollingBinomialTrend$new(windows = c(22, 66),
#                                             workers = 8L,
#                                             at = c(300:315),
#                                             lag = 1L,
#                                             na_pad = TRUE,
#                                             simplify = FALSE)
# x = RollingBinomialTrendInit$get_rolling_features(OhlcvInstance)
# head(x)
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
    #' @param na_pad Argument na_pad in runner package.
    #' @param simplify Argument simplify in runner package.
    #'
    #' @return A new `RollingBinomialTrend` object.
    initialize = function(windows, workers, lag, at, na_pad, simplify) {
      
      # super init
      super$initialize(
        windows,
        workers,
        lag,
        at,
        na_pad,
        simplify,
        private$packages
      )
    },
    
    
    #' @description
    #' Function calculates trend, p-value features from binomialtrend on rolling window.
    #'
    #' @param data X field of Ohlcv object
    #' @param window window length. This argument is given internaly
    #' @param price Price column in Ohlcv
    #'
    #' @return Calculate rolling trend, p-value features from binomialtrend package.
    rolling_function = function(data, window, price) {
      
      # check if there is enough data
      if (length(unique(data$symbol)) > 1) {
        print(paste0("not enough data for symbol ", data$symbol[1]))
        return(NA)
      }
      
      # calculate arima forecasts
      y <- na.omit(data$close)
      y <- binomialtrend::binomialtrend(y)
    
      results <- c(y$parameter, y$p.value)

      names(results) <- c("trend", "p-value")
      results <- as.data.table(as.list(results))
      results <- data.table(symbol = data$symbol[1], date = data$date[length(data$date)], results)
      colnames(results)[3:ncol(results)] <- paste(colnames(results)[3:ncol(results)], window, sep = "_")
      return(results)
    }
  ),
  
  private = list(
    packages = "binomialtrend"
  )
)
