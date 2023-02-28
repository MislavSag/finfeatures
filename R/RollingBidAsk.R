#' @title RollingBidAsk Class
#'
#' @description
#' Function calculates spread from exuber bidask package.
#'
#' @export
#' @examples
#' data(spy_hour)
#' OhlcvInstance = Ohlcv$new(spy_hour, date_col = "datetime")
#' RollingBidAskInstance <- RollingBidAsk$new(windows = 200,
#'                                            workers = 2L,
#'                                            at = c(300, 500),
#'                                            lag = 1L,
#'                                            method = "EDGE")
#' x = RollingBidAskInstance$get_rolling_features(OhlcvInstance)
#' head(x)
#' # multiple windows and parallel and vector arguments
#' RollingBidAskInstance <- RollingBidAsk$new(windows = 200,
#'                                            workers = 2L,
#'                                            at = c(300, 500),
#'                                            lag = 1L,
#'                                            method = c("EDGE", "Roll"))
#' x = RollingBidAskInstance$get_rolling_features(OhlcvInstance)
#' head(x)
RollingBidAsk = R6::R6Class(
  "RollingBidAsk",
  inherit = RollingGeneric,

  public = list(

    #' @field methods Bidask methods.
    methods = NULL,

    #' @description
    #' Create a new RollingBidAsk object.
    #'
    #' @param windows Vector of windows that will be applied on features.
    #' @param workers Number of workers. Greater than 1 for parallle processing
    #' @param lag Lag variable in runner package.
    #' @param at Argument at in runner package.
    #' @param methods Argument methods in Bidask package.
    #'
    #' @return A new `RollingBidAsk` object.
    initialize = function(windows,
                          workers,
                          lag,
                          at,
                          methods = c("EDGE", "Roll", "OHLC", "OHL.CHL")) {

      # define all params combination
      private$params <- expand.grid(methods = methods, stringsAsFactors = FALSE)
      colnames(private$params) <- c("methods")

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

      # check if there is enough data
      if (length(unique(x$symbol)) > 1) {
        return(NA)
      }

      # data prep
      data_ <- x[, .(date, open, high, low, close)]
      setnames(data_, c("date", "Open", "High", "Low", "Close"))
      data_ <- as.xts.data.table(data_)

      # calculate spreads
      y <- tryCatch(bidask::spread(data_,
                                   method = params,
                                   probs = c(0.05, 0.95)), error = function(e) NA)
      if (all(is.na(y))) {
        return(NULL)
      } else {
        result <- as.data.table(y)
        colnames(result) <- paste("bidask", window, params, colnames(result), sep = "_")
        result[, result[, colnames(result)[grep("index", colnames(result))]]] <- NULL
        return(as.data.table(result))
      }
    }
  ),
  private = list(
    packages = "bidask",
    params = NULL
  )
)
