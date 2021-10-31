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
#'                                            na_pad = TRUE,
#'                                            simplify = FALSE
#' )
#' x = RollingBidAskInstance$get_rolling_features(OhlcvInstance)
#' head(x)
RollingBidAsk = R6Class(
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
    #' @param na_pad Argument na_pad in runner package.
    #' @param simplify Argument simplify in runner package.
    #' @param methods Argument methods in Bidask package.
    #'
    #' @return A new `RollingBidAsk` object.
    initialize = function(windows, workers, lag, at, na_pad, simplify, methods = c("EDGE", "Roll", "OHLC", "OHL.CHL")) {
      self$methods = methods

      super$initialize(
        windows,
        workers,
        lag,
        at,
        na_pad,
        simplify
      )
    },

    #' @description
    #' Function calculates spread from exuber bidask package.
    #'
    #' @param data X field of Ohlcv object
    #' @param window window length. This argument is given internaly
    #'
    #' @return Calculate rolling features from Bidask package.
    rolling_function = function(data, window) {

      # check if there is enough data
      if (length(unique(data$symbol)) > 1) {
        print(paste0("not enough data for symbol ", data$symbol[1]))
        return(NA)
      }

      # data prep
      data_ <- data[, .(date, open, high, low, close)]
      setnames(data_, c("date", "Open", "High", "Low", "Close"))
      data_ <- as.xts.data.table(data_)

      # calculate spreads
      y <- tryCatch(spread(data_,
                           method = self$methods,
                           probs = c(0.05, 0.95)), error = function(e) NA)
      if (all(is.na(y))) {
        return(NULL)
      } else {
        result <- cbind(symbol = data$symbol[1], date = data$date[length(data$date)], as.data.table(y))
        colnames(result)[3:ncol(result)] <- paste("bidask", window, colnames(result)[3:ncol(result)], sep = "_")
        return(as.data.table(result))
      }

    }
  )
)
