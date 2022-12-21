#' @title RollingExuber Class
#'
#' @description
#' Function calculates radf values from exuber package on rolling window.
#'
#' @export
#' @examples
#' data(spy_hour)
#' OhlcvInstance = Ohlcv$new(spy_hour, date_col = "datetime")
#' RollingExuberInit = RollingExuber$new(windows = 200,
#'                                       workers = 1L,
#'                                       at = c(300:310, 500:510),
#'                                       lag = 1L,
#'                                       na_pad = TRUE,
#'                                       simplify = FALSE,
#'                                       exuber_lag = 1)
#' x = RollingExuberInit$get_rolling_features(OhlcvInstance)
#' head(x)
#' # parallel and multiple windows
#' RollingExuberInit = RollingExuber$new(windows = c(200, 400),
#'                                       workers = 2L,
#'                                       at = c(300:310, 500:510),
#'                                       lag = 1L,
#'                                       na_pad = TRUE,
#'                                       simplify = FALSE)
#' x = RollingExuberInit$get_rolling_features(OhlcvInstance)
#' head(x)
RollingExuber = R6::R6Class(
  "RollingExuber",
  inherit = RollingGeneric,

  public = list(

    #' @field exuber_lag Lag to use in exuber, see exuber package.
    exuber_lag = NULL,

    #' @description
    #' Create a new RollingExuber object.
    #'
    #' @param windows Vector of windows that will be applied on features.
    #' @param workers Number of workers. Greater than 1 for parallle processing
    #' @param lag Lag variable in runner package.
    #' @param at Argument at in runner package.
    #' @param na_pad Argument na_pad in runner package.
    #' @param simplify Argument simplify in runner package.
    #' @param exuber_lag Exuber lag, see exuber package
    #'
    #' @return A new `RollingExuber` object.
    initialize = function(windows, workers, lag, at, na_pad, simplify,
                          exuber_lag = 0L) {

      # initialize exuber arguments
      self$exuber_lag = exuber_lag

      # super initialize from RollingGeneric
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
    #' Function calculates radf values from exuber package on rolling window.
    #'
    #' @param data X field of Ohlcv object
    #' @param window window length. This argument is given internaly
    #' @param price Prcie column in Ohlcv
    #'
    #' @return Calculate rolling radf features from exuber package.
    rolling_function = function(data, window, price) {

      # check if there is enough data
      if (length(unique(data$symbol)) > 1) {
        print(paste0("not enough data for symbol ", data$symbol[1]))
        return(NA)
      }

      # calculate radf valuies and save
      # library(finfeatures)
      # data(spy_hour)
      # x <- spy_hour$close[1:600]
      # y <- exuber::radf(x, lag = 1, minw = exuber:::psy_minw(600))

      y <- tryCatch(exuber::radf(data[, get(price)],
                                 lag = self$exuber_lag,
                                 minw = exuber:::psy_minw(window)),
                    error = function(e) NA)
      if (all(is.na(y))) {
        return(NULL)
      } else {
        stats <- exuber::tidy(y)
        bsadf <- data.table::last(exuber::augment(y))
        bsadf <- bsadf[, (ncol(bsadf)-1):ncol(bsadf)]
        result <- cbind(symbol = data$symbol[1], date = data$date[length(data$date)], stats, bsadf)
        result$id <- NULL
        colnames(result)[3:ncol(result)] <- paste("exuber", window, self$exuber_lag, colnames(result)[3:ncol(result)], sep = "_")
        return(as.data.table(result))
      }
    }
  ),

  private = list(
    packages = c("exuber")
  )
)
