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
#'                                       exuber_lag = 1)
#' x = RollingExuberInit$get_rolling_features(OhlcvInstance)
#' head(x)
#' # parallel and multiple windows
#' RollingExuberInit = RollingExuber$new(windows = c(200, 400),
#'                                       workers = 2L,
#'                                       at = c(300:310, 500:510),
#'                                       lag = 1L,
#'                                       exuber_lag = c(1, 2))
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
    #' @param exuber_lag Exuber lag, see exuber package
    #'
    #' @return A new `RollingExuber` object.
    initialize = function(windows,
                          workers,
                          lag,
                          at,
                          exuber_lag = 0L) {

      # define all params combination
      private$params <- expand.grid(exuber_lag = exuber_lag, stringsAsFactors = FALSE)
      colnames(private$params) <- c("exuber_lag")

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

      y <- tryCatch(exuber::radf(x[, get(price_col)],
                                 lag = params,
                                 minw = exuber:::psy_minw(window)),
                    error = function(e) NA)
      if (all(is.na(y))) {
        return(NULL)
      } else {
        stats <- exuber::tidy(y)
        bsadf <- data.table::last(exuber::augment(y))
        bsadf <- bsadf[, (ncol(bsadf)-1):ncol(bsadf)]
        result <- cbind(stats, bsadf)
        result$id <- NULL
        colnames(result) <- paste("exuber", window, params,
                                  colnames(result), sep = "_")
        return(as.data.table(result))
      }
    }
  ),

  private = list(
    packages = c("exuber"),
    params = NULL
  )
)
