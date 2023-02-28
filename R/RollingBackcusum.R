#' @title RollingBackcusum Class
#'
#' @description
#' Function calculates backCUSUM tests on rolling window from backCUSUM pakcage.
#'
#' @export
#' @examples
#' data(spy_hour)
#' OhlcvInstance = Ohlcv$new(spy_hour, date_col = "datetime")
#' RollingBackcusumInit = RollingBackcusum$new(windows = 200,
#'                                             workers = 1L,
#'                                             at = c(300, 500),
#'                                             lag = 0L,
#'                                             alternative = "greater",
#'                                             return_power = 1)
#' x = RollingBackcusumInit$get_rolling_features(OhlcvInstance)
#' head(x)
#' # multiple windows and parallel
#' RollingBackcusumInit = RollingBackcusum$new(windows = c(22, 66),
#'                                             workers = 2L,
#'                                             at = c(300:315),
#'                                             lag = 1L,
#'                                             alternative = "greater",
#'                                             return_power = c(1, 2))
#' x = RollingBackcusumInit$get_rolling_features(OhlcvInstance)
#' head(x)
RollingBackcusum = R6::R6Class(
  "RollingBackcusum",
  inherit = RollingGeneric,


  public = list(

    #' @field alternative Check arguments of SBQ.test function in BackCUSUM package
    alternative = NULL,

    #' @description
    #' Create a new RollingBackcusum object.
    #'
    #' @param windows Vector of windows that will be applied on features.
    #' @param workers Number of threads.
    #' @param lag Argument lag in runner package.
    #' @param at Argument at in runner package.
    #' @param alternative Check arguments of SBQ.test function in BackCUSUM package
    #' @param return_power Power returns by return_power.
    #'
    #' @return A new `RollingBackcusum` object.
    initialize = function(windows,
                          workers,
                          lag,
                          at,
                          alternative = "greater",
                          return_power = 1) {

      # define all params combination
      private$params <- expand.grid(alternative = alternative,
                                    return_power = return_power,
                                    stringsAsFactors = FALSE)
      colnames(private$params) <- c("alternative", "return_power")

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

      # calculate arima forecasts
      y <- na.omit(x$returns^params$return_power)
      y <- backCUSUM::SBQ.test(as.formula('y ~ 1'), alternative = params$alternative)
      results <- c(y[['statistic']], as.integer(y[['rejection']]))
      names(results) <- c("statistics", paste0("backcusum_rejections_", as.numeric(names(y[['rejection']])) * 1000))
      results <- as.data.table(as.list(results))
      colnames(results) <- paste("backcusum", window,
                                 paste0(params, collapse = "_"), colnames(results), sep = "_")
      return(results)
    }
  ),

  private = list(
    packages = "backCUSUM",
    params = NULL
  )
)
