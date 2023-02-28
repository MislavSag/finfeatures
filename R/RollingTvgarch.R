#' @title RollingTvgarch Class
#'
#' @description
#' Function calculates tvgarch predictions and coefficients from tvgarch package on rolling window.
#'
#' @export
#' @examples
#' data(spy_hour)
#' OhlcvInstance = Ohlcv$new(spy_hour, date_col = "datetime")
#' RollingTvgarchInit = RollingTvgarch$new(windows = 200,
#'                                         workers = 1L,
#'                                         at = c(300, 500),
#'                                         lag = 0L,
#'                                         turbo = TRUE)
#' x = RollingTvgarchInit$get_rolling_features(OhlcvInstance)
#' head(x)
#' RollingTvgarchInit = RollingTvgarch$new(windows = 200,
#'                                         workers = 2L,
#'                                         at = c(300, 500),
#'                                         lag = 0L,
#'                                         turbo = c(TRUE, FALSE))
#' x = RollingTvgarchInit$get_rolling_features(OhlcvInstance)
#' head(x)
RollingTvgarch = R6::R6Class(
  "RollingTvgarch",
  inherit = RollingGeneric,

  public = list(

    #' @field turbo Check argument in tvgarch function.
    turbo = NULL,

    #' @description
    #' Create a new RollingExuber object.
    #'
    #' @param windows Vector of windows that will be applied on features.
    #' @param workers Number of workers. Greater than 1 for parallle processing
    #' @param lag Lag variable in runner package.
    #' @param at Argument at in runner package.
    #' @param turbo Check argument in tvgarch function.
    #'
    #' @return A new `RollingGas` object.
    initialize = function(windows,
                          workers,
                          lag,
                          at,
                          turbo = TRUE) {

      # define all params combination
      private$params <- expand.grid(turbo = turbo, stringsAsFactors = FALSE)
      colnames(private$params) <- c("turbo")

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
      y <- na.omit(x$returns) * 100
      y <- tvgarch::tvgarch(y, turbo = TRUE)
      pred <- as.numeric(predict(y))
      coefs <- coef(y)
      results <- data.table(pred_1 = pred[1], pred_n = tail(pred, 1), pred_mean = mean(pred, na.rm = TRUE), t(coefs))
      colnames(results) <- paste0("tvgarch_", colnames(results))
      colnames(results) <- paste(colnames(results), params, window, sep = "_")
      return(results)
    }
  ),

  private = list(
    packages = "tvgarch",
    params = NULL
  )
)
