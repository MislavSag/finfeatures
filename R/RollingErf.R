#' #' @title Erf Class
#' #'
#' #' @description
#' #' Function calculates predictions (quantiles) using extreme radnom forest
#' #' (erf package).
#' #'
#' #' @export
#' #' @examples
#' #' data(spy_hour)
#' #' OhlcvInstance = Ohlcv$new(spy_hour, date_col = "datetime")
#' #' RollingErfInit = RollingErf$new(
#' #'   windows = 200,
#' #'   workers = 1L,
#' #'   at = c(300, 500),
#' #'   lag = 0L)
#' #' x = RollingErfInit$get_rolling_features(OhlcvInstance)
#' #' head(x)
#' RollingErf = R6::R6Class(
#'   "RollingErf",
#'   inherit = RollingGeneric,
#'
#'   public = list(
#'
#'     #' @description
#'     #' Create a new RollingErf object.
#'     #'
#'     #' @param windows Vector of windows that will be applied on features.
#'     #' @param workers Number of threads.
#'     #' @param lag Argument lag in runner package.
#'     #' @param at Argument at in runner package.
#'     #' @param intermediate_estimator Argument intermediate_estimator  in erf package.
#'     #'
#'     #' @return A new `RollingErf` object.
#'     initialize = function(windows,
#'                           workers,
#'                           lag,
#'                           at,
#'                           intermediate_estimator = c("grf", "neural_nets")) {
#'
#'       # define all params combination
#'       private$params = expand.grid(intermediate_estimator = intermediate_estimator,
#'                                    stringsAsFactors = FALSE)
#'       colnames(private$params) = c("intermediate_estimator")
#'
#'       # super initialize from RollingGeneric
#'       super$initialize(
#'         windows,
#'         workers,
#'         lag,
#'         at,
#'         private$packages
#'       )
#'     },
#'
#'     #' @description
#'     #' Function calculates radf values from exuber package on rolling window.
#'     #'
#'     #' @param x Ohlcv object.
#'     #' @param window Rolling window lengths.
#'     #' @param price_col Prcie column in Ohlcv
#'     #' @param params Vector of parameters
#'     #'
#'     #' @return Calculate rolling radf features from exuber package.
#'     rolling_function = function(x, window, price_col, params) {
#'
#'       # Debug
#'       data(spy_hour)
#'       x = as.data.table(spy_hour)
#'       x[, returns := close / shift(close, 1) - 1, by = symbol]
#'       X = x[, shift(returns, c(1, 5, 22, 44, 66, 150, 252))]
#'       rows_remove = X[, which(is.na(.SD)), .SDcols = ncol(X)]
#'       y = x[-rows_remove, returns]
#'       X = as.matrix(X[-rows_remove])
#'       res = erf::erf(X = tail(X, 500), Y = tail(y, 500))
#'
#'       # TODO: Create function with some basic predictors to use
#'
#'       # calculate arima forecasts
#'       X = x[, shift(returns, c(1, 5, 22, 44, 66, 150, 252))]
#'       rows_remove = X[, which(is.na(.SD)), .SDcols = ncol(X)]
#'       y = x[-rows_remove, returns]
#'       X = as.matrix(X[-rows_remove])
#'       res = erf::erf(X = X, Y = y, intermediate_estimator = params$intermediate_estimator)
#'
#'       predict(res)
#'
#'       results = data.table(
#'         vse = y_vse,
#'         wnoise_statistic = y_wnoise$Wnoise,
#'         wnoise_pvalue = y_wnoise$p.value,
#'         short_long_statistic = y_sl$SLmemory,
#'         short_long_pvalue = y_sl$p.value
#'       )
#'       colnames(results) = paste(colnames(results), window, params, sep = "_")
#'       return(results)
#'     }
#'   ),
#'
#'   private = list(
#'     packages = "erf",
#'     params = NULL
#'   )
#' )
