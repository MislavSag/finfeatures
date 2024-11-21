#' @title Erf Class
#'
#' @description
#' Function calculates predictions (quantiles) using extreme radnom forest
#' (erf package).
#'
#' @export
#' @examples
#' data(spy_hour)
#' OhlcvInstance = Ohlcv$new(spy_hour, date_col = "datetime")
#' RollingErfInit = RollingErf$new(
#'   windows = 200,
#'   workers = 1L,
#'   at = c(300, 500),
#'   lag = 0L)
#' x = RollingErfInit$get_rolling_features(OhlcvInstance)
#' head(x)
RollingErf = R6::R6Class(
  "RollingErf",
  inherit = RollingGeneric,

  public = list(

    #' @description
    #' Create a new RollingErf object.
    #'
    #' @param windows Vector of windows that will be applied on features.
    #' @param workers Number of threads.
    #' @param lag Argument lag in runner package.
    #' @param at Argument at in runner package.
    #' @param m Argument m in vse4ts package.
    #'
    #' @return A new `RollingVse` object.
    initialize = function(windows,
                          workers,
                          lag,
                          at,
                          m = 0.5) {

      # define all params combination
      private$params = expand.grid(m = m, stringsAsFactors = FALSE)
      colnames(private$params) = c("m")

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
      y = na.omit(x$returns)
      y_vse = vse4ts::vse(y, m = params)
      y_wnoise = vse4ts::Wnoise.test(y, m = params)
      y_sl = vse4ts::SLmemory.test(y, m = params)

      results = data.table(
        vse = y_vse,
        wnoise_statistic = y_wnoise$Wnoise,
        wnoise_pvalue = y_wnoise$p.value,
        short_long_statistic = y_sl$SLmemory,
        short_long_pvalue = y_sl$p.value
      )
      colnames(results) = paste(colnames(results), window, params, sep = "_")
      return(results)
    }
  ),

  private = list(
    packages = "vse4ts",
    params = NULL
  )
)
