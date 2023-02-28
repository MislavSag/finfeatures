#' @title RollingForecats Class
#'
#' @description
#' Function calculates forecastas based on auto.arima and nnetar functions from forecast package.
#'
#' @export
#' @examples
#' data(spy_hour)
#' OhlcvInstance = Ohlcv$new(spy_hour, date_col = "datetime")
#' #arima
#' RollingForecatsInstance = RollingForecats$new(windows = c(10, 20),
#'                                               workers = 1L,
#'                                               lag = 1L,
#'                                               at = c(100:110, 200:210),
#'                                               forecast_type = "autoarima",
#'                                               h = 22)
#' x = RollingForecatsInstance$get_rolling_features(OhlcvInstance)
#' head(x)
#' # nnetar
#' RollingForecatsInstance = RollingForecats$new(windows = c(10, 20),
#'                                               workers = 2L,
#'                                               lag = 1L,
#'                                               at = c(100:110, 200:210),
#'                                               forecast_type = c("autoarima", "nnetar"),
#'                                               h = 10)
#' x = RollingForecatsInstance$get_rolling_features(OhlcvInstance)
#' head(x)
RollingForecats = R6::R6Class(
  "RollingForecats",
  inherit = RollingGeneric,

  public = list(

    #' @field forecast_type Type of forecast.
    forecast_type = NULL,

    #' @field h Forecast horizont.
    h = NULL,

    #' @description
    #' Create a new RollingForecats object.
    #'
    #' @param windows Vector of windows that will be applied on features.
    #' @param workers Number of threads.
    #' @param lag Argument lag in runner package.
    #' @param at Argument at in runner package.
    #' @param forecast_type Type of time series forecasts.
    #' @param h Forecast horizont.
    #'
    #' @return A new `RollingForecats` object.
    initialize = function(windows,
                          workers,
                          lag,
                          at,
                          forecast_type = c("autoarima", "nnetar", "ets"),
                          h = 10) {

      # define all params combination
      private$params <- expand.grid(forecast_type = forecast_type,
                                    h = h, stringsAsFactors = FALSE)
      colnames(private$params) <- c("forecast_type", "h")

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
      if (params$forecast_type == "autoarima") {
        y <- forecast::auto.arima(x$returns)
        y <- as.data.table(forecast::forecast(y, params$h))
        cols_prefix <- "autoarima_"
      } else if (params$forecast_type == "nnetar") {
        y <- forecast::nnetar(na.omit(x$returns))
        y <- as.data.table(forecast::forecast(y, PI = TRUE, h=params$h, npaths = 120))
        cols_prefix <- "nnetar_"
      } else if (params$forecast_type == "ets") {
        y <- forecast::ets(na.omit(x[, get(price_col)]))
        y <- as.data.table(forecast::forecast(y, PI = TRUE, h=params$h, npaths = 120))
        y <- y - tail(x[, get(price_col)], 1)
        cols_prefix <- "ets_"
      }

      # clean arima forecasts
      first_forecasts <- y[1, ]
      colnames(first_forecasts) <- gsub(" ", "", paste0(cols_prefix, "1_", window, "_", colnames(first_forecasts)))
      last_forecasts <- y[nrow(y), ]
      colnames(last_forecasts) <- gsub(" ", "", paste0(cols_prefix, "last_", window, "_", colnames(last_forecasts)))
      mean_forecasts <- as.data.table(apply(y, 2, mean, na.rm = TRUE, simplify = FALSE))
      colnames(mean_forecasts) <- gsub(" ", "", paste0(cols_prefix, "mean_", window, "_", colnames(mean_forecasts)))
      sd_forecasts <- as.data.table(apply(y, 2, sd, na.rm = TRUE, simplify = FALSE))
      colnames(sd_forecasts) <- gsub(" ", "", paste0(cols_prefix, "sd_", window, "_", colnames(sd_forecasts)))
      data.table(first_forecasts, last_forecasts, mean_forecasts, sd_forecasts)
    }
  ),
  private = list(
    packages = "forecast",
    params = NULL
  )
)
