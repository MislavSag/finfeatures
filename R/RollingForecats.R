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
#'                                               workers = 2L,
#'                                               lag = 1L,
#'                                               at = c(100:110, 200:210),
#'                                               na_pad = TRUE,
#'                                               simplify = FALSE,
#'                                               forecast_type = "autoarima",
#'                                               h = 22)
#' x = RollingForecatsInstance$get_rolling_features(OhlcvInstance)
#' head(x)
#' # nnetar
#' RollingForecatsInstance = RollingForecats$new(windows = c(10, 20),
#'                                               workers = 2L,
#'                                               lag = 1L,
#'                                               at = c(100:110, 200:210),
#'                                               na_pad = TRUE,
#'                                               simplify = FALSE,
#'                                               forecast_type = "nnetar",
#'                                               h = 10)
#' x = RollingForecatsInstance$get_rolling_features(OhlcvInstance)
#' head(x)
RollingForecats = R6::R6Class(
  "RollingForecats",
  inherit = RollingGeneric,

  public = list(

    #' @field forecast_type Type of time series forecasts.
    forecast_type = NULL,

    #' @field h Forecast horizont.
    h = NULL,

    #' @description
    #' Create a new RollingForecats object.
    #'
    #' @param windows Vector of windows that will be applied on features.
    #' @param workers Number of workers. Greater than 1 for parallle processing
    #' @param lag Lag variable in runner package.
    #' @param at Argument at in runner package.
    #' @param na_pad Argument na_pad in runner package.
    #' @param simplify Argument simplify in runner package.
    #' @param forecast_type Type of time series forecasts.
    #' @param h Forecast horizont.
    #'
    #' @return A new `RollingForecats` object.
    initialize = function(windows, workers, lag, at, na_pad, simplify, forecast_type = c("autoarima", "nnetar"), h = 10) {
      self$forecast_type = forecast_type
      self$h = h

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
    #' Function calculates forecastas based on auto.arima and nnetar functions from forecast package.
    #'
    #' @param data X field of Ohlcv object
    #' @param window window length. This argument is given internaly
    #' @param price Prcie column in Ohlcv
    #'
    #' @return Calculate rolling features from forecasting package.
    rolling_function = function(data, window, price) {

      # check if there is enough data
      if (length(unique(data$symbol)) > 1) {
        print(paste0("not enough data for symbol ", data$symbol[1]))
        return(NA)
      }

      # calculate arima forecasts
      if (self$forecast_type == "autoarima") {
        y <- auto.arima(data$returns)
        y <- as.data.table(forecast(y, self$h))
        cols_prefix <- "autoarima_"
      } else if (self$forecast_type == "nnetar") {
        y <- nnetar(na.omit(data$returns))
        y <- as.data.table(forecast(y, PI = TRUE, h=self$h, npaths = 120))
        cols_prefix <- "nnetar_"
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
      data.table(symbol = data$symbol[1], date = data$date[length(data$date)], first_forecasts, last_forecasts, mean_forecasts, sd_forecasts)
    }
  )
)
