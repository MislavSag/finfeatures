#' @title RollingWaveletArima Class
#'
#' @description
#' Function calculates forecastas based on WaveletArima package.
#'
#' @export
#' @examples
#' data(spy_hour)
#' OhlcvInstance = Ohlcv$new(spy_hour, date_col = "datetime")
#' RollingWaveletArimaInstance = RollingWaveletArima$new(windows = c(10, 20),
#'                                                       workers = 2L,
#'                                                       lag = 1L,
#'                                                       at = c(100:110, 200:210),
#'                                                       na_pad = TRUE,
#'                                                       simplify = FALSE,
#'                                                       filter = "haar")
#' x = RollingWaveletArimaInstance$get_rolling_features(OhlcvInstance)
#' head(x)
RollingWaveletArima = R6::R6Class(
  "RollingWaveletArima",
  inherit = RollingGeneric,

  public = list(

    #' @field filter Wavelet filter use in the decomposition.
    filter = NULL,

    #' @field Waveletlevels The level of wavelet decomposition.
    Waveletlevels = NULL,

    #' @field MaxARParam The maximum AR order for auto.arima.
    MaxARParam = NULL,

    #' @field MaxMAParam The maximum MA order for auto.arima.
    MaxMAParam = NULL,

    #' @field NForecast Forecast horizont.
    NForecast = NULL,

    #' @description
    #' Create a new RollingForecats object.
    #'
    #' @param windows Vector of windows that will be applied on features.
    #' @param workers Number of threads.
    #' @param lag Argument lag in runner package.
    #' @param at Argument at in runner package.
    #' @param na_pad Argument na_pad in runner package.
    #' @param simplify Argument simplify in runner package.
    #' @param filter Wavelet filter use in the decomposition.
    #' @param Waveletlevels The level of wavelet decomposition.
    #' @param MaxARParam The maximum AR order for auto.arima.
    #' @param MaxMAParam The maximum MA order for auto.arima.
    #' @param NForecast Forecast horizont.
    #'
    #' @return A new `RollingForecats` object.
    initialize = function(windows, workers, lag, at, na_pad, simplify,
                          filter = c("haar", "la8"), MaxARParam = 5,
                          MaxMAParam = 5, NForecast = 5) {

      # parameters
      self$filter = match.arg(filter)
      self$MaxARParam = MaxARParam
      self$MaxMAParam = MaxMAParam
      self$NForecast = NForecast
      # What to do with Waveletlevels ?

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
      y <- WaveletArima::WaveletFittingarma(ts = data$returns,
                                            filter = self$filter,
                                            Waveletlevels = floor(log(length(data$returns))),
                                            MaxARParam = self$MaxARParam,
                                            MaxMAParam = self$MaxMAParam,
                                            NForecast = self$NForecast)
      forecasts <- data.table::as.data.table(t(y$Finalforecast))
      colnames(forecasts) <- paste0("WaveletFittingarma_forecasts_", seq_along(forecasts))
      forecasts[, WaveletFittingarma_forecasts_mean := mean(y$Finalforecast)]
      forecasts[, WaveletFittingarma_forecasts_sd := sd(y$Finalforecast)]
      cbind(symbol = data$symbol[1],
            date = data$date[length(data$date)],
            forecasts)
    }
  ),
  private = list(
    packages = c("WaveletArima")
  )
)
