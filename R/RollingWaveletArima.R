#' @title RollingWaveletArima Class
#'
#' @description
#' Function calculates forecastas based on WaveletArima package.
#'
#' @export
#' @examples
#' data(spy_hour)
#' OhlcvInstance = Ohlcv$new(spy_hour, date_col = "datetime")
#' RollingWaveletArimaInit = RollingWaveletArima$new(windows = 200,
#'                                                   workers = 1L,
#'                                                   at = c(300, 500),
#'                                                   lag = 0L,
#'                                                   filter = "haar")
#' x = RollingWaveletArimaInit$get_rolling_features(OhlcvInstance)
#' head(x)
#' RollingWaveletArimaInit = RollingWaveletArima$new(windows = 200,
#'                                                   workers = 2L,
#'                                                   at = c(300, 500),
#'                                                   lag = 0L,
#'                                                   filter = c("haar", "la8"))
#' x = RollingWaveletArimaInit$get_rolling_features(OhlcvInstance)
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
    #' @param filter Wavelet filter use in the decomposition.
    #' @param Waveletlevels The level of wavelet decomposition.
    #' @param MaxARParam The maximum AR order for auto.arima.
    #' @param MaxMAParam The maximum MA order for auto.arima.
    #' @param NForecast Forecast horizont.
    #'
    #' @return A new `RollingForecats` object.
    initialize = function(windows,
                          workers,
                          lag,
                          at,
                          filter = c("haar", "la8"),
                          MaxARParam = 5,
                          MaxMAParam = 5,
                          NForecast = 5) {

      # define all params combination
      private$params <- expand.grid(filter = filter,
                                    MaxARParam = MaxARParam,
                                    MaxMAParam = MaxMAParam,
                                    NForecast = NForecast,
                                    stringsAsFactors = FALSE)
      colnames(private$params) <- c("filter", "MaxARParam", "MaxMAParam",
                                    "NForecast")

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
      y <- WaveletArima::WaveletFittingarma(ts = na.omit(x$returns),
                                            filter = params$filter,
                                            Waveletlevels = floor(log(length(x$returns))),
                                            MaxARParam = params$MaxARParam,
                                            MaxMAParam = params$MaxMAParam,
                                            NForecast = params$NForecast)
      forecasts <- data.table::as.data.table(t(y$Finalforecast))
      colnames(forecasts) <- paste0("WaveletFittingarma_forecasts_", seq_along(forecasts))
      forecasts[, WaveletFittingarma_forecasts_mean := mean(y$Finalforecast)]
      forecasts[, WaveletFittingarma_forecasts_sd := sd(y$Finalforecast)]
      forecasts
    }
  ),
  private = list(
    packages = c("WaveletArima"),
    params = NULL
  )
)
