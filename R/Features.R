#' @title Features Class
#'
#' @description
#' Calculates all features for financial time series forecasting.
#'
#' @importFrom R6 R6Class is.R6
#'
#' @export
#' @examples
#' data(spy_hour)
#' OhlcvInstance = Ohlcv$new(spy_hour, date_col = "datetime")
#' FeaturesInstance = Features$new(OhlcvInstance,
#'                                 at = c(300:315, 1000:1100),
#'                                 lag = 1L)
#' x = FeaturesInstance$get_features()
#' tail(x)
Features = R6::R6Class(
  "Features",

  public = list(

    #' @field ohlcv_object OHLCV object
    ohlcv_object = NULL,

    #' @field at Argument `at` in runner package
    at = NULL,

    #' @field lag Argument `lag` in runner package
    lag = NULL,

    #' @description
    #' Create a new OhlcvFeatures object.
    #'
    #' @param ohlcv_object Length of window for calculating rolling versions of the indicators.
    #' @param at Argument `at` in runner package
    #' @param lag Argument `lag` in runner package
    #'
    #' @return A new `Features` object.
    initialize = function(ohlcv_object, at, lag = 1L) {
      self$ohlcv_object = ohlcv_object
      self$at = at
      self$lag = lag
    },

    #' @description
    #' Function calculates basic features from OHLCV financial data
    #'
    #' @param data X field of Ohlcv object
    #' @param window window length. This argument is given internaly
    #'
    #' @return Data.table with new features.
    get_features = function() {

      # prepare data
      OhlcvInstance <- self$ohlcv_object
      at_ = self$at
      lag_ <- self$lag

      ########## TEST
      # data(spy_hour)
      # OhlcvInstance = Ohlcv$new(spy_hour, date_col = "datetime")
      # ohlcv = OhlcvInstance$X
      # at_ <- c(300:315, 1000:1020)
      # lag_ <- 1L
      ######### TEST

      # Features from OHLLCV
      print("Calculate Ohlcv features.")
      OhlcvFeaturesInit = OhlcvFeatures$new(windows = c(5, 22, 22 * 3, 22 * 6, 22 * 12),
                                            quantile_divergence_window =  c(22, 22*3, 22*12))
      OhlcvFeaturesSet = OhlcvFeaturesInit$get_ohlcv_features(OhlcvInstance)
      OhlcvFeaturesSet <- OhlcvFeaturesSet[at_ - lag_] ## HERE !!!!!!!!!!

      # BidAsk features
      print("Calculate BidAsk features.")
      RollingBidAskInstance <- RollingBidAsk$new(windows = c(5, 22),
                                                 workers = 12L,
                                                 at = at_,
                                                 lag = lag_,
                                                 na_pad = TRUE,
                                                 simplify = FALSE
      )
      RollingBidAskFeatures = RollingBidAskInstance$get_rolling_features(OhlcvInstance)

      # BackCUSUM features
      print("Calculate BackCUSUM features.")
      RollingBackcusumInit = RollingBackcusum$new(windows = c(100),
                                                  workers = 12L,
                                                  at = at_,
                                                  lag = lag_,
                                                  na_pad = TRUE,
                                                  simplify = FALSE)
      RollingBackCusumFeatures = RollingBackcusumInit$get_rolling_features(OhlcvInstance)

      # Exuber features
      print("Calculate Exuber features.")
      RollingExuberInit = RollingExuber$new(windows = c(100, 300, 600),
                                            workers = 12L,
                                            at = at_,
                                            lag = lag_,
                                            na_pad = TRUE,
                                            simplify = FALSE,
                                            exuber_lag = 1L)
      RollingExuberFeatures = RollingExuberInit$get_rolling_features(OhlcvInstance)

      # Forecast Features
      print("Calculate AutoArima features.")
      RollingForecatsInstance = RollingForecats$new(windows = c(252),
                                                    workers = 12L,
                                                    lag = lag_,
                                                    at = at_,
                                                    na_pad = TRUE,
                                                    simplify = FALSE,
                                                    forecast_type = "autoarima",
                                                    h = 22)
      RollingForecatsAutoarimaFeatures = RollingForecatsInstance$get_rolling_features(OhlcvInstance)
      print("Calculate Nnetar features.")
      RollingForecatsInstance = RollingForecats$new(windows = c(252),
                                                    workers = 12L,
                                                    lag = lag_,
                                                    at = at_,
                                                    na_pad = TRUE,
                                                    simplify = FALSE,
                                                    forecast_type = "nnetar",
                                                    h = 22)
      RollingForecatNnetarFeatures = RollingForecatsInstance$get_rolling_features(OhlcvInstance)

      # GAS
      print("Calculate GAS features.")
      RollingGasInit = RollingGas$new(windows = c(100),
                                      workers = 12L,
                                      at = at_,
                                      lag = lag_,
                                      na_pad = TRUE,
                                      simplify = FALSE,
                                      gas_dist = "sstd",
                                      gas_scaling = "Identity",
                                      prediction_horizont = 10)
      RollingGasFeatures = RollingGasInit$get_rolling_features(OhlcvInstance)

      # Gpd features
      print("Calculate Gpd features.")
      RollingGpdInit = RollingGpd$new(windows = c(22 * 6),
                                      workers = 12L,
                                      at = at_,
                                      lag = lag_,
                                      na_pad = TRUE,
                                      simplify = FALSE,
                                      threshold = 0.05)
      RollingGpdFeatures = RollingGpdInit$get_rolling_features(OhlcvInstance)

      # theft catch22 features
      print("Calculate Catch22 features.")
      RollingTheftInit = RollingTheft$new(windows = c(22, 22 * 3, 22 * 12),
                                          workers = 12L,
                                          at = at_,
                                          lag = lag_,
                                          na_pad = TRUE,
                                          simplify = FALSE,
                                          features_set = "catch22")
      RollingTheftCatch22Features = RollingTheftInit$get_rolling_features(OhlcvInstance)

      # theft feasts features
      print("Calculate feasts features.")
      RollingTheftInit = RollingTheft$new(windows = c(22, 22 * 3, 22 * 12),
                                          workers = 12L,
                                          at = at_,
                                          lag = lag_,
                                          na_pad = TRUE,
                                          simplify = FALSE,
                                          features_set = "feasts")
      RollingTheftFeastsFatures = RollingTheftInit$get_rolling_features(OhlcvInstance)

      # theft tsfeatures features
      print("Calculate Tsfeatures features.")
      RollingTheftInit = RollingTheft$new(windows = c(22 * 3, 22 * 12),
                                          workers = 12L,
                                          at = at_,
                                          lag = lag_,
                                          na_pad = TRUE,
                                          simplify = FALSE,
                                          features_set = "tsfeatures")
      RollingTheftTsfeaturesFeatures = RollingTheftInit$get_rolling_features(OhlcvInstance)

      # theft tsfel features
      print("Calculate tsfel features.")
      RollingTheftInit = RollingTheft$new(windows = 200,
                                          workers = 1L,
                                          at = at_,
                                          lag = lag_,
                                          na_pad = TRUE,
                                          simplify = FALSE,
                                          features_set = "tsfel")
      RollingTheftTsfelFeatures = RollingTheftInit$get_rolling_features(OhlcvInstance)

      # merge all features
      features <- Reduce(function(x, y) merge(x, y, by = c("symbol", "date"), all.x = TRUE, all.y = FALSE),
                         list(OhlcvFeaturesSet, RollingBidAskFeatures, RollingBackCusumFeatures, RollingExuberFeatures,
                              RollingForecatsAutoarimaFeatures, RollingForecatNnetarFeatures,
                              RollingGasFeatures, RollingGpdFeatures, RollingTheftCatch22Features,
                              RollingTheftFeastsFatures, RollingTheftTsfeaturesFeatures, RollingTheftTsfelFeatures))


      return(features)
    }
  )
)
