#' @title Features Set
#'
#' @description
#' Calculate features specified in config file.
#'
#' @export
Features = R6::R6Class(
  "Features",

  public = list(

    #' @description
    #' Create a new Features object.
    #'
    #' @param features_config AWS S3 Tiledb config
    #'
    #' @return A new `Features` object.
    initialize = function(features_config = "inst/extdata/features_config.yaml") {

      # read yaml config
      features_conf <- yaml.load_file(features_config)
      features_conf <- replaceInList(features_conf, function(x)if(is.null(x)) integer(0) else x)
    },

    #' @description
    #' Function calculates forecastas based on auto.arima and nnetar functions from forecast package.
    #'
    #' @param data X field of Ohlcv object
    #'
    #' @return Calculate rolling features from forecasting package.
    calcualte_featuers = function(data) {

      # debug
      # data <- spy_hour[1:102]
      # ohlcv <- Ohlcv$new(data, date_col = "datetime")

      # exuber
      if ("RollingExuber" %in% names(features_conf)) {
        RollingExuberInit = do.call(RollingExuber$new, features_conf$RollingExuber)
        RollingExuberFeatures <- RollingExuberInit$get_rolling_features(ohlcv)
      }

      # autoarima
      if ("RollingForecasts" %in% names(features_conf)) {
        RollingAutoarimaInit = do.call(RollingForecats$new, features_conf$RollingForecasts)
        RollingAutoarimaFeatures <- RollingAutoarimaInit$get_rolling_features(ohlcv)
      }

      test <- NULL

      # merge all features
      features_c <- c("RollingExuberFeatures", "RollingAutoarimaFeatures",
                      "RollingBidAskFeatues")
      features_l <- features_c[vapply(features_c, exists, logical(1))]
      features <- Reduce(function(x, y) merge(x, y, by = c("symbol", "date"), all.x = TRUE, all.y = FALSE),
                         sapply(features_l, get))

      return(features)
    }
  ),
  private = list(

    #' @description
    #' Function calculates forecastas based on auto.arima and nnetar functions from forecast package.
    #' https://stackoverflow.com/questions/38950005/how-to-manipulate-null-elements-in-a-nested-list/
    #'
    #' @param x List
    #' @param FUN function to apply
    #'
    #' @return Calculate rolling features from forecasting package.
    replaceInList = function (x, FUN, ...)
    {
      if (is.list(x)) {
        for (i in seq_along(x)) {
          x[i] <- list(replaceInList(x[[i]], FUN, ...))
        }
        x
      }
      else FUN(x, ...)
    }
  )
)
