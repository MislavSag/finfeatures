#' @title RollingTheft Class
#'
#' @description
#' Function calculates catch 22 features on rolling window.
#'
#' @export
#' @examples
#' data(spy_hour)
#' OhlcvInstance = Ohlcv$new(spy_hour, date_col = "datetime")
#' # catch22 features
#' RollingTheftInit = RollingTheft$new(windows = 200,
#'                                     workers = 1L,
#'                                     at = c(300, 500),
#'                                     lag = 0L,
#'                                     features_set = "catch22")
#' x = RollingTheftInit$get_rolling_features(OhlcvInstance)
#' head(x)
#' # parallel and multiple windows and vector argument
#' RollingTheftInit = RollingTheft$new(windows = 200,
#'                                     workers = 2L,
#'                                     at = c(300, 500),
#'                                     lag = 0L,
#'                                     features_set = c("catch22", "feasts"))
#' x = RollingTheftInit$get_rolling_features(OhlcvInstance)
#' head(x)
RollingTheft = R6::R6Class(
  "RollingTheft",
  inherit = RollingGeneric,

  public = list(

    #' @field features_set Features from theft package we want to calcualte.
    features_set = NULL,

    #' @description
    #' Create a new RollingExuber object.
    #'
    #' @param windows Vector of windows that will be applied on features.
    #' @param workers Number of workers. Greater than 1 for parallle processing
    #' @param lag Lag variable in runner package.
    #' @param at Argument at in runner package.
    #' @param features_set Argument from the theft package
    #'
    #' @return A new `RollingTheft` object.
    initialize = function(windows,
                          workers,
                          lag,
                          at,
                          features_set = c("catch22", "feasts", "tsfeatures",
                                           "kats", "tsfresh", "tsfel")) {

      # define all params combination
      private$params <- expand.grid(features_set = features_set, stringsAsFactors = FALSE)
      colnames(private$params) <- c("features_set")

      # super initialize from RollingGeneric
      super$initialize(
        windows,
        workers,
        lag,
        at,
        private$packages
      )

      # DEBUGE - set python path
      # use_virtualenv("C:/Users/Mislav/projects_py/pyquant")
      # builtins = import_builtins(convert = FALSE)
      # main = import_main(convert = FALSE)
      # tsfel = reticulate::import("tsfel", convert = FALSE)
      # tsfresh = reticulate::import("tsfresh", convert = FALSE)
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

      # debug
      # x = as.data.table(spy_hour)
      # x = x[, .(symbol, datetime, close)]
      # setnames(x, "datetime", "date")
      # params = c("catch22", "feasts")

      # calculate features
      y = data.table::as.data.table(
        theft::calculate_features(
          x,
          "symbol",
          "date",
          price_col,
          feature_set = params
        )
      )
      y[, var_names := paste(feature_set, names, window, sep = "_")]
      y = data.table::transpose(y[, .(var_names, values)], make.names = TRUE)
      results = data.table::as.data.table(y)

      # chamnge column names to fit to mlr3
      colnames(results) <- gsub(" |-", "_", colnames(results))

      return(results)
    }
  ),
  private = list(
    packages = "theft",
    params = NULL
  )
)

######### DEBUG ###########
# library(reticulate)
# library(finfeatures)
# # library(theft)
# library(theftms)
# reticulate::use_virtualenv("C:/Users/Mislav/projects_py/pyquant", required = TRUE)
# # mlfinlab = reticulate::import("mlfinlab", convert = FALSE)
# # pd = reticulate::import("pandas", convert = FALSE)
# builtins = import_builtins(convert = FALSE)
# main = import_main(convert = FALSE)
# # tsfel = reticulate::import("tsfel", convert = FALSE)
# tsfresh = reticulate::import("tsfresh", convert = FALSE)
# warnigns = reticulate::import("warnings", convert = FALSE)
# warnigns$filterwarnings('ignore')
#
#
# data(spy_hour)
# OhlcvInstance = Ohlcv$new(spy_hour, date_col = "datetime")
# catch22 features
# RollingTheftInit = RollingTheft$new(windows = 200,
#                                     workers = 1L,
#                                     at = c(300, 500),
#                                     lag = 0L,
#                                     features_set = "catch22")
# x = RollingTheftInit$get_rolling_features(OhlcvInstance)
# head(x)
# x = RollingTheftInit$get_rolling_features(OhlcvInstance, price_col = "returns")
# head(x)
#
#
#
# feature_matrix <- calculate_features(data = simData,
#                                      id_var = "id",
#                                      time_var = "timepoint",
#                                      values_var = "values",
#                                      group_var = "process",
#                                      feature_set = "catch22",
#                                      seed = 123)
#
# feature_matrix <- calculate_features(data = simData,
#                                      id_var = "id",
#                                      time_var = "timepoint",
#                                      values_var = "values",
#                                      group_var = "process",
#                                      feature_set = "feasts",
#                                      seed = 123)
#
# feature_matrix <- calculate_features(data = simData,
#                                      id_var = "id",
#                                      time_var = "timepoint",
#                                      values_var = "values",
#                                      group_var = "process",
#                                      feature_set = "feasts",
#                                      seed = 123)
#
# feature_matrix <- calculate_features(data = simData,
#                                      id_var = "id",
#                                      time_var = "timepoint",
#                                      values_var = "values",
#                                      group_var = "process",
#                                      feature_set = "TSFEL",
#                                      seed = 123)
#
# options(progress_enabled = FALSE)
# system.time({
#   feature_matrix <- suppressWarnings(
#     calculate_features(data = simData[1:250, ],
#                        id_var = "id",
#                        time_var = "timepoint",
#                        values_var = "values",
#                        # group_var = "process",
#                        feature_set = "tsfresh",
#                        seed = 123)
#   )
# })
#
# (0.5 * 50000 / 60) / 60 / 24
# # theft with returns
# RollingTheftInit = RollingTheft$new(windows = 200,
#                                     workers = 1L,
#                                     at = c(300, 500),
#                                     lag = 0L,
#                                     features_set = "tsfel")
# x = RollingTheftInit$get_rolling_features(OhlcvInstance)
# head(x)
