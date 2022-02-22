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
#'                                     na_pad = TRUE,
#'                                     simplify = FALSE,
#'                                     features_set = "catch22")
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
    #' @param na_pad Argument na_pad in runner package.
    #' @param simplify Argument simplify in runner package.
    #' @param features_set Argument from the theft package
    #'
    #' @return A new `RollingTheft` object.
    initialize = function(windows, workers, lag, at, na_pad, simplify,
                          features_set = c("catch22", "feasts", "tsfeatures", "kats", "tsfresh", "tsfel")) {
      self$features_set = features_set

      super$initialize(
        windows,
        workers,
        lag,
        at,
        na_pad,
        simplify
      )

      # set python path
      # library(reticulate)
      # reticulate::use_python("C:/ProgramData/Anaconda3/envs/mlfinlabenv/python.exe", required = TRUE)
      # mlfinlab = reticulate::import("mlfinlab", convert = FALSE)
      # pd = reticulate::import("pandas", convert = FALSE)
      # builtins = import_builtins(convert = FALSE)
      # main = import_main(convert = FALSE)


    },

    #' @description
    #' Function calculates catch 22 features on rolling window.
    #'
    #' @param data X field of Ohlcv object
    #' @param window window length. This argument is given internaly
    #' @param price Prcie column in Ohlcv
    #'
    #' @return Calculate rolling radf features from theft package.
    rolling_function = function(data, window, price) {

      # debugging
      # y <<- data

      # check if there is enough data
      if (length(unique(data$symbol)) > 1) {
        print(paste0("not enough data for symbol ", data$symbol[1]))
        return(NA)
      }

      # calculate features
      y <- as.data.table(calculate_features(data, "symbol", "date", price, feature_set = self$features_set, tsfresh_cleanup = TRUE))
      y[, var_names := paste(method, names, window, sep = "_")]
      y <- transpose(y[, .(var_names, values)], make.names = TRUE)
      results <- data.table(symbol = data$symbol[1], date = data$date[length(data$date)], y)
      results
    }
  )
)
