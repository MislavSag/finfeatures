#' @title RollingTsfeatures Class
#'
#' @description
#' Function calculates tsfeatures features on rolling window.
#'
#' @export
#' @examples
#' data(spy_hour)
#' OhlcvInstance = Ohlcv$new(spy_hour, date_col = "datetime")
#' RollingTsfeaturesInit = RollingTsfeatures$new(windows = 200,
#'                                               workers = 1L,
#'                                               at = c(300, 500),
#'                                               lag = 0L,
#'                                               na_pad = TRUE,
#'                                               simplify = FALSE)
#' x = RollingTsfeaturesInit$get_rolling_features(OhlcvInstance)
#' head(x)
# EROR: 8 nodes produced errors; first error: This time series is too short. Specify proper segment length in `l`
RollingTsfeatures = R6::R6Class(
  "RollingTsfeatures",
  inherit = RollingGeneric,

  public = list(

    #' @description
    #' Create a new RollingTsfeatures object.
    #'
    #' @param windows Vector of windows that will be applied on features.
    #' @param workers Number of workers. Greater than 1 for parallle processing
    #' @param lag Lag variable in runner package.
    #' @param at Argument at in runner package.
    #' @param na_pad Argument na_pad in runner package.
    #' @param simplify Argument simplify in runner package.
    #'
    #' @return A new `RollingTsfeatures` object.
    initialize = function(windows, workers, lag, at, na_pad, simplify) {

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
    #' Function calculates tsfeatures features on rolling window.
    #'
    #' @param data X field of Ohlcv object
    #' @param window window length. This argument is given internaly
    #' @param price Prcie column in Ohlcv
    #'
    #' @return Calculate rolling tsfeatures features from tsfeatures package.
    rolling_function = function(data, window, price) {

      # check if there is enough data
      if (length(unique(data$symbol)) > 1) {
        print(paste0("not enough data for symbol ", data$symbol[1]))
        return(NA)
      }

      # DEBUG
      # print(head(data))
      # print(data[, get(price)])
      # test_ <<- data[, get(price)]
      # DEBUG

      featureList <- c("stl_features", "entropy", "acf_features",
                       "compengine", "arch_stat", "crossing_points", "flat_spots",
                       "heterogeneity", "holt_parameters", "hurst",
                       "lumpiness", "max_kl_shift", "max_level_shift", "max_var_shift",
                       "nonlinearity", "pacf_features", "stability", "unitroot_kpss",
                       "unitroot_pp", "embed2_incircle", "firstzero_ac",
                       "histogram_mode", "localsimple_taures", "sampenc",
                       "spreadrandomlocal_meantaul")
      y <- as.data.table(tsfeatures(data[, get(price)], features = featureList))
      colnames(y) <- paste(colnames(y), window, sep = "_")
      results <- data.table(symbol = data$symbol[1], date = data$date[length(data$date)], y)
      colnames(results) <- gsub(" |-", "_", colnames(results))

      ################## DEBUG ###############
      # test__ <<- data[, get(price)]
      ################## DEBUG ###############

      results
    }
  )
)
