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
#'                                               scale = TRUE)
#' x = RollingTsfeaturesInit$get_rolling_features(OhlcvInstance)
#' head(x)
#' RollingTsfeaturesInit = RollingTsfeatures$new(windows = 200,
#'                                               workers = 2L,
#'                                               at = c(300, 500),
#'                                               lag = 0L,
#'                                               scale = c(TRUE, FALSE))
#' x = RollingTsfeaturesInit$get_rolling_features(OhlcvInstance)
#' head(x)
# EROR: 8 nodes produced errors; first error: This time series is too short. Specify proper segment length in `l`
RollingTsfeatures = R6::R6Class(
  "RollingTsfeatures",
  inherit = RollingGeneric,

  public = list(

    #' @field scale Check argument of tsfeatures function.
    scale = NULL,

    #' @description
    #' Create a new RollingTsfeatures object.
    #'
    #' @param windows Vector of windows that will be applied on features.
    #' @param workers Number of workers. Greater than 1 for parallle processing
    #' @param lag Lag variable in runner package.
    #' @param at Argument at in runner package.
    #' @param scale Check argument of tsfeatures function.
    #'
    #' @return A new `RollingTsfeatures` object.
    initialize = function(windows,
                          workers,
                          lag,
                          at,
                          scale) {

      # define all params combination
      private$params <- expand.grid(scale = scale, stringsAsFactors = FALSE)
      colnames(private$params) <- c("scale")

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

      # remove spreadrandomlocal_meantaul because of error:
      # This time series is too short. Specify proper segment length in `l`
      # IF GETTING ABOVE ERROR, INSTALL DEVELOPMENT VERSION OF THE PACKAGE, SEE
      # MY ISSUE ON GITHUB.
      featureList <- c("stl_features", "entropy", "acf_features",
                       "compengine", "arch_stat", "crossing_points", "flat_spots",
                       "heterogeneity", "holt_parameters", "hurst",
                       "lumpiness", "max_kl_shift", "max_level_shift", "max_var_shift",
                       "nonlinearity", "pacf_features", "stability", "unitroot_kpss",
                       "unitroot_pp", "embed2_incircle", "firstzero_ac",
                       "histogram_mode", "localsimple_taures", "sampenc")
      y <- as.data.table(tsfeatures::tsfeatures(x[, get(price_col)],
                                                features = featureList,
                                                scale = params))
      colnames(y) <- paste(colnames(y), window, params, sep = "_")
      colnames(y) <- gsub(" |-", "_", colnames(y))
      y
    }
  ),

  private = list(
    packages = "tsfeatures",
    params = NULL
  )
)
