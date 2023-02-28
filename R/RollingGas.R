#' @title RollingGas Class
#'
#' @description
#' Function calculates GAS risk values from GAS package on rolling window.
#'
#' @export
#' @examples
#' data(spy_hour)
#' OhlcvInstance = Ohlcv$new(spy_hour, date_col = "datetime")
#' RollingGasInit = RollingGas$new(windows = 200,
#'                                 workers = 1L,
#'                                 at = c(300, 500),
#'                                 lag = 0L,
#'                                 gas_dist = "sstd",
#'                                 gas_scaling = "Identity",
#'                                 prediction_horizont = 10)
#' x = RollingGasInit$get_rolling_features(OhlcvInstance)
#' head(x)
#' RollingGasInit = RollingGas$new(windows = 200,
#'                                 workers = 2L,
#'                                 at = c(300, 500),
#'                                 lag = 0L,
#'                                 gas_dist = c("std", "sstd"),
#'                                 gas_scaling = "Identity",
#'                                 prediction_horizont = 10)
#' x = RollingGasInit$get_rolling_features(OhlcvInstance)
#' head(x)
RollingGas = R6::R6Class(
  "RollingGas",
  inherit = RollingGeneric,

  public = list(

    #' @field gas_dist Dist parametere in UniGASSpec fucntionUniGASSpec.
    gas_dist = NULL,

    #' @field gas_scaling Scaling parametere in UniGASSpec fucntionUniGASSpec.
    gas_scaling = NULL,

    #' @field prediction_horizont GAS prediction horizont.
    prediction_horizont = NULL,

    #' @field GASSpec Defined internally
    GASSpec = NULL,

    #' @description
    #' Create a new RollingExuber object.
    #'
    #' @param windows Vector of windows that will be applied on features.
    #' @param workers Number of workers. Greater than 1 for parallle processing
    #' @param lag Lag variable in runner package.
    #' @param at Argument at in runner package.
    #' @param gas_dist Dist parametere in UniGASSpec fucntionUniGASSpec.
    #' @param gas_scaling Scaling parametere in UniGASSpec fucntionUniGASSpec.
    #' @param prediction_horizont GAS prediction horizont.
    #'
    #' @return A new `RollingGas` object.
    initialize = function(windows,
                          workers,
                          lag,
                          at,
                          gas_dist = "sstd",
                          gas_scaling = "Identity",
                          prediction_horizont = 22) {

      # define all params combination
      private$params <- expand.grid(gas_dist = gas_dist,
                                    gas_scaling = gas_scaling,
                                    prediction_horizont = prediction_horizont,
                                    stringsAsFactors = FALSE)
      colnames(private$params) <- c("gas_dist", "gas_scaling", "prediction_horizont")

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
    #' Help function for Rolling GAS
    #'
    #' @param df A data.table object with GAS results.
    #' @param colname_prefix Prefix for column names
    #'
    #' @return Calculate rolling GAS features from GAS package.
    get_series_statistics = function(df, colname_prefix = "var") {

      # solve No visible binding for global variable
      id = value = col_name = `.` = variable = NULL

      # calculate statistics
      stats <- lapply(df, function(x) {
        var_1 <- x[1]
        var_subsample <- mean(x[1:(length(x)/2)], na.rm = TRUE)
        var_all <- mean(x, na.rm = TRUE)
        var_std <- sd(x, na.rm = TRUE)
        list(var_1 = var_1, var_subsample = var_subsample, var_all = var_all, var_std = var_std)
      })
      stats <- melt(rbindlist(stats, idcol = "id"), id.vars = "id")
      stats[, col_name := paste(variable, gsub("\\.", "_", id), sep = "_")]
      stats <- transpose(stats[, .(col_name, value)], make.names = TRUE)
      colnames(stats) <- gsub("var", colname_prefix, colnames(stats))
      return(stats)
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

      # GAS specification
      GASSpec <- GAS::UniGASSpec(Dist = params$gas_dist,
                                 ScalingType = params$gas_scaling,
                                 GASPar = list(location = TRUE,
                                               scale = TRUE,
                                               shape = TRUE,
                                               skewness = TRUE))


      # calculate arima forecasts
      Fit <- tryCatch(GAS::UniGASFit(GASSpec, na.omit(x$returns)),
                      error = function(e) NA)
      if (isS4(Fit)) y <- GAS::UniGASFor(Fit, H = params$prediction_horizont, ReturnDraws = TRUE) else y <- NA
      if ((!isS4(Fit) && is.na(y)) || any(is.na(y@Draws))) {
        return(NA)
      } else {
        q <- as.data.table(GAS::quantile(y, c(0.01, 0.05)))
        q <- self$get_series_statistics(q, "var")
        es <- as.data.table(GAS::ES(y, c(0.01, 0.05)))
        es <- self$get_series_statistics(es, "es")
        moments <- as.data.table(GAS::getMoments(y))
        moments <- self$get_series_statistics(moments, "moments")
        f <- as.data.table(GAS::getForecast(y))
        f <- self$get_series_statistics(f, "f")
        results <- cbind(q, es, moments, f)
        colnames(results) <- paste(colnames(results), window,
                                   paste0(params, collapse = "_"), sep = "_")
        return(results)
      }
    }
  ),

  private = list(
    packages = "GAS",
    params = NULL
  )
)
