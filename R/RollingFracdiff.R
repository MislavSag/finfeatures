#' @title RollingFracdiff Class
#'
#' @description
#' Function calculates coefficients of ARFIMA modelfrom fracdiff package.
#'
#' @export
#' @examples
#' data(spy_hour)
#' OhlcvInstance = Ohlcv$new(spy_hour, date_col = "datetime")
#' #arima
#' RollingForecatsInstance = RollingForecats$new(windows = c(10, 20),
#'                                               workers = 1L,
#'                                               lag = 1L,
#'                                               at = c(100:110, 200:210),
#'                                               na_pad = TRUE,
#'                                               simplify = FALSE,
#'                                               forecast_type = "autoarima",
#'                                               h = 22)
#' x = RollingForecatsInstance$get_rolling_features(OhlcvInstance)
#' head(x)
RollingFracdiff = R6::R6Class(
  "RollingFracdiff",
  inherit = RollingGeneric,

  public = list(

    #' @field nar Argument nar in fracdiff function.
    nar = NULL,

    #' @field nam Argument nam in fracdiff function.
    nma = NULL,

    #' @field bandw_exp Argument bandw.exp in fracdiff function.
    bandw_exp = NULL,

    #' @description
    #' Create a new RollingFracdiff object.
    #'
    #' @param windows Vector of windows that will be applied on features.
    #' @param workers Number of threads.
    #' @param lag Argument lag in runner package.
    #' @param at Argument at in runner package.
    #' @param na_pad Argument na_pad in runner package.
    #' @param simplify Argument simplify in runner package.
    #' @param nar Argument nar in fracdiff function.
    #' @param nma Argument nam in fracdiff function.
    #' @param bandw_exp Argument bandw.exp in fracdiff function.
    #'
    #' @return A new `RollingFracdiff` object.
    initialize = function(windows, workers, lag, at, na_pad, simplify,
                          nar = c(1, 2), nma = c(1, 2), bandw_exp = c(0.1, 0.5, 0.9)) {

      # checks
      if (length(nar) != length(nam)) stop("nar and nam must be of the same length")

      # parameters
      self$nar = nar
      self$nma = nma
      self$bandw_exp = bandw_exp

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

      # calculate arfima coefficitents
      # price <- spy_hour$close[1:5000]
      fitted <- lapply(seq_along(nar), function(i) {
        fracdiff::fracdiff(data[, get(price)], nar = nar[i], nma = nma[i])
      })
      fitted_coefs <- lapply(fitted, coef)
      for (i in seq_along(fitted_coefs)) {
        names(fitted_coefs[[i]]) <- paste0(names(fitted_coefs[[i]]), "_", i)
      }
      vars <- do.call(c, fitted_coefs)

      # calculate d
      ds <- lapply(bandw_exp, function(be) {
        fdGPH_ <- as.data.frame(fracdiff::fdGPH(data[, get(price)], bandw.exp = be))
        colnames(fdGPH_) <- paste0(colnames(fdGPH_), "_fdGPH_", be)
        fdSperio_ <- as.data.frame(fracdiff::fdSperio(data[, get(price)], bandw.exp = be, beta = 0.9))
        colnames(fdSperio_) <- paste0(colnames(fdSperio_), "_fdSperio_", be)
        cbind(fdGPH_, fdSperio_)
      })
      vars_ds <- unlist(ds)
      names(vars_ds) <- gsub("//.", "_", names(vars_ds))
      vars <- c(vars, vars_ds)

      # clean names
      names(vars) <- paste0(names(vars), "_", window)
      data.table(symbol = data$symbol[1], date = data$date[length(data$date)], t(vars))
    }
  ),
  private = list(
    packages = "fracdiff"
  )
)
