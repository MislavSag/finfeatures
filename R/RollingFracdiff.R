#' @title RollingFracdiff Class
#'
#' @description
#' Function calculates coefficients of ARFIMA modelfrom fracdiff package.
#'
#' @export
#' @examples
#' data(spy_hour)
#' OhlcvInstance = Ohlcv$new(spy_hour, date_col = "datetime")
#' RollingFracdiffInit = RollingFracdiff$new(windows = 200,
#'                                           workers = 1L,
#'                                           at = c(300, 500),
#'                                           lag = 0L,
#'                                           nar = 1,
#'                                           nma = 1,
#'                                           bandw_exp = c(0.1, 0.2))
#' x = RollingFracdiffInit$get_rolling_features(OhlcvInstance)
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
    #' @param nar Argument nar in fracdiff function.
    #' @param nma Argument nam in fracdiff function.
    #' @param bandw_exp Argument bandw.exp in fracdiff function.
    #'
    #' @return A new `RollingFracdiff` object.
    initialize = function(windows,
                          workers,
                          lag,
                          at,
                          nar = c(1, 2),
                          nma = c(1, 2),
                          bandw_exp = c(0.1, 0.5, 0.9)) {

      # checks
      if (length(nar) != length(nma)) stop("nar and nam must be of the same length")

      # define all params combination
      private$params <- expand.grid(nar = nar,
                                    nma = nma,
                                    bandw_exp = bandw_exp,
                                    stringsAsFactors = FALSE)
      colnames(private$params) <- c("nar", "nma", "bandw_exp")

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

      # calculate arfima coefficitents
      fitted <- lapply(seq_along(params$nar), function(i) {
        fracdiff::fracdiff(x[, get(price_col)], nar = params$nar[i], nma = params$nma[i])
      })
      fitted_coefs <- lapply(fitted, coef)
      for (i in seq_along(fitted_coefs)) {
        names(fitted_coefs[[i]]) <- paste0(names(fitted_coefs[[i]]), "_", i)
      }
      vars <- do.call(c, fitted_coefs)

      # calculate d
      ds <- lapply(params$bandw_exp, function(be) {
        fdGPH_ <- as.data.frame(fracdiff::fdGPH(x[, get(price_col)], bandw.exp = be))
        colnames(fdGPH_) <- paste0(colnames(fdGPH_), "_fdGPH_", be)
        fdSperio_ <- as.data.frame(fracdiff::fdSperio(x[, get(price_col)], bandw.exp = be, beta = 0.9))
        colnames(fdSperio_) <- paste0(colnames(fdSperio_), "_fdSperio_", be)
        cbind(fdGPH_, fdSperio_)
      })
      vars_ds <- unlist(ds)
      names(vars_ds) <- gsub("//.", "_", names(vars_ds))
      vars <- c(vars, vars_ds)

      # clean names
      names(vars) <- paste0(names(vars), "_", window)
      data.table(t(vars))
    }
  ),
  private = list(
    packages = "fracdiff",
    params = NULL
  )
)
