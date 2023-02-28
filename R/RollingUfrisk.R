#' @title RollingUfrisk Class
#'
#' @description
#' Function calculates risk measures (Var, ES) from ufRisk package on rolling window.
#'
#' @export
#' @examples
#' data(spy_hour)
#' OhlcvInstance = Ohlcv$new(spy_hour, date_col = "datetime")
#' RollingUfriskInit = RollingUfrisk$new(windows = 200,
#'                                       workers = 1L,
#'                                       at = c(300, 500),
#'                                       lag = 0L,
#'                                       a_v = 0.99,
#'                                       a_e = 0.975,
#'                                       model = "sGARCH",
#'                                       garch_order = c(1, 1))
#' x = RollingUfriskInit$get_rolling_features(OhlcvInstance)
#' head(x)
#' RollingUfriskInit = RollingUfrisk$new(windows = 200,
#'                                       workers = 2L,
#'                                       at = c(300, 500),
#'                                       lag = 0L,
#'                                       a_v = 0.99,
#'                                       a_e = 0.975,
#'                                       model = c("sGARCH", "eGARCH"),
#'                                       garch_order = c(1, 1))
#' x = RollingUfriskInit$get_rolling_features(OhlcvInstance)
#' head(x)
RollingUfrisk = R6::R6Class(
  "RollingUfrisk",
  inherit = RollingGeneric,

  public = list(

    #' @field a_v Argument a.v in ufRisk package
    a_v = NULL,

    #' @field a_e Argument a.e in ufRisk package
    a_e = NULL,

    #' @field model Argument model in ufRisk package
    model = NULL,

    #' @field garch_order Argument garchOrder in ufRisk package
    garch_order = NULL,

    #' @description
    #' Create a new RollingExuber object.
    #'
    #' @param windows Vector of windows that will be applied on features.
    #' @param workers Number of workers. Greater than 1 for parallle processing
    #' @param lag Lag variable in runner package.
    #' @param at Argument at in runner package.
    #' @param a_v Argument a.v in ufRisk package
    #' @param a_e Argument a.e in ufRisk package
    #' @param model Argument model in ufRisk package
    #' @param garch_order Argument garch_order in ufRisk package
    #'
    #' @return A new `RollingQuarks` object.
    initialize = function(windows,
                          workers,
                          lag,
                          at,
                          a_v = 0.99,
                          a_e = 0.975,
                          model = c("sGARCH", "eGARCH", "apARCH", "fiGARCH", "filGARCH"),
                          garch_order = c(1, 1)
    ) {

      # define all params combination
      private$params <- expand.grid(a_v = a_v,
                                    a_e = a_e,
                                    model = model,
                                    garch_order = garch_order,
                                    stringsAsFactors = FALSE)
      colnames(private$params) <- c("turbo")

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

      # calculate Var and ES
      y <- tryCatch({
        ufRisk::varcast(x[, get(price_col)], a.v = params$a_v, a.e = params$a_e,
                        model = params$model, garchOrder = params$garch_order,
                        n.out = 1)
      }, error = function(e) NULL)

      # check if there are errors
      if (is.null(y)) {
        risk_measures <- data.frame(es = NA, var_e = NA, var_v = NA)
        colnames(risk_measures) <- paste0("ufrisk", "_", colnames(risk_measures),
                                          "_", window, "_", paste0(params, collapse = "_"))
      } else {
        # clean results
        risk_measures <- data.frame(es = y$ES, var_e = y$VaR.e, var_v = y$VaR.v)
        colnames(risk_measures) <- paste0("ufrisk", "_", colnames(risk_measures),
                                          "_", window, "_", paste0(params, collapse = "_"))
      }
      return(as.data.table(risk_measures))
    }
  ),

  private = list(
    packages = "ufRisk",
    params = NULL
  )
)
