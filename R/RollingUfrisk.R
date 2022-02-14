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
#'                                       at = c(300:310, 500:510),
#'                                       lag = 1L,
#'                                       na_pad = TRUE,
#'                                       simplify = FALSE)
#' x = RollingUfriskInit$get_rolling_features(OhlcvInstance)
#' head(x)
#' # parallel and multiple windows
#' RollingUfriskInit = RollingUfrisk$new(windows = c(200, 400),
#'                                       workers = 2L,
#'                                       at = c(300:310, 500:510),
#'                                       lag = 1L,
#'                                       na_pad = TRUE,
#'                                       simplify = FALSE)
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
    #' @param na_pad Argument na_pad in runner package.
    #' @param simplify Argument simplify in runner package.
    #' @param a_v Argument a.v in ufRisk package
    #' @param a_e Argument a.e in ufRisk package
    #' @param model Argument model in ufRisk package
    #' @param garch_order Argument garch_order in ufRisk package
    #'
    #' @return A new `RollingQuarks` object.
    initialize = function(windows, workers, lag, at, na_pad, simplify,
                          a_v = 0.99, a_e = 0.975,
                          model = c("sGARCH", "eGARCH", "apARCH", "fiGARCH", "filGARCH"),
                          garch_order = c(1, 1)
    ) {
      self$a_v = a_v
      self$a_e = a_e
      self$model = model
      self$garch_order = garch_order

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
    #' Function calculates risk measures (Var, ES) from ufRisk package on rolling window.
    #'
    #' @param data X field of Ohlcv object
    #' @param window window length. This argument is given internaly
    #' @param price Prcie column in Ohlcv
    #'
    #' @return Calculate rolling Var and ES features from ufRisk package.
    rolling_function = function(data, window, price) {

      # check if there is enough data
      if (length(unique(data$symbol)) > 1) {
        print(paste0("not enough data for symbol ", data$symbol[1]))
        return(NA)
      }

      model = c("sGARCH", "eGARCH", "apARCH", "fiGARCH", "filGARCH") #self$model

      # get forecasts
      results_l <- list()
      for (i in 1:length(model)) {
        # calculate Var and ES
        y = varcast(data[, get(price)], a.v = self$a_v, a.e = self$a_e,
                    model = model[i], garchOrder = self$garch_order, n.out = 1)

        # DEBUG
        # y = varcast(spy_hour$close[1:1000], a.v = 0.99, a.e = 0.975,
        #             model = model[i], garchOrder = c(1,1), n.out = 1)

        # clean results
        risk_measures <- data.frame(es = y$ES, var_e = y$VaR.e, var_v = y$VaR.v)
        colnames(risk_measures) <- paste0("ufrisk_", model[i], "_", colnames(risk_measures), "_", window)
        results_l[[i]] <- risk_measures
      }
      results_l <- lapply(results_l, function(x) x[1, ])
      result <- do.call(cbind, results_l)
      result <- cbind(symbol = data$symbol[1], date = tail(data$date, 1), result)
      return(as.data.table(result))
    }
  )
)
