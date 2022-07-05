#' @title RollingQuarks Class
#'
#' @description
#' Function calculates risk measures (Var, ES) from quarks package on rolling window.
#'
#' @export
#' @examples
#' data(spy_hour)
#' OhlcvInstance = Ohlcv$new(spy_hour, date_col = "datetime")
#' RollingQuarksInit = RollingQuarks$new(windows = 200,
#'                                       workers = 1L,
#'                                       at = c(300:310, 500:510),
#'                                       lag = 1L,
#'                                       na_pad = TRUE,
#'                                       simplify = FALSE)
#' x = RollingQuarksInit$get_rolling_features(OhlcvInstance)
#' head(x)
#' # parallel and multiple windows
#' RollingQuarksInit = RollingQuarks$new(windows = c(200, 400),
#'                                       workers = 2L,
#'                                       at = c(300:310, 500:510),
#'                                       lag = 1L,
#'                                       na_pad = TRUE,
#'                                       simplify = FALSE)
#' x = RollingQuarksInit$get_rolling_features(OhlcvInstance)
#' head(x)
RollingQuarks = R6::R6Class(
  "RollingQuarks",
  inherit = RollingGeneric,

  public = list(

    #' @field p Argument p in quarks package
    p = NULL,

    #' @field model Argument model in quarks package
    model = NULL,

    #' @field method Argument method in quarks package
    method = NULL,

    #' @description
    #' Create a new RollingExuber object.
    #'
    #' @param windows Vector of windows that will be applied on features.
    #' @param workers Number of workers. Greater than 1 for parallle processing
    #' @param lag Lag variable in runner package.
    #' @param at Argument at in runner package.
    #' @param na_pad Argument na_pad in runner package.
    #' @param simplify Argument simplify in runner package.
    #' @param p Argument p in quarks package
    #' @param model Argument model in quarks package
    #' @param method Argument method in quarks package
    #'
    #' @return A new `RollingQuarks` object.
    initialize = function(windows, workers, lag, at, na_pad, simplify,
                          p = 0.975, model = c("EWMA", "GARCH"),
                          method = c("plain", "age") # "vwhs", "fhs"
                          ) {
      self$p = p
      self$model = model
      self$method = method

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
    #' Function calculates risk measures (Var, ES) from quarks package on rolling window.
    #'
    #' @param data X field of Ohlcv object
    #' @param window window length. This argument is given internaly
    #' @param price Prcie column in Ohlcv
    #'
    #' @return Calculate rolling radf features from exuber package.
    rolling_function = function(data, window, price) {

      # check if there is enough data
      if (length(unique(data$symbol)) > 1) {
        print(paste0("not enough data for symbol ", data$symbol[1]))
        return(NA)
      }
      # p = c(0.95, 0.975, 0.99)
      # model = c("EWMA", "GARCH")
      # method = c("plain", "age", "vwhs", "fhs")

      p = self$p
      model = self$model
      method = self$method
      params <- expand.grid(p, model, method, stringsAsFactors = FALSE)
      colnames(params) <- c("p", "model", "method")
      params <- params[!(params$model == 'GARCH' & params$method == "fhs"), ]

      # get forecasts
      results_l <- list()
      for (i in 1:nrow(params)) {

        # params
        params_ <- params[i, ]

        # create file name
        params_$p <- params_$p * 1000
        col_name <- paste0(paste(params_, sep = "_"), collapse = "_")
        params_$p <- params_$p / 1000

        if (params_$method == "fhs") {
          y <- quarks::rollcast(data[, get(price)], # data[, get(price)],
                                p = params_$p,
                                model = params_$model,
                                method = params_$method,
                                nout = 1L,
                                nwin = window - 1,
                                nboot = 1000
          )
        } else {
          y <- quarks::rollcast(data[, get(price)], # data[, get(price)],
                                p = params_$p,
                                model = params_$model,
                                method = params_$method,
                                nout = 1L,
                                nwin = window - 1,
          )
        }
        VaR <- (y$xout - abs(y$VaR)) / y$xout
        ES <- (y$xout - abs(y$ES)) / y$xout
        cols <- c(paste0("var_", col_name), paste0("es_", col_name))
        risk_measures <- data.table(VaR, ES)
        colnames(risk_measures) <- paste0(cols, "_", window)
        results_l[[i]] <- risk_measures
      }
      result <- do.call(cbind, results_l)
      result <- cbind(symbol = data$symbol[1], date = data$date[length(data$date)], result)
      return(as.data.table(result))
    }
  ),

  private = list(
    packages = "quarks"
  )
)
