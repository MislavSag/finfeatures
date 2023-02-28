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
#'                                       lag = 1L)
#' x = RollingQuarksInit$get_rolling_features(OhlcvInstance)
#' head(x)
#' # parallel and multiple windows and vector arguments
#' RollingQuarksInit = RollingQuarks$new(windows = c(200, 400),
#'                                       workers = 2L,
#'                                       at = c(300:310, 500:510),
#'                                       lag = 1L,
#'                                       p = c(0.95, 0.975))
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
    #' @param p Argument p in quarks package
    #' @param model Argument model in quarks package
    #' @param method Argument method in quarks package
    #'
    #' @return A new `RollingQuarks` object.
    initialize = function(windows,
                          workers,
                          lag,
                          at,
                          p = 0.975,
                          model = c("EWMA", "GARCH"),
                          method = c("plain", "age") # "vwhs", "fhs"
    ) {

      # define all params combination
      private$params <- expand.grid(p = p,
                                    model = model,
                                    method = method, stringsAsFactors = FALSE)
      colnames(private$params) <- c("p", "model", "method")

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

      # create file name
      params$p <- params$p * 1000
      col_name <- paste0(paste(params, sep = "_"), collapse = "_")
      params$p <- params$p / 1000

      if (params$method == "fhs") {
        y <- quarks::rollcast(x[, get(price_col)],
                              p = params$p,
                              model = params$model,
                              method = params$method,
                              nout = 1L,
                              nwin = window - 1,
                              nboot = 1000
        )
      } else {
        y <- quarks::rollcast(x[, get(price_col)],
                              p = params$p,
                              model = params$model,
                              method = params$method,
                              nout = 1L,
                              nwin = window - 1,
        )
      }
      VaR <- (y$xout - abs(y$VaR)) / y$xout
      ES <- (y$xout - abs(y$ES)) / y$xout
      cols <- c(paste0("var_", col_name), paste0("es_", col_name))
      risk_measures <- data.table(VaR, ES)
      colnames(risk_measures) <- paste0(cols, "_", window)
      return(as.data.table(risk_measures))
    }
  ),

  private = list(
    packages = "quarks",
    params = NULL
  )
)
