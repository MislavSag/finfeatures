#' @title RollingGeneric Class
#'
#' @description
#' This is the abstract base class for tasks that start with "Rolling".
#' It helps calculating rolling values of features.
#'
#' @export
RollingGeneric = R6::R6Class(
  "RollingGeneric",

  public = list(

    #' @field windows Vector of windows that will be applied on features.
    windows = NULL,

    #' @field workers Number of workers. Greater than 1 for parallle processing
    workers = NULL,

    #' @field lag Lag variable in runner package.
    lag = NULL,

    #' @field at Argument at in runner package.
    at = NULL,

    #' @field packages Packages that needs to be installed.
    packages = NULL,

    #' @description
    #' Create a new RollingGeneric object.
    #'
    #' @param windows Vector of windows that will be applied on features.
    #' @param workers Number of workers. Greater than 1 for parallle processing
    #' @param lag Lag variable in runner package.
    #' @param at Argument at in runner package.
    #' @param packages Packages that need to be installed.
    #'
    #' @return A new `RollingGeneric` object.
    initialize = function(windows = 10,
                          workers = 1L,
                          lag = 0L,
                          at = integer(0),
                          packages = NULL) {

      # attributes
      self$windows = windows
      self$workers = workers
      self$at = at
      self$lag = lag
      self$packages = packages
    },

    #' @description
    #' This function will be used in application. For now it doesn't do anythin
    #'
    #' @param x Ohlcv object.
    #' @param window Rolling window lengths.
    #' @param price_col Price column in Ohlcv
    #' @param params Vector of parameters
    #'
    #' @return Depending on used classes for clalculating features. It returns df.
    rolling_function = function(x, window, price_col, params) {
      NULL
    },

    #' @description
    #' Helping function for calculating rolling features.
    #'
    #' @param Ohlcv Ohlcv object
    #' @param log_prices If TRUE, we log OHLC.
    #' @param price_col Price column in Ohlcv
    #'
    #' @return Depending on used classes for clalculating features. It returns df.
    get_rolling_features = function(Ohlcv, log_prices = FALSE, price_col = NULL) {

      # check if necessary packages are installed
      installed = all(sapply(self$packages, requireNamespace, quietly = TRUE))
      if (!(installed)) {
        message(paste0("You need to install the packages ",
                       self$packages, " to use this class."))
        return(NULL)
      }

      # Check if price_col is in columns of Ohlcv$X using checkmate package
      assert_subset(price_col, colnames(Ohlcv$X))

      # get data
      # library(finfeatures)
      # data(spy_hour)
      # setDT(spy_hour)
      # setnames(spy_hour, "datetime", "date")
      # ohlcv = Ohlcv$new(spy_hour)
      # class(ohlcv)
      if (test_class(Ohlcv, "Ohlcv")) {
        data = copy(Ohlcv$X)
      } else if (test_class(Ohlcv, "data.table")) {
        data = copy(Ohlcv)
      } else {
        stop("Ohlcv object should be either Ohlcv or data.table class.")
      }

      # log
      if (log_prices) {
        cols = c("open", "high", "low", "close")
        data[, (cols) := lapply(.SD, log), .SDcols = cols]
      }

      # Price col
      if (is.null(price_col) & test_class(Ohlcv, "Ohlcv")) {
        price_col = Ohlcv$price
      } else if (is.null(price_col) & test_class(Ohlcv, "data.table")) {
        price_col = "close"
      }

      # start cluster if workers greater than 1
      if (self$workers > 1) {
        if (.Platform$OS.type == "windows") {
          cl = parallel::makeCluster(self$workers)
          parallel::clusterExport(cl, c("data"), envir = environment())
          parallel::clusterCall(cl, function() lapply(self$packages, require, character.only = TRUE))
        } else if (.Platform$OS.type == "unix") {
          cl <- parallel::makeForkCluster(nnodes=self$workers)
        }
      } else {
        cl <- NULL
      }

      # calculation
      predictors_l <- lapply(1:nrow(private$params), function(i) {
        # params
        params_ <- private$params[i, ]

        # calculate rolling predictors for parameters
        data[self$at, cbind(symbol, date, do.call(cbind, lapply(self$windows, function(w) {
          rbindlist(lapply(runner(
            x = copy(data),
            f = function(x) self$rolling_function(x, w, price_col, params_), # x, window, price_col, params
            k = w,
            lag = self$lag,
            at = self$at,
            na_pad = TRUE,
            simplify = FALSE,
            cl = cl
          ), as.data.table), fill = TRUE)[, V1 := NULL]})))]
      })
      predictors <- Reduce(function(x, y) merge(x, y, by = c("symbol", "date")),
                           predictors_l)

      # clean column names
      colnames(predictors) <- gsub(" |-|\\.|\"", "_", colnames(predictors))
      colnames(predictors) <- gsub("__", "_", colnames(predictors))

      # stop connection
      if (self$workers > 1) {
        parallel::stopCluster(cl)
        # stopImplicitCluster()
      }

      # add log to column names if used
      if (log_prices) {
        colnames(predictors)[3:ncol(predictors)] <- paste0(colnames(predictors)[3:ncol(predictors)], "_log")
      }

      return(predictors)
    }
  ),
  private = list(
    params = NULL
  )
)
