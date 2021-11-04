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

    #' @field na_pad Argument na_pad from runner package
    na_pad = NULL,

    #' @field simplify Argument simplify from runner package.
    simplify = NULL,

    #' @description
    #' Create a new RollingGeneric object.
    #'
    #' @param windows Vector of windows that will be applied on features.
    #' @param workers Number of workers. Greater than 1 for parallle processing
    #' @param lag Lag variable in runner package.
    #' @param at Argument at in runner package.
    #' @param na_pad Argument na_pad in runner package.
    #' @param simplify Argument simplify in runner package.
    #'
    #' @return A new `RollingGeneric` object.
    initialize = function(windows = 10, workers = 1L, lag = integer(1), at = integer(0), na_pad = TRUE, simplify = FALSE) {
      self$windows = windows
      self$workers = workers
      self$at = at
      self$lag = lag
      self$na_pad = na_pad
      self$simplify = simplify
    },

    #' @description
    #' This function will be used in application. For now it doesn't do anythin
    #'
    #' @param x Ohlcv object.
    #' @param window Rolling window lengths.
    #' @param price Prcie column in Ohlcv
    #'
    #' @return Depending on used classes for clalculating features. It returns df.
    rolling_function = function(x, window, price_col) {
      NULL
    },

    #' @description
    #' Helping function for calculating rolling features.
    #'
    #' @param Ohlcv Ohlcv object
    #'
    #' @return Depending on used classes for clalculating features. It returns df.
    get_rolling_features = function(Ohlcv) {

      # get data
      data = Ohlcv$X

      # start cluser if workers greater than 1
      if (self$workers > 1) {
        cl <- makeCluster(self$workers)
        registerDoParallel(cl)
        clusterExport(cl, c("data"), envir = environment())
        clusterCall(cl, function() lapply(private$packages, require, character.only = TRUE))
      } else {
        cl <- NULL
      }

      # runner function
      data_list <- list()
      for (i in 1:length(self$windows)) {
        rolling_features = runner(
          x = data,
          f = function(x) self$rolling_function(x, self$windows[i], Ohlcv$price),
          k = self$windows[i],
          lag = self$lag,
          at = self$at,
          na_pad = self$na_pad,
          simplify = self$simplify,
          cl = cl
        )
        data_list[[i]] <- rbindlist(rolling_features[lengths(rolling_features) > 1L])
      }

      # stop connection
      if (self$workers > 1) {
        stopCluster(cl)
        stopImplicitCluster(cl)
      }

      # merge all results
      data_all_windows <- Reduce(function(x, y) merge(x, y, by = c("symbol", "date"), all.x = TRUE, all.y = TRUE), data_list)
      return(data_all_windows)
    }
  ),

  private = list(

    packages = c("data.table", "exuber", "backcCUSUM")
  )
)
