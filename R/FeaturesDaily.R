#' @title Daily features
#'
#' @description
#' Calculate features specified in config file.
#'
#' @export
FeaturesDaily = R6::R6Class(
  "FeaturesDaily",

  public = list(

    #' @description
    #' Create a new Features object.
    #'
    #' @param path Path to the data
    #' @param features_config AWS S3 Tiledb config
    #' @param at Row indecies for wich we calcilate the features. If NULL,
    #'     it will be calculated for all rows.
    #'
    #' @return A new `FeaturesDaily` object.
    initialize = function(ohlcv, path, features_config, at = NULL) {
      # Debug
      # library(yaml)
      # library(checkmate)
      # library(finfeatures)
      # library(data.table)
      # path = "F:/predictors/test"
      # features_config = "inst/extdata/features_config.yaml"
      # at = c(5, 10)
      # self = list()
      # data(spy_hour)
      # dt = as.data.table(spy_hour[1:102, ])
      # ohlcv = Ohlcv$new(dt, date_col = "datetime")


      # Read yaml config
      features_conf = yaml.load_file(features_config)
      features_conf = lapply(features_conf, function(x) {x$at = at; x})

      # Set variables
      self$path = assert_directory(path)
      self$features_conf = features_conf
      self$ohlcv = assert_class(ohlcv, "Ohlcv")
    },

    #' @description
    #' Function calculates forecastas based on auto.arima and nnetar functions from forecast package.
    #'
    #' @param data X field of Ohlcv object
    #'
    #' @return Calculate rolling features from forecasting package.
    calculate_features = function(data) {

      # Exuber
      for (s in setdiff(names(features_conf), "defaults")) {
        print(s)
        predictors_old = private$get_latest(s)

      }

      latest = private$get_latest("RollingExuber")
      if ("RollingExuber" %in% names(features_conf)) {
        RollingExuberInit = do.call(RollingExuber$new, features_conf$RollingExuber)
        RollingExuberFeatures <- RollingExuberInit$get_rolling_features(ohlcv)
      }

      # # autoarima
      # if ("RollingForecasts" %in% names(features_conf)) {
      #   RollingAutoarimaInit = do.call(RollingForecats$new, features_conf$RollingForecasts)
      #   RollingAutoarimaFeatures <- RollingAutoarimaInit$get_rolling_features(ohlcv)
      # }

      # test <- NULL

      # # merge all features
      # features_c <- c("RollingExuberFeatures", "RollingAutoarimaFeatures",
      #                 "RollingBidAskFeatues")
      # features_l <- features_c[vapply(features_c, exists, logical(1))]
      # features <- Reduce(function(x, y) merge(x, y, by = c("symbol", "date"), all.x = TRUE, all.y = FALSE),
      #                    sapply(features_l, get))

      # return(features)
    }
  ),
  private = list(

    #' @description
    #' Function get_latest get latest predictors from path.
    #'
    #' @param predictors_name Name of predictors
    #'
    #' @return Latest predictors
    get_latest = function(predictors_name) tryCatch({
      f = file.info(dir_ls(self.path, glob = predictors_name))
      if (length(f) == 0) {
        print(paste0("There is no file with ", predictors_name))
        return(NULL)
      }
      latest = tail(f[order(f$ctime), ], 1)
      latest = row.names(latest)
      fread(latest)
    }, error = function(e) NULL),

    #' @description
    #' Function calculates forecastas based on auto.arima and nnetar functions from forecast package.
    #' https://stackoverflow.com/questions/38950005/how-to-manipulate-null-elements-in-a-nested-list/
    #'
    #' @param x List
    #' @param FUN function to apply
    #'
    #' @return Calculate rolling features from forecasting package.
    replaceInList = function (x, FUN, ...)
    {
      if (is.list(x)) {
        for (i in seq_along(x)) {
          x[i] <- list(replaceInList(x[[i]], FUN, ...))
        }
        x
      }
      else FUN(x, ...)
    },

    # rolling_predicotrs_main = function(uri, log_ = FALSE) {
    #   # define symbols
    #   if (update_mode) {
    #     symbols_ = path_ext_remove(path_file(dir_ls(uri)))
    #   } else {
    #     symbols_ = sp500_symbols
    #   }
    #
    #   # main function to calculate predictors
    #   for (s in symbols_) {
    #     # debug
    #     # s = "emc"
    #     # uri = URIBINOMIALTREND
    #     print(s)
    #
    #     # dir name
    #     file_name = file.path(uri, paste0(s, ".parquet"))
    #
    #     # sample data for symbol
    #     dt_ <- dt[symbol == s]
    #     dt_ = dt_[open > 0.00001 & high > 0.00001 & low > 0.00001 & close > 0.00001]
    #     if (nrow(dt_) < max(windows_)) next()
    #     if (dt_[, length(unique(close)) < 100]) next()
    #
    #     # get already estimated predictors
    #     if (file.exists(file_name)) {
    #       predictors_symbol = read_parquet(file_name)
    #       predictors_symbol[, date := with_tz(date, "America/New_York")]
    #       new_dates =
    #         as.POSIXct(
    #           setdiff(dt_[, date], predictors_symbol$date),
    #           origin = as.POSIXct("1970-01-01 00:00:00", tz = "UTC"),
    #           tz = "America/New_York"
    #         )
    #       if (length(new_dates) == 0) next()
    #       at_ = which(dt_[, date] %in% new_dates)
    #     } else {
    #       at_ <- 1:nrow(dt_)
    #     }
    #
    #     # define Ohlcv Instance
    #     ohlcv = Ohlcv$new(dt_, date_col = "date", id_col = "symbol")
    #
    #     # calculate predictor
    #     RollPredictorInstance = predictors_instance_tmp(at_)
    #     predictors_new = RollPredictorInstance$get_rolling_features(ohlcv, log_)
    #
    #     # remove symbol. We know symbol from file name
    #     predictors_new[, symbol := NULL]
    #
    #     # change time zone
    #     predictors_new[, date := with_tz(date, "UTC")]
    #
    #     # rbind new and old
    #     if (file.exists(file_name)) {
    #       predictors_new = rbindlist(list(predictors_symbol, predictors_new), fill = TRUE)
    #       predictors_new = unique(predictors_new, by = "date")
    #     }
    #
    #     # save to file_name
    #     setorder(predictors_new, date)
    #     write_parquet(predictors_new, file_name)
    #   }
    # }

    # get_at_ = function(predictors) {
    #   # debug
    #   # predictors = copy(RollingBidAskFeatures)
    #   # predictors = NULL
    #
    #   # test if previous data exists
    #   if (is.null(predictors)) {
    #     print("No data. ")
    #     new_dataset = dataset[, .(symbol, date = as.IDate(date))]
    #   } else {
    #     # get only new data
    #     new_dataset = fsetdiff(dataset[, .(symbol, date = as.IDate(date))],
    #                            predictors[, .(symbol, date)])
    #   }
    #
    #   # new_dataset = new_dataset[date > as.Date("2021-01-01")]
    #   new_data <- merge(OhlcvInstance$X[, .(symbol, date)],
    #                     new_dataset[, .(symbol, date, index = TRUE)],
    #                     by = c("symbol", "date"), all.x = TRUE, all.y = FALSE)
    #   at_ = new_data[, which(index == TRUE)]
    #   at_
    # },

    get_at = function(ohlcv, predictors) {
      if (is.null(predictors)) {
        return(self$at)
      } else {
        ohlcv$X[, index := 1][predictors[self$at], on = c("symbol", "date")]

        new_at = setdiff(self$at, )
        if (length(new_at) == 0) {
          print("No new data.")
          return(NULL)
        }
        at_ = new_dataset[, index := 1][OhlcvInstance$X, on = c("symbol", "date")]
        at_ = at_[, which(last_week_day == TRUE & index == 1)]
      }
      at_
    },

    create_file_name = function(name, pre = FALSE) {
      time_ = format.POSIXct(Sys.time(), format = "%Y%m%d%H%M%S")
      file_ = paste0(name, "-", time_, ".csv")
      if (pre) {
        file_ = paste0("Pre", file_)
      }
      file_ = path(self$path, file_)
      return(file_)
    }
  )
)
