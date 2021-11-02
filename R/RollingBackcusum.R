#' @title RollingBackcusum Class
#'
#' @description
#' Function calculates backCUSUM tests on rolling window from backCUSUM pakcage.
#'
#' @export
#' @examples
#' data(spy_hour)
#' OhlcvInstance = Ohlcv$new(spy_hour, date_col = "datetime")
#' RollingBackcusumInit = RollingBackcusum$new(windows = 200,
#'                                             workers = 1L,
#'                                             at = c(300, 500),
#'                                             lag = 0L,
#'                                             na_pad = TRUE,
#'                                             simplify = FALSE)
#' x = RollingBackcusumInit$get_rolling_features(OhlcvInstance)
#' head(x)
#' # multiple windows and parallel
# RollingBackcusumInit = RollingBackcusum$new(windows = c(22, 66),
#                                             workers = 8L,
#                                             at = c(300:315),
#                                             lag = 1L,
#                                             na_pad = TRUE,
#                                             simplify = FALSE)
# x = RollingBackcusumInit$get_rolling_features(OhlcvInstance)
# head(x)
RollingBackcusum = R6::R6Class(
  "RollingBackcusum",
  inherit = RollingGeneric,

  public = list(

    #' @description
    #' Function calculates radf values from exuber package on rolling window.
    #'
    #' @param data X field of Ohlcv object
    #' @param window window length. This argument is given internaly
    #'
    #' @return Calculate rolling radf features from backCUSUM package.
    rolling_function = function(data, window) {

      # check if there is enough data
      if (length(unique(data$symbol)) > 1) {
        print(paste0("not enough data for symbol ", data$symbol[1]))
        return(NA)
      }

      # calculate arima forecasts
      y <- na.omit(data$returns)
      y <- SBQ.test(as.formula('y ~ 1'), alternative = 'greater')# [['statistic']]
      results <- c(y[['statistic']], as.integer(y[['rejection']]))
      names(results) <- c("statistics", paste0("backcusum_rejections_", as.numeric(names(y[['rejection']])) * 1000))
      results <- as.data.table(as.list(results))
      results <- data.table(symbol = data$symbol[1], date = data$date[length(data$date)], results)
      colnames(results)[3:ncol(results)] <- paste(colnames(results)[3:ncol(results)], window, sep = "_")
      return(results)
    }
  )
)
