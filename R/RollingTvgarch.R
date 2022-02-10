#' @title RollingTvgarch Class
#'
#' @description
#' Function calculates tvgarch predictions and coefficients from tvgarch package on rolling window.
#'
#' @export
#' @examples
#' data(spy_hour)
#' OhlcvInstance = Ohlcv$new(spy_hour, date_col = "datetime")
#' RollingTvgarchInit = RollingTvgarch$new(windows = 200,
#'                                         workers = 1L,
#'                                         at = c(300, 500),
#'                                         lag = 0L,
#'                                         na_pad = TRUE,
#'                                         simplify = FALSE)
#' x = RollingTvgarchInit$get_rolling_features(OhlcvInstance)
#' head(x)
RollingTvgarch = R6::R6Class(
  "RollingTvgarch",
  inherit = RollingGeneric,

  public = list(

    #' @description
    #' Function calculates tvgarch predictions and coefficients from tvgarch package on rolling window.
    #'
    #' @param data X field of Ohlcv object
    #' @param window window length. This argument is given internaly
    #' @param price Prcie column in Ohlcv
    #'
    #' @return Calculate rolling tvgarch features from tvgarch package.
    rolling_function = function(data, window, price) {

      # check if there is enough data
      if (length(unique(data$symbol)) > 1) {
        print(paste0("not enough data for symbol ", data$symbol[1]))
        return(NA)
      }

      # yEst <- tvgarch(y = ySim, turbo = TRUE)
      # predict(yEst)
      # coef(yEst)

      # calculate arima forecasts
      y <- na.omit(data$returns) * 100
      y <- tvgarch(y, turbo = TRUE)
      pred <- predict(y)
      coefs <- coef(y)
      results <- data.table(pred_1 = pred[1], pred_n = tail(pred, 1), pred_mean = mean(pred, na.rm = TRUE), t(coefs))
      colnames(results) <- paste0("tvgarch_", colnames(results))
      results <- data.table(symbol = data$symbol[1], date = data$date[length(data$date)], results)
      colnames(results)[3:ncol(results)] <- paste(colnames(results)[3:ncol(results)], window, sep = "_")
      return(results)
    }
  )
)
