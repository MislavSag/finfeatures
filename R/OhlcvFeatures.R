#' @title OhlcvFeatures Class
#'
#' @description
#' Function calculates basic features from OHLCV financial data
#'
#' @export
#' @examples
#' data(spy_hour)
#' OhlcvInstance = Ohlcv$new(spy_hour, date_col = "datetime")
#' RollingGpdInit = OhlcvFeatures$new(windows = c(200, 300), quantile_divergence_window =  c(50, 100))
#' x = RollingGpdInit$get_ohlcv_features(OhlcvInstance)
#' tail(x)
OhlcvFeatures = R6::R6Class(
  "OhlcvFeatures",

  public = list(

    #' @field windows Length of window for calculating rolling versions of the indicators.
    windows = NULL,

    #' @field quantile_divergence_window Window sizes from divergence from quantiles indicator.
    quantile_divergence_window = NULL,

    #' @description
    #' Create a new OhlcvFeatures object.
    #'
    #' @param windows Length of window for calculating rolling versions of the indicators.
    #' @param quantile_divergence_window Window sizes from divergence from quantiles indicator.
    #'
    #' @return A new `RollingGpd` object.
    initialize = function(windows = c(5, 22), quantile_divergence_window = c(50, 100)) {
      self$windows = windows
      self$quantile_divergence_window = quantile_divergence_window
    },

    #' @description
    #' Function calculates basic features from OHLCV financial data
    #'
    #' @param data X field of Ohlcv object
    #' @param window window length. This argument is given internaly
    #'
    #' @return Data.table with new features.
    get_ohlcv_features = function(data) {

      # prepare data
      # data <- OhlcvInstance
      ohlcv <- as.data.table(data$X)
      setkey(ohlcv, "symbol")
      windows_ = self$windows
      # windows_ = c(200, 300)

      # checks
      testSubset(c("symbol", "open", "high", "low", "close"), colnames(ohlcv))
      assert_double(ohlcv$open, lower = 1e-005)
      assert_double(ohlcv$high, lower = 1e-005)
      assert_double(ohlcv$low, lower = 1e-005)
      assert_double(ohlcv$close, lower = 1e-005)

      # close ATH
      ohlcv[, close_ath := (cummax(close) - close) / cummax(close), by = symbol]

      # whole number discrepancy
      ohlcv$pretty_1 <- sapply(ohlcv$close, function(x) pretty(x)[1])
      ohlcv$pretty_2 <- sapply(ohlcv$close, function(x) pretty(x)[2])
      ohlcv[, close_round_div_down := (close - pretty_1) / pretty_1]
      ohlcv[, close_round_div_up := (close - pretty_2) / pretty_2]
      ohlcv[, `:=`(pretty_1 = NULL, pretty_2 = NULL)]

      # returns
      new_cols <- paste0("returns_", c(1, windows_))
      ohlcv[, (new_cols) := lapply(c(1, windows_), function(w) close / shift(close, n = w) - 1), by = symbol]

      # rolling volatility
      new_cols <- paste0("sd_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) roll::roll_sd(returns_1, width = w)), by = symbol]

      # Close-to-Close Volatility
      new_cols <- paste0("sd_close_", windows_)
      new_cols <- paste0("sd_parkinson_", self$windows)
      ohlcv[, (new_cols) := lapply(self$windows, function(w) volatility(cbind(open, high, low, close), n = w, calc = "parkinson")),
            by = symbol]
      new_cols <- paste0("sd_rogers.satchell_", self$windows)
      ohlcv[, (new_cols) := lapply(self$windows, function(w) volatility(cbind(open, high, low, close), n = w, calc = "rogers.satchell")),
            by = symbol]
      new_cols <- paste0("sd_gk.yz_", self$windows)
      ohlcv[, (new_cols) := lapply(self$windows, function(w) volatility(cbind(open, high, low, close), n = w, calc = "gk.yz")),
            by = symbol]
      new_cols <- paste0("sd_yang.zhang_", windows_)
      ohlcv[, (new_cols) := lapply(self$windows, function(w) volatility(cbind(open, high, low, close), n = w, calc = "yang.zhang")),
            by = symbol]

      # rolling skewness
      new_cols <- paste0("skew_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) as.vector(RollingSkew(returns_1, window = w, na_method = "ignore"))), by = symbol]

      # rolling kurtosis
      new_cols <- paste0("kurtosis_", self$windows)
      ohlcv[, (new_cols) := lapply(self$windows, function(w) as.vector(RollingKurt(returns_1, window = w, na_method = "ignore"))), by = symbol]

      # rolling TA indicators
      # RSI
      new_cols <- paste0("rsi_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) rsi(close, n = w)), by = symbol]
      # BBANDS
      new_cols <- expand.grid("bbands", c("dn", "mavg", "up", "pctB"), windows_)
      new_cols <- paste(new_cols$Var1, new_cols$Var2, new_cols$Var3, sep = "_")
      ohlcv[, (new_cols) := do.call(cbind, lapply(windows_, function(w) as.data.frame(BBands(close, n = w)))), by = symbol]
      new_cols_change <- new_cols[grep("up|mavg|dn", new_cols)]
      ohlcv[, (new_cols_change) := lapply(.SD, function(x) (close - x) / x), .SDcols = new_cols_change]
      # percent rand
      new_cols <- paste0("percent_rank_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) QuantTools::roll_percent_rank(close, n = w)), by = symbol]

      # trading rules
      ohlcv[, close_above_sma200 := (close - sma(close, n = 200)) / sma(close, n = 200), by = symbol]
      ohlcv[, close_above_sma100 := (close - sma(close, n = 100)) / sma(close, n = 100), by = symbol]
      ohlcv[, close_above_sma50 := (close - sma(close, n = 50)) / sma(close, n = 50), by = symbol]
      ohlcv[, close_above_sma22 := (close - sma(close, n = 22)) / sma(close, n = 22), by = symbol]
      ohlcv[, ema_above_sma200 := (ema(close, n = 50) - sma(close, n = 200)) / sma(close, n = 200), by = symbol]
      ohlcv[, ema_above_sma100 := (ema(close, n = 50) - sma(close, n = 100)) / sma(close, n = 100), by = symbol]
      ohlcv[, close_above_vwap_20 := (close - TTR::VWAP(close, volume, n = 20)) / TTR::VWAP(close, volume, n = 20), by = symbol]
      ohlcv[, close_above_vwap_50 := (close - TTR::VWAP(close, volume, n = 50)) / TTR::VWAP(close, volume, n = 50), by = symbol]

      # rolling volume
      # new_cols <- paste0("volume_", windows_)
      # ohlcv[, (new_cols) := lapply(windows_, function(w) frollmean(volume / 1000, n = w, na.rm = TRUE)), by = symbol]
      new_cols <- paste0("volume_rate_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) volume / shift(volume, n = w) - 1), by = symbol]

      # rolling linear regression model: y = 1 + date + e NOT THAT GOOD AFTER ALL ?
      # new_cols <- expand.grid("lm", c("cf1", "cf2", "r2", "std1", "std2"), windows_)
      # new_cols <- paste(new_cols$Var1, new_cols$Var2, new_cols$Var3, sep = "_")
      # ohlcv[, (new_cols) := do.call(cbind,
      #                               lapply(windows_,
      #                                      function(w) as.data.frame(as.data.table(roll::roll_lm(log(close), date, width = w))))),
      #       by = symbol]
      # new_cols_change <- new_cols[grep("cf|std", new_cols)]
      # ohlcv[, (new_cols_change) := lapply(.SD, function(x) x / 100000), .SDcols = new_cols_change]

      # rolling sharpe ratio
      new_cols <- paste0("sharpe_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) as.vector(RollingSharpe(returns_1, rep(0, length(close)), window = w,
                                                                                 na_method = "ignore"))), by = symbol]

      # rolling quantile substraction
      generate_quantile_divergence <- function(ohlcv, p = 0.99, window_sizes = self$quantile_divergence_window) {
        q_cols <- paste0("q", p * 100, "_close_", window_sizes)
        ohlcv[, (q_cols) := lapply(window_sizes, function(w) roll::roll_quantile(close, width = w, p = p)), by = symbol]
        new_cols <- paste0("q", p * 100, "_close_divergence_", window_sizes)
        ohlcv[, (new_cols) := lapply(q_cols, function(x) (close - get(x)) / close), by = symbol]
        ohlcv[, (q_cols):=NULL]
        return(ohlcv)
      }
      ohlcv <- generate_quantile_divergence(ohlcv, p = 0.01)
      ohlcv <- generate_quantile_divergence(ohlcv, p = 0.25)
      ohlcv <- generate_quantile_divergence(ohlcv, p = 0.5)
      ohlcv <- generate_quantile_divergence(ohlcv, p = 0.75)
      ohlcv <- generate_quantile_divergence(ohlcv, p = 0.25)
      ohlcv <- generate_quantile_divergence(ohlcv, p = 0.99)

      # estimate changepoints breaks
      for (i in c(370, 500, 1000, 5000)) {
        # ohlcv[, paste(c('breaks', 'changes'), i, sep = '_') := self$get_changepoints(returns, method = 'Mood', i), by = .(symbol)]
        ohlcv[, paste(c('breaks', 'changes'), i, sep = '_') := self$get_changepoints(returns, method = 'Mood', i), by = .(symbol)]
      }

      return(ohlcv)
    },

    #' @description
    #' Helper function for calculating changepoints features
    #'
    #' @param returns Returns column.
    #' @param method Argument method from cpm package
    #' @param ARL0 Argument ARL0 from cpm package
    #' @param startup Argument sartup from cpm package
    #'
    #' @return Data.table with new features.
    get_changepoints = function(returns, method, arl0) {
      # change points roll
      detectiontimes <- numeric()
      changepoints <- numeric()
      cpm <- makeChangePointModel(cpmType=method, ARL0=arl0, startup=200)
      i <- 0
      while (i < length(returns)) {

        i <- i + 1

        # if returns is na returns FALSE
        if (is.na(returns[i])) {
          next()
        }

        # process each observation in turn
        cpm <- processObservation(cpm, returns[i])

        # if a change has been found, log it, and reset the CPM
        if (changeDetected(cpm) == TRUE) {
          detectiontimes <- c(detectiontimes,i)

          # the change point estimate is the maximum D_kt statistic
          Ds <- getStatistics(cpm)
          tau <- which.max(Ds)
          if (length(changepoints) > 0) {
            tau <- tau + changepoints[length(changepoints)]
          }
          changepoints <- c(changepoints,tau)

          # reset the CPM
          cpm <- cpmReset(cpm)

          #resume monitoring from the observation following the change point
          i <- tau
        }
      }
      points <- cbind.data.frame(detectiontimes, changepoints)
      breaks <- rep(FALSE, length(returns))
      breaks[detectiontimes] <- TRUE
      change <- rep(FALSE, length(returns))
      change[changepoints] <- TRUE
      return(cbind.data.frame(breaks, change))
    }
  )
)
