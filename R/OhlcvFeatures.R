#' @title OhlcvFeatures Class
#'
#' @description
#' Function calculates basic features from OHLCV financial data
#'
#' @export
#' @examples
#' data(spy_hour)
#' OhlcvInstance = Ohlcv$new(spy_hour, date_col = "datetime")
#' RollingGpdInit = OhlcvFeatures$new(windows = c(200, 300),
#'                                    quantile_divergence_window =  c(50, 100))
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

      # returns
      new_cols <- paste0("returns_", c(1, windows_))
      ohlcv[, (new_cols) := lapply(c(1, windows_), function(w) close / shift(close, n = w) - 1), by = symbol]

      # rolling volatility
      new_cols <- paste0("sd_", self$windows)
      ohlcv[, (new_cols) := lapply(self$windows, function(w) roll::roll_sd(returns_1, width = w)), by = symbol]

      # Close-to-Close Volatility
      new_cols <- paste0("sd_close_", self$windows)
      ohlcv[, (new_cols) := lapply(self$windows, function(w) volatility(close, n = w, calc = "close")), by = symbol]
      new_cols <- paste0("sd_parkinson_", self$windows)
      ohlcv[, (new_cols) := lapply(self$windows, function(w) volatility(cbind(open, high, low, close), n = w, calc = "parkinson")),
            by = symbol]
      new_cols <- paste0("sd_rogers.satchell_", self$windows)
      ohlcv[, (new_cols) := lapply(self$windows, function(w) volatility(cbind(open, high, low, close), n = w, calc = "rogers.satchell")),
            by = symbol]
      new_cols <- paste0("sd_gk.yz_", self$windows)
      ohlcv[, (new_cols) := lapply(self$windows, function(w) volatility(cbind(open, high, low, close), n = w, calc = "gk.yz")),
            by = symbol]
      new_cols <- paste0("sd_yang.zhang_", self$windows)
      ohlcv[, (new_cols) := lapply(self$windows, function(w) volatility(cbind(open, high, low, close), n = w, calc = "yang.zhang")),
            by = symbol]

      # rolling skewness
      new_cols <- paste0("skew_", self$windows)
      ohlcv[, (new_cols) := lapply(self$windows, function(w) as.vector(RollingSkew(returns_1, window = w, na_method = "ignore"))), by = symbol]

      # rolling kurtosis
      new_cols <- paste0("kurtosis_", self$windows)
      ohlcv[, (new_cols) := lapply(self$windows, function(w) as.vector(RollingKurt(returns_1, window = w, na_method = "ignore"))), by = symbol]

      # rolling TA indicators
      new_cols <- paste0("rsi_", self$windows)
      ohlcv[, (new_cols) := lapply(self$windows, function(w) rsi(close, n = w)), by = symbol]
      new_cols <- expand.grid("bbands", c("dn", "mavg", "up", "pctB"), self$windows)
      new_cols <- paste(new_cols$Var1, new_cols$Var2, new_cols$Var3, sep = "_")
      ohlcv <- cbind(ohlcv,
                     setNames(do.call(cbind.data.frame, lapply(self$windows, function(w) BBands(ohlcv$close, n = w))), new_cols))
      new_cols <- paste0("percent_rank_", self$windows)
      ohlcv[, (new_cols) := lapply(self$windows, function(w) roll_percent_rank(close, n = w)), by = symbol]

      # trading rules
      ohlcv[, close_above_sma200 := as.integer(close > sma(close, n = 200)), by = symbol]
      ohlcv[, ema_above_sma200 := as.integer(ema(close, n = 50) > sma(close, n = 200)), by = symbol]
      ohlcv[, close_above_vwap_20 := as.integer(close > TTR::VWAP(close, volume, n = 20)), by = symbol]

      # rolling volume
      new_cols <- paste0("volume_", self$windows)
      ohlcv[, (new_cols) := lapply(self$windows, function(w) frollmean(volume / 1000, n = w, na.rm = TRUE)), by = symbol]
      new_cols <- paste0("volume_rate_", self$windows)
      ohlcv[, (new_cols) := lapply(self$windows, function(w) volume / shift(volume, n = w) - 1), by = symbol]

      # rolling linear regression model: y = 1 + y_t-1 + e
      new_cols <- paste0("lm_", self$windows)
      ohlcv[, (new_cols) := lapply(self$windows, function(w) roll::roll_lm(log(close), date, width = w))$coefficients[2], by = symbol]
      new_cols <- paste0("r2_", self$windows)
      ohlcv[, (new_cols) := lapply(self$windows, function(w) roll::roll_lm(log(close), date, width = w))$r.squared, by = symbol]

      # rolling sharpe ratio
      new_cols <- paste0("sharpe_", self$windows)
      ohlcv[, (new_cols) := lapply(self$windows, function(w) as.vector(RollingSharpe(returns_1, rep(0, length(close)), window = w,
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
        ohlcv[, paste(c('breaks', 'changes'), i, sep = '_') := get_changepoints(returns, method = 'Mood', i), by = .(symbol)]
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
