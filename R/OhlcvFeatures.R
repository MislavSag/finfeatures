#' @title OhlcvFeatures Class
#'
#' @description
#' Function calculates basic features from OHLCV financial data
#'
#' @export
#' @examples
#' data(spy_hour)
#' OhlcvInstance = Ohlcv$new(spy_hour, date_col = "datetime")
#' RollingOhlcvFeatures = OhlcvFeatures$new(at = NULL,
#'                                          windows = c(200, 300),
#'                                          quantile_divergence_window =  c(50, 100))
#' x = RollingOhlcvFeatures$get_ohlcv_features(OhlcvInstance)
#' tail(x)
OhlcvFeatures = R6::R6Class(
  "OhlcvFeatures",

  public = list(

    #' @field Row index for which we calcualte indicators.
    at = NULL,

    # @field Frequency unit, for example 8 for hour and 60 fo minute, 1 for day
    # frequnit = NULL,

    #' @field windows Length of window for calculating rolling versions of the indicators.
    windows = NULL,

    #' @field quantile_divergence_window Window sizes from divergence from quantiles indicator.
    quantile_divergence_window = NULL,

    #' @description
    #' Create a new OhlcvFeatures object.
    #'
    #' @param at Row index for which we calcualte indicators.
    # @param frequnit Frequency unit, for example 8 for hour and 60 fo minute, 1 for day
    #' @param windows Length of window for calculating rolling versions of the indicators.
    #' @param quantile_divergence_window Window sizes from divergence from quantiles indicator.
    #'
    #' @return A new `RollingGpd` object.
    initialize = function(at = NULL, windows = c(5, 22), quantile_divergence_window = c(50, 100)) {
      self$at = at
      self$windows = windows
      self$quantile_divergence_window = quantile_divergence_window
      # self$frequnit = frequnit
    },

    #' @description
    #' Function calculates basic features from OHLCV financial data
    #'
    #' @param data X field of Ohlcv object
    #' @param window window length. This argument is given internaly.
    #'
    #' @return Data.table with new features.
    get_ohlcv_features = function(data) {

      ###### DEBUG ######
      # data(spy_hour)
      # data(stocks)
      # library(data.table)
      # library(finfeatures)
      # library(checkmate)
      # library(TTR)
      # library(RollingWindow)
      # library(QuantTools)
      # ohlcv <- as.data.table(stocks)
      # windows_ = c(5, 10, 22, 22 * 3, 22 * 6, 22 * 12, 22 * 12 * 2)
      # quantile_divergence_window =  c(22, 22*3, 22*6, 22*12, 22*12*2)
      # at_ <- c(500, 1000)
      # ohlcv <- fread("D:/temp/prices_dt.csv")
      # at_ <- readRDS("D:/temp/at.rds")
      ###### DEBUG ######

      # prepare data
      ohlcv <- as.data.table(data$X)
      # setkey(ohlcv, "symbol") # change sort !
      windows_ = self$windows
      at_ = self$at
      # frequnit_ = self$frequnit

      # checks
      testSubset(c("symbol", "open", "high", "low", "close"), colnames(ohlcv))
      assert_double(ohlcv$open, lower = 1e-005)
      assert_double(ohlcv$high, lower = 1e-005)
      assert_double(ohlcv$low, lower = 1e-005)
      assert_double(ohlcv$close, lower = 1e-005)

      # keep only rows we nedd for calculation, to make calculation faster
      if (!is.null(at_)) {
        keep_dates <- ohlcv[at_, .(symbol, date)]
        # keep_indecies <- lapply(at_, function(x) (max(x - max(windows_), 1)):(min(x + max(windows_), nrow(ohlcv))))
        keep_indecies <- lapply(at_, function(x) (max(x - max(c(windows_, quantile_divergence_window)), 1)):x)
        keep_indecies <- unique(unlist(keep_indecies))
        ohlcv <- ohlcv[keep_indecies]
      }

      # close ATH
      print("Calculate close ATH div.")
      ohlcv[, close_ath := (cummax(high) - close) / cummax(high), by = symbol]
      new_cols <- paste0("ath_", windows_)
      ohlcv[, (new_cols) := lapply(c(1, windows_), function(w) (shift(frollapply(high, w, max), 1L) / close) - 1), by = symbol]

      # whole number discrepancy
      print("Calculate whole number discrepancy.")
      ohlcv$pretty_1 <- vapply(ohlcv$close, function(x) pretty(x)[1], numeric(1L))
      ohlcv$pretty_2 <- sapply(ohlcv$close, function(x) pretty(x)[2])
      ohlcv[, pretty_1 := ifelse(pretty_1 <= 0, 0.01, pretty_1)]
      ohlcv[, close_round_div_down := (close - pretty_1) / pretty_1]
      ohlcv[, close_round_div_up := (close - pretty_2) / pretty_2]
      ohlcv[, `:=`(pretty_1 = NULL, pretty_2 = NULL)]

      # returns
      print("Calculate returns.")
      new_cols <- paste0("returns_", unique(c(1:12, windows_)))
      ohlcv[, (new_cols) := lapply(c(1:12, windows_), function(w) close / shift(close, n = w) - 1), by = symbol]

      # rolling volatility
      print("Calculate rolling volatility.")
      new_cols <- paste0("sd_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) roll::roll_sd(returns_1, width = w)), by = symbol]

      # Close-to-Close Volatility
      print("Calculate OHLCV volatility.")
      new_cols <- paste0("sd_parkinson_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) volatility(cbind(open, high, low, close), n = w, calc = "parkinson")),
            by = symbol]
      new_cols <- paste0("sd_rogers.satchell_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) volatility(cbind(open, high, low, close), n = w, calc = "rogers.satchell")),
            by = symbol]
      new_cols <- paste0("sd_gk.yz_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) volatility(cbind(open, high, low, close), n = w, calc = "gk.yz")),
            by = symbol]
      new_cols <- paste0("sd_yang.zhang_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) volatility(cbind(open, high, low, close), n = w, calc = "yang.zhang")),
            by = symbol]

      # rolling skewness
      print("Calculate moments.")
      new_cols <- paste0("skew_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) as.vector(RollingSkew(returns_1, window = w, na_method = "ignore"))), by = symbol]

      # rolling kurtosis
      new_cols <- paste0("kurtosis_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) as.vector(RollingKurt(returns_1, window = w, na_method = "ignore"))), by = symbol]

      # rolling TA indicators
      print("Calculate technival indicators.")
      # RSI
      new_cols <- paste0("rsi_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) rsi(close, n = w)), by = symbol]
      # BBANDS
      new_cols <- expand.grid("bbands", c("dn", "mavg", "up", "pctB"), windows_)
      new_cols <- paste(new_cols$Var1, new_cols$Var2, new_cols$Var3, sep = "_")
      ohlcv[, (new_cols) := do.call(cbind, lapply(windows_, function(w) as.data.frame(BBands(close, n = w)))), by = symbol]
      new_cols_change <- new_cols[grep("up|mavg|dn", new_cols)]
      ohlcv[, (new_cols_change) := lapply(.SD, function(x) (close - x) / x), .SDcols = new_cols_change]
      # ADX
      # set window to 14 because I get error otherwise
      new_cols <- expand.grid("adx", c("dip", "din", "dx", "adx"), 14)
      new_cols <- paste(new_cols$Var1, new_cols$Var2, new_cols$Var3, sep = "_")
      ohlcv[, (new_cols) := do.call(cbind, lapply(14, function(w) as.data.frame(ADX(cbind(high, low, close), n = w)))), by = symbol]
      # CCI
      new_cols <- paste0("cci_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) CCI(cbind(high, low, close), n = w)), by = symbol]
      # OBV
      new_cols <- paste0("obv_", windows_[1])
      ohlcv[, (new_cols) := lapply(windows_[1], function(w) OBV(close, volume)), by = symbol]
      # SAR
      new_cols <- paste0("sar_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) as.vector(SAR(cbind(high, close)))), by = symbol]
      # WPR
      new_cols <- paste0("wpr_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) WPR(cbind(high, low, close))), by = symbol]
      # AROON
      new_cols <- expand.grid("aroon", c("aroonUp", "aroonDn", "oscillator"), windows_)
      new_cols <- paste(new_cols$Var1, new_cols$Var2, new_cols$Var3, sep = "_")
      ohlcv[, (new_cols) := do.call(cbind, lapply(windows_, function(w) as.data.frame(aroon(cbind(high, low), n = w)))), by = symbol]
      # chaikinAD
      new_cols <- paste0("chaikinad_", windows_[1])
      ohlcv[, (new_cols) := lapply(windows_[1], function(w) chaikinAD(cbind(high, low, close), volume)), by = symbol]
      # percent rank
      new_cols <- paste0("percent_rank_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) QuantTools::roll_percent_rank(close, n = w)), by = symbol]
      # tail(TTR::(ohlcv[, .(high, low, close)], ohlcv$volume))

      # trading rules
      print("Calculate tradin rules.")
      ohlcv[, close_above_sma200 := (close - sma(close, n = 200)) / sma(close, n = 200), by = symbol]
      ohlcv[, close_above_sma100 := (close - sma(close, n = 100)) / sma(close, n = 100), by = symbol]
      ohlcv[, close_above_sma50 := (close - sma(close, n = 50)) / sma(close, n = 50), by = symbol]
      ohlcv[, close_above_sma22 := (close - sma(close, n = 22)) / sma(close, n = 22), by = symbol]
      ohlcv[, ema_above_sma200 := (ema(close, n = 50) - sma(close, n = 200)) / sma(close, n = 200),
            by = symbol]
      ohlcv[, ema_above_sma100 := (ema(close, n = 50) - sma(close, n = 100)) / sma(close, n = 100),
            by = symbol]
      ohlcv[, close_above_vwap_20 := (close - TTR::VWAP(close, volume, n = 20)) / TTR::VWAP(close, volume, n = 20),
            by = symbol]
      ohlcv[, close_above_vwap_50 := (close - TTR::VWAP(close, volume, n = 50)) / TTR::VWAP(close, volume, n = 50),
            by = symbol]
      ohlcv[, close_above_vwap_100 := (close - TTR::VWAP(close, volume, n = 100)) / TTR::VWAP(close, volume, n = 100),
            by = symbol]
      ohlcv[, close_above_vwap_200 := (close - TTR::VWAP(close, volume, n = 200)) / TTR::VWAP(close, volume, n = 200),
            by = symbol]

      # rolling volume
      # new_cols <- paste0("volume_", windows_)
      # ohlcv[, (new_cols) := lapply(windows_, function(w) frollmean(volume / 1000, n = w, na.rm = TRUE)), by = symbol]
      new_cols <- paste0("volume_rate_", c(1:3, windows_))
      ohlcv[, (new_cols) := lapply(c(1:3, windows_), function(w) (volume + 1) / (shift(volume, n = w) + 1) - 1),
            by = symbol]

      # rolling linear regression model: y = 1 + date + e NOT THAT GOOD AFTER ALL ?
      print("Calculate rolling lin regression.")
      new_cols <- expand.grid("lm", c("cf1", "cf2", "r2", "std1", "std2"), windows_)
      new_cols <- paste(new_cols$Var1, new_cols$Var2, new_cols$Var3, sep = "_")
      ohlcv[, (new_cols) := do.call(cbind,
                                    lapply(windows_,
                                           function(w) as.data.frame(as.data.table(roll::roll_lm(1:length(close), log(close), width = w))))),
            by = symbol]
      cols_ <- new_cols[grep("lm_cf", new_cols)]
      ohlcv[, (cols_) := lapply(.SD, function(x) (exp(x)^252-1) * 100), .SDcols = cols_]

      # rolling sharpe ratio
      new_cols <- paste0("sharpe_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) as.vector(RollingSharpe(returns_1, rep(0, length(close)), window = w,
                                                                                 na_method = "ignore"))), by = symbol]
      # rolling quantile substraction
      print("Calculate rolling quantile div.")
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
      ohlcv <- generate_quantile_divergence(ohlcv, p = 0.99)

      # support / resistance (TODO too slow)
      # print("Calculate rolling support / resistance.")
      # ohlcv[, close_round := round(close, 1)]
      # windows__ <- unique(c(windows_, 200, 500))
      # new_cols <- paste0("rolling_mode_", windows__)
      # baseMode <- function(x, narm = FALSE) {
      #   if (narm) x <- x[!is.na(x)]
      #   ux <- unique(x)
      #   ux[which.max(table(match(x, ux)))]
      # }
      # ohlcv[, (new_cols) := lapply(windows__, function(w) frollapply(close_round, w, function(x) {
      #   baseMode(x)
      # })), by = symbol]
      # new_cols_close <- paste0(new_cols, "_ratio")
      # ohlcv[, (new_cols_close) := lapply(.SD, function(x) (close / x) - 1), .SDcols = new_cols]
      # ohlcv[, close_round := NULL]
      # ohlcv[, (new_cols) := NULL]

      # estimate changepoints breaks (fast = FALSE)
      for (i in c(370, 500, 1000, 5000)) {
        ohlcv[, paste(c('breaks'), i, sep = '_') := self$get_changepoints(returns_1, method = 'Mood', i), by = .(symbol)]
      }

      # keep only relevant columns
      if (!is.null(at_)) {
        keep_dates[, index := TRUE]
        ohlcv_sample <- merge(ohlcv, keep_dates,
                              by = c("symbol", "date"), all.x = TRUE, all.y = FALSE)
        ohlcv_sample <- ohlcv_sample[index == TRUE]
        ohlcv_sample[, index := NULL]
      } else {
        ohlcv_sample <- copy(ohlcv)
      }

      return(ohlcv_sample)
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
      cpm <- cpm::makeChangePointModel(cpmType=method, ARL0=arl0, startup=200)
      i <- 0
      while (i < length(returns)) {

        i <- i + 1

        # if returns is na returns FALSE
        if (is.na(returns[i])) {
          next()
        }

        # process each observation in turn
        cpm <- cpm::processObservation(cpm, returns[i])

        # if a change has been found, log it, and reset the CPM
        if (changeDetected(cpm) == TRUE) {
          detectiontimes <- c(detectiontimes,i)

          # the change point estimate is the maximum D_kt statistic
          Ds <- cpm::getStatistics(cpm)
          tau <- which.max(Ds)
          if (length(changepoints) > 0) {
            tau <- tau + changepoints[length(changepoints)]
          }
          changepoints <- c(changepoints,tau)

          # reset the CPM
          cpm <- cpm::cpmReset(cpm)

          #resume monitoring from the observation following the change point
          i <- tau
        }
      }
      points <- cbind.data.frame(detectiontimes, changepoints)
      breaks <- rep(FALSE, length(returns))
      breaks[detectiontimes] <- TRUE
      # change <- rep(FALSE, length(returns))
      # change[changepoints] <- TRUE
      # return(cbind.data.frame(breaks, change))
      return(breaks)
    }
  )
)

# is.infinite.data.frame <- function(x) do.call(cbind, lapply(x, is.infinite))
# keep_cols <- names(which(colMeans(!is.infinite(as.data.frame(clf_data))) > 0.999))
# print(paste0("Removing columns with Inf values: ", setdiff(colnames(clf_data), keep_cols)))
# clf_data <- clf_data[, .SD, .SDcols = keep_cols]
