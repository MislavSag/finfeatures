#' @title OhlcvFeatures Class
#'
#' @description
#' Function calculates basic features from OHLCV financial data
#'
#' @exports
OhlcvFeaturesDaily = R6::R6Class(
  "OhlcvFeaturesDaily",

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
    },
    # self$frequnit = frequnit

    #' @description
    #' Function calculates basic features from OHLCV financial data
    #'
    #' @param data X field of Ohlcv object.
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
      # library(PerformanceAnalytics)
      # library(TTR)
      # library(RollingWindow)
      # # import daily data
      # col = c("date", "open", "high", "low", "close", "volume", "close_adj", "symbol")
      # ohlcv = fread("F:/lean_root/data/all_stocks_daily.csv", col.names = col)
      # ohlcv = unique(ohlcv, by = c("symbol", "date"))
      # unadjustd_cols = c("open", "high", "low")
      # ohlcv[, (unadjustd_cols) := lapply(.SD, function(x) (close_adj / close) * x), .SDcols = unadjustd_cols]
      # ohlcv = na.omit(ohlcv)
      # setorder(ohlcv, symbol, date)
      # setnames(ohlcv, c("close", "close_adj"), c("close_raw", "close"))
      # ohlcv = ohlcv[open > 0.00001 & high > 0.00001 & low > 0.00001 & close > 0.00001]
      # ohlcv[, returns := close / shift(close) - 1, by = "symbol"]
      # ohlcv_n = ohlcv[, .N, by = symbol]
      # symbols_keep = ohlcv_n[N > max(windows_), symbol]
      # ohlcv = ohlcv[symbol %in% symbols_keep]
      # dim(ohlcv)
      # # ohlcv = Ohlcv$new(ohlcv, id_col = "symbol", date_col = "date")
      #
      # ohlcv <- as.data.table(stocks)
      # windows_ = c(5, 10, 22, 22 * 3, 22 * 6, 22 * 12, 22 * 12 * 2)
      # self = list()
      # self$quantile_divergence_window =  c(50, 100)
      # at_ <- NULL
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


      # MY PREDICTORS -----------------------------------------------------------
      print("My predictors.")

      # close ATH
      print("Close ATH")
      ohlcv[, close_ath := close / cummax(fifelse(is.na(high), -Inf, high)), by = symbol]

      # minimum return
      print("Rolling min returns")
      ohlcv[, min_ret := frollapply(returns, 22, min, na.rm = TRUE), by = symbol]

      # returns
      print("Calculate returns")
      w_ = c(1:5, 5*2, 22*(1:12), 252 * 2, 252 * 4)
      new_cols <- paste0("returns_", w_)
      ohlcv[, (new_cols) := lapply(w_, function(w) close / shift(close, n = w) - 1), by = symbol]

      # volumes
      print("Volume percent changes")
      w_ = c(1:5, 5*2, 22*(1:12), 252 * 2, 252 * 4)
      new_cols <- paste0("volume_", w_)
      ohlcv[, (new_cols) := lapply(w_, function(w) volume / shift(volume, n = w) - 1), by = symbol]

      # whole number discrepancy
      print("Calculate whole number discrepancy.")
      ohlcv[, pretty_1 := fcase(
        close < 0.1, ceiling(close * 100) / 100,
        close < 1 & close > 0.1, ceiling(close * 10) / 10,
        close < 10 & close >= 1, ceiling(close),
        close < 100 & close >= 10, ceiling(close / 10) * 10,
        close < 1000 & close >= 100, ceiling(close / 100) * 100,
        close < 10000 & close >= 1000, ceiling(close / 1000) * 1000,
        close >= 10000 & close < 100000, ceiling(close / 10000) * 10000,
        close >= 100000, ceiling(close / 100000) * 100000
      )]
      ohlcv[, pretty_2 := fcase(
        close < 0.1, floor((close - 0.0001) * 100) / 100,
        close < 1 & close > 0.1, floor((close - 0.0001) * 10) / 10,
        close < 10 & close >= 1, floor((close - 0.0001)),
        close < 100 & close >= 10, floor((close - 0.0001) / 10) * 10,
        close < 1000 & close >= 100, floor((close - 0.0001) / 100) * 100,
        close < 10000 & close >= 1000, floor((close - 0.0001) / 1000) * 1000,
        close >= 10000 & close < 100000, floor((close - 0.0001) / 10000) * 10000,
        close >= 100000, floor((close - 0.0001) / 100000) * 100000
      )]
      # ohlcv[sample(1:nrow(ohlcv), 20), .(symbol, date, close, pretty_1, pretty_2)]
      # ohlcv[close > 1000000, .(symbol, date, close, pretty_1, pretty_2)]
      # ohlcv[close < 1, .(symbol, date, close, pretty_1, pretty_2)]
      # ohlcv[close < 0.1, .(symbol, date, close, pretty_1, pretty_2)]
      # ohlcv[close < 0.01, .(symbol, date, close, pretty_1, pretty_2)]
      # ohlcv[pretty_1  == pretty_2]
      ohlcv[, pretty_1 := (close - pretty_1) / pretty_1]
      ohlcv[, pretty_2 := (close - pretty_2) / pretty_2]

      # rolling volatility
      print("Rolling volatility.")
      new_cols <- paste0("sd_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) roll::roll_sd(returns, width = w)), by = symbol]

      # Close-to-Close Volatility
      print("OHLCV volatility.")
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
      print("Rolling sharpe")
      new_cols <- paste0("sharpe_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) as.vector(RollingSharpe(returns_1, rep(0, length(close)), window = w,
                                                                                 na_method = "ignore"))), by = symbol]

      # rolling quantile substraction
      print("Calculate rolling quantile div")
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

      # OPENSOURCE AP --------------------------------------------------------
      print("Open source asset pricing daily variation.")

      # 52 week
      print("52 week")
      w_ = c(22 * 1:12)
      new_cols <- paste0("ath_", w_)
      ohlcv[, (new_cols) := lapply(w_,
                                   function(w) close / frollapply(high, w, max, na.rm = TRUE)),
            by = symbol]
      tail(ohlcv[symbol == "aapl"], 50)

      # maximum return
      print("Rolling max returns")
      ohlcv[, max_ret := frollapply(returns, 22, max, na.rm = TRUE), by = symbol]

      # dolvol
      print("DolVol")
      ohlcv[, dolvolm := close * volume, by = symbol]
      ohlcv[, dolvolm := frollsum(dolvolm, 22, na.rm=TRUE), by = symbol]
      w_ = c(1,22 * 1:3)
      new_cols <- paste0("dolvol_", w_)
      ohlcv[, (new_cols) := lapply(w_, function(y) shift(x=dolvolm, n=y))]
      # mom12m already caluclated in my predictors

      # MERGE ALL ---------------------------------------------------------------
      # keep only relevant columns
      if (!is.null(at_)) {
        keep_dates[, index := TRUE]
        ohlcv_sample <- merge(ohlcv, keep_dates,
                              by = c("symbol", "date"), all.x = TRUE, all.y = FALSE)
        ohlcv_sample <- ohlcv_sample[index == TRUE]
        ohlcv_sample[, index := NULL]
        return(ohlcv_sample)
      } else {
        return(ohlcv)
      }
    }
  )
)