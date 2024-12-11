#' @title OhlcvFeatures Class
#'
#' @description
#' Function calculates basic features from OHLCV financial data
#'
#' @export
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
    #' @param ohlcv X field of Ohlcv object.
    #'
    #' @return Data.table with new features.
    get_ohlcv_features = function(ohlcv) {

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
      # ohlcv = fread("F:/lean/data/stocks_daily.csv", col.names = col)
      # ohlcv = unique(ohlcv, by = c("symbol", "date"))
      # unadjustd_cols = c("open", "high", "low")
      # ohlcv[, (unadjustd_cols) := lapply(.SD, function(x) (close_adj / close) * x), .SDcols = unadjustd_cols]
      # ohlcv = na.omit(ohlcv)
      # setorder(ohlcv, symbol, date)
      # setnames(ohlcv, c("close", "close_adj"), c("close_raw", "close"))
      # ohlcv = ohlcv[open > 0.00001 & high > 0.00001 & low > 0.00001 & close > 0.00001]
      # ohlcv[, returns := close / shift(close) - 1, by = "symbol"]
      # ohlcv_n = ohlcv[, .N, by = symbol]
      # windows_ = c(5, 10, 22, 22 * 3, 22 * 6, 22 * 12, 22 * 12 * 2)
      # symbols_keep = ohlcv_n[N > max(windows_), symbol]
      # ohlcv = ohlcv[symbol %in% symbols_keep]
      # dim(ohlcv)
      # # ohlcv = Ohlcv$new(ohlcv, id_col = "symbol", date_col = "date")
      # at = sample(1:nrow(ohlcv), 100000)
      # self = list()
      # self$at = at
      # self$windows = c(5, 10, 22, 22 * 3, 22 * 6, 22 * 12, 22 * 12 * 2)
      # self$quantile_divergence_window =  c(50, 100)

      # 3) intraday
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
      # ohlcv <- as.data.table(stocks)
      # windows_ = c(5, 10, 22, 22 * 3, 22 * 6, 22 * 12, 22 * 12 * 2)
      # self = list()
      # self$quantile_divergence_window =  c(50, 100)
      # at_ <- NULL
      ###### DEBUG ######

      # checks
      assert_class(ohlcv, "data.table")

      # prepare data
      setkey(ohlcv, "symbol")
      windows_ = self$windows
      if (is.null(self$at)) {
        at_ = 1:nrow(ohlcv)
      } else {
        at_ = self$at
      }
      setorder(ohlcv, symbol, date) # https://github.com/Rdatatable/data.table/issues/3456
      ids = c("symbol", "date")

      # additional checks
      testSubset(c("symbol", "open", "high", "low", "close"), colnames(ohlcv))
      assert_double(ohlcv$open, lower = 1e-008)
      assert_double(ohlcv$high, lower = 1e-008)
      assert_double(ohlcv$low, lower = 1e-008)
      assert_double(ohlcv$close, lower = 1e-008)

      # MY PREDICTORS -----------------------------------------------------------
      print("My predictors.")

      # returns
      print("Calculate returns")
      w_ = c(1:5, 5*2, 22*(1:12), 252 * 2, 252 * 4)
      w_ = sort(unique(c(windows_, w_)))
      new_cols = paste0("returns_", w_)
      ohlcv[, (new_cols) := lapply(w_, function(w) close / shift(close, n = w) - 1), by = symbol]
      if (!is.null(at_)) {
        dt_returns = ohlcv[at_, .SD, .SDcols = c(ids, new_cols)]
      }

      # close ATH
      print("Close ATH")
      ohlcv[, close_ath := close / cummax(fifelse(is.na(high), -Inf, high)), by = symbol]
      if (!is.null(at_)) {
        dt_ath = ohlcv[at_, .SD, .SDcols = c(ids, "close_ath")]
        ohlcv[, close_ath := NULL]
      }

      # minimum return
      print("Rolling min returns")
      new_cols = paste0("min_ret_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(n) {
        frollapply(returns_1, n, min, na.rm = TRUE)
      }), by = symbol]
      if (!is.null(at_)) {
        dt_minret = ohlcv[at_, .SD, .SDcols = c(ids, new_cols)]
        ohlcv[, (new_cols) := NULL]
      }

      # volumes
      print("Volume percent changes")
      w_ = c(1:5, 5*2, 22*(1:12), 252 * 2, 252 * 4)
      w_ = sort(unique(c(windows_, w_)))
      new_cols = paste0("volume_", w_)
      ohlcv[, (new_cols) := lapply(w_, function(w) volume / shift(volume, n = w) - 1), by = symbol]
      if (!is.null(at_)) {
        dt_volumes = ohlcv[at_, .SD, .SDcols = c(ids, new_cols)]
        ohlcv[, (new_cols) := NULL]
      }

      # bid ask spread
      methods_ = c("EDGE", "OHL", "OHLC", "AR", "AR2", "CS", "CS2", "ROLL")
      ohlcv = ohlcv[, as.data.table(spread(
        as.xts.data.table(.SD),
        width = 150,
        method = methods_
      )),
      by = symbol,
      .SDcols = c("date", "Open" = "open", "High" = "high", "Low" = "low", "Close" = "close")][
        ohlcv, on = c("symbol" = "symbol", "index" = "date")]
      setnames(ohlcv, "index", "date")
      setnames(ohlcv, methods_, paste0("bidask_", tolower(methods_)))
      if (!is.null(at_)) {
        dt_bidask = ohlcv[at_, .SD, .SDcols = c(ids, paste0("bidask_", tolower(methods_)))]
        ohlcv[, paste0("bidask_", tolower(methods_)) := NULL]
      }

      # Whole number discrepancy
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
      if (!is.null(at_)) {
        cols = colnames(ohlcv)[data.table::patterns("symbol|date|^pretty", cols = names(ohlcv))]
        dt_pretty = ohlcv[at_, ..cols]
        cols = colnames(ohlcv)[data.table::patterns("^pretty", cols = names(ohlcv))]
        ohlcv[, (cols) := NULL]
      }

      # rolling volatility
      print("Rolling volatility.")
      new_cols = paste0("sd_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) roll::roll_sd(returns_1, width = w)), by = symbol]
      if (!is.null(at_)) {
        dt_volatility = ohlcv[at_, .SD, .SDcols = c(ids, new_cols)]
        ohlcv[, (new_cols) := NULL]
      }

      # Close-to-Close Volatility
      print("OHLCV volatility.")
      new_cols = paste0("sd_parkinson_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) volatility(cbind(open, high, low, close), n = w, calc = "parkinson")),
            by = symbol]
      if (!is.null(at_)) {
        dt_parkinson = ohlcv[at_, .SD, .SDcols = c(ids, new_cols)]
        ohlcv[, (new_cols) := NULL]
      }
      new_cols = paste0("sd_rogers.satchell_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) {
        tryCatch({volatility(cbind(open, high, low, close), n = w, calc = "rogers.satchell")},
                 error = function(e) NA)
      }), by = symbol]
      if (!is.null(at_)) {
        dt_rogers = ohlcv[at_, .SD, .SDcols = c(ids, new_cols)]
        ohlcv[, (new_cols) := NULL]
      }
      new_cols = paste0("sd_gk.yz_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) {
        tryCatch({volatility(cbind(open, high, low, close), n = w, calc = "gk.yz")},
                 error = function(e) NA)
      }), by = symbol]
      if (!is.null(at_)) {
        dt_gkyz = ohlcv[at_, .SD, .SDcols = c(ids, new_cols)]
        ohlcv[, (new_cols) := NULL]
      }

      new_cols = paste0("sd_yang.zhang_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) {
        tryCatch({volatility(cbind(open, high, low, close), n = w, calc = "yang.zhang")},
                 error = function(e) NA)
      }), by = symbol]
      if (!is.null(at_)) {
        dt_yangzhang = ohlcv[at_, .SD, .SDcols = c(ids, new_cols)]
        ohlcv[, (new_cols) := NULL]
      }

      # rolling skewness
      print("Calculate moments.")
      new_cols = paste0("skew_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) as.vector(RollingSkew(returns_1, window = w, na_method = "ignore"))),
            by = symbol]
      if (!is.null(at_)) {
        dt_skew = ohlcv[at_, .SD, .SDcols = c(ids, new_cols)]
        ohlcv[, (new_cols) := NULL]
      }

      # rolling kurtosis
      new_cols = paste0("kurtosis_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) as.vector(RollingKurt(returns_1, window = w, na_method = "ignore"))), by = symbol]
      if (!is.null(at_)) {
        dt_kurtosis = ohlcv[at_, .SD, .SDcols = c(ids, new_cols)]
        ohlcv[, (new_cols) := NULL]
      }

      # rolling TA indicators
      print("Calculate technical indicators.")
      # ATR
      print("ATR")
      new_cols <- expand.grid("atr", c("tr", "atr", "trueHigh", "trueLow"), 14)
      new_cols <- paste(new_cols$Var1, new_cols$Var2, new_cols$Var3, sep = "_")
      ohlcv[, (new_cols) := do.call(cbind, lapply(14, function(w) as.data.frame(ATR(cbind(high, low, close), n = w)[, c(1, 2)]))), by = symbol]
      new_new_cols = paste0(new_cols, "_closedv")
      ohlcv[, (new_new_cols) := lapply(.SD, function(x) x / close), .SDcols = new_cols]
      if (!is.null(at_)) {
        dt_atr = ohlcv[at_, .SD, .SDcols = c(ids, new_cols, new_new_cols)]
        ohlcv[, c(new_cols, new_new_cols) := NULL]
      }
      # BBANDS
      print("BBANDS")
      new_cols = expand.grid("bbands", c("dn", "mavg", "up", "pctB"), windows_)
      new_cols = paste(new_cols$Var1, new_cols$Var2, new_cols$Var3, sep = "_")
      ohlcv[, (new_cols) := do.call(cbind, lapply(windows_, function(w) as.data.frame(BBands(close, n = w)))), by = symbol]
      new_cols_change <- new_cols[grep("bbands.*up|bbands.*mavg|bbands.*dn", new_cols)]
      ohlcv[, (new_cols_change) := lapply(.SD, function(x) close / x), .SDcols = new_cols_change]
      if (!is.null(at_)) {
        dt_bbands = ohlcv[at_, .SD, .SDcols = c(ids, new_cols)]
        ohlcv[, c(new_cols) := NULL]
      }
      # chaikinAD
      print("chaikinAD")
      ohlcv[, ("chaikinad_one_window") := chaikinAD(cbind(high, low, close), volume), by = symbol]
      # chaikin volatility
      print("Chaikin volatility")
      ohlcv[, ("chaikinVol_one_window") := chaikinVolatility(cbind(high, low), n = 10), by = symbol]
      # CLV
      print("CLV")
      ohlcv[, ("clv_one_window") := CLV(cbind(high, low, close)), by = symbol]
      if (!is.null(at_)) {
        cols_ = c("chaikinad_one_window", "chaikinVol_one_window", "clv_one_window")
        dt_chaikin_clv = ohlcv[at_, .SD, .SDcols = c(ids, cols_)]
        ohlcv[, c(cols_) := NULL]
      }
      # CMF
      print("CMF")
      new_cols = paste0("cmf_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) CMF(cbind(high, low, close), volume, n = w)), by = symbol]
      if (!is.null(at_)) {
        dt_cmf = ohlcv[at_, .SD, .SDcols = c(ids, new_cols)]
        ohlcv[, (new_cols) := NULL]
      }
      # CMO
      print("CMO")
      new_cols = paste0("cmo_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) CMO(close, n = w)), by = symbol]
      if (!is.null(at_)) {
        dt_cmo = ohlcv[at_, .SD, .SDcols = c(ids, new_cols)]
        ohlcv[, (new_cols) := NULL]
      }
      new_cols = paste0("cmo_volume_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) CMO(volume, n = w)), by = symbol]
      if (!is.null(at_)) {
        dt_cmo_volume = ohlcv[at_, .SD, .SDcols = c(ids, new_cols)]
        ohlcv[, (new_cols) := NULL]
      }
      # CTI
      # TOO SLOW
      # new_cols <- paste0("cti_", windows_)
      # ohlcv[, (new_cols) := lapply(windows_, function(w) c(rep(NA, w-1), CTI(close, n = w))), by = symbol]
      # DonchianChannel
      print("Donchian Channel")
      new_cols = expand.grid("dochian", c("high", "mid", "low"), windows_)
      new_cols = paste(new_cols$Var1, new_cols$Var2, new_cols$Var3, sep = "_")
      ohlcv[, (new_cols) := do.call(cbind, lapply(windows_, function(w) as.data.frame(DonchianChannel(cbind(high, low), n = w)))),
            by = symbol]
      ohlcv[, (new_cols) := lapply(.SD, function(x) close / x), .SDcols = new_cols]
      if (!is.null(at_)) {
        dt_dochian = ohlcv[at_, .SD, .SDcols = c(ids, new_cols)]
        ohlcv[, (new_cols) := NULL]
      }
      # DPO - look at warning in documentation
      # DVI
      print("DVI")
      windows__ = windows_[windows_ < 500]
      new_cols = expand.grid("dvi", c("dvi_mag", "dvi_str", "dvi"), windows__)
      new_cols = paste(new_cols$Var1, new_cols$Var2, new_cols$Var3, sep = "_")
      ohlcv[, (new_cols) := do.call(cbind, lapply(windows__, function(w) as.data.frame(DVI(close, n = w)))),
            by = symbol]
      if (!is.null(at_)) {
        dt_dvi = ohlcv[at_, .SD, .SDcols = c(ids, new_cols)]
        ohlcv[, (new_cols) := NULL]
      }
      # EMV - produces mostly Inf and Nan
      # GMMA
      print("GMMA")
      new_cols = gsub(" ", "_", colnames(GMMA(ohlcv[1:500, close])))
      new_cols = expand.grid("GMMA", new_cols)
      new_cols <- paste(new_cols$Var1, new_cols$Var2, new_cols$Var3, sep = "_")
      ohlcv[, (new_cols) := as.data.frame(GMMA(close)), by = symbol]
      ohlcv[, (new_cols) := lapply(.SD, function(x) close / x), .SDcols = new_cols]
      if (!is.null(at_)) {
        dt_gmma = ohlcv[at_, .SD, .SDcols = c(ids, new_cols)]
        ohlcv[, (new_cols) := NULL]
      }
      # keltnerChannels
      print("Keltner Channels")
      new_cols = expand.grid("keltnerchannels", c("dn", "mavg", "up"), windows_)
      new_cols = paste(new_cols$Var1, new_cols$Var2, new_cols$Var3, sep = "_")
      ohlcv[, (new_cols) := do.call(cbind, lapply(windows_, function(w) as.data.frame(keltnerChannels(cbind(high, low, close), n = w)))),
            by = symbol]
      new_cols_change <- new_cols[grep("keltnerchannels.*up|keltnerchannels.*mavg|keltnerchannels.*dn", new_cols)]
      ohlcv[, (new_cols_change) := lapply(.SD, function(x) close / x), .SDcols = new_cols_change]
      if (!is.null(at_)) {
        dt_keltnerchannels = ohlcv[at_, .SD, .SDcols = c(ids, new_cols_change)]
        ohlcv[, c(new_cols_change) := NULL]
      }
      # KST
      print("KST")
      windows__ = windows_[-length(windows_)]
      new_cols = expand.grid("kst", c("kst", "signal"), windows__)
      new_cols = paste(new_cols$Var1, new_cols$Var2, new_cols$Var3, sep = "_")
      ohlcv[, (new_cols) := do.call(cbind, lapply(windows__, function(w) as.data.frame(KST(close, n = ceiling(w / 2), nROC = w)))),
            by = symbol]
      if (!is.null(at_)) {
        dt_kst = ohlcv[at_, .SD, .SDcols = c(ids, new_cols)]
        ohlcv[, (new_cols) := NULL]
      }
      # MFI
      print("MFI")
      new_cols = paste0("mfi_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) MFI(cbind(high, low, close), volume, n = w)), by = symbol]
      if (!is.null(at_)) {
        dt_mfi = ohlcv[at_, .SD, .SDcols = c(ids, new_cols)]
        ohlcv[, (new_cols) := NULL]
      }
      # RSI
      print("RSI")
      new_cols = paste0("rsi_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) rsi(close, n = w)), by = symbol]
      if (!is.null(at_)) {
        dt_rsi = ohlcv[at_, .SD, .SDcols = c(ids, new_cols)]
        ohlcv[, (new_cols) := NULL]
      }
      # ADX
      # set window to 14 because I get error otherwise
      new_cols = expand.grid("adx", c("dip", "din", "dx", "adx"), 14)
      new_cols = paste(new_cols$Var1, new_cols$Var2, new_cols$Var3, sep = "_")
      ohlcv[, (new_cols) := do.call(cbind, lapply(14, function(w) as.data.frame(ADX(cbind(high, low, close), n = w)))),
            by = symbol]
      if (!is.null(at_)) {
        dt_adx = ohlcv[at_, .SD, .SDcols = c(ids, new_cols)]
        ohlcv[, (new_cols) := NULL]
      }
      new_cols = expand.grid("adx", c("dip", "din", "dx", "adx"), 22 * 3)
      new_cols = paste(new_cols$Var1, new_cols$Var2, new_cols$Var3, sep = "_")
      ohlcv[, (new_cols) := do.call(cbind, lapply(22 * 3, function(w) as.data.frame(ADX(cbind(high, low, close), n = w)))),
            by = symbol]
      if (!is.null(at_)) {
        dt_adx_22_3 = ohlcv[at_, .SD, .SDcols = c(ids, new_cols)]
        ohlcv[, (new_cols) := NULL]
      }
      # CCI
      new_cols = paste0("cci_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) CCI(cbind(high, low, close), n = w)), by = symbol]
      if (!is.null(at_)) {
        dt_cci = ohlcv[at_, .SD, .SDcols = c(ids, new_cols)]
        ohlcv[, (new_cols) := NULL]
      }
      # OBV
      ohlcv[, ("obv") := OBV(close, volume), by = symbol]
      if (!is.null(at_)) {
        dt_obv = ohlcv[at_, .SD, .SDcols = c(ids, "obv")]
        ohlcv[, ("obv") := NULL]
      }
      # SAR
      ohlcv[, ("sar") := as.vector(SAR(cbind(high, close))), by = symbol]
      ohlcv[, ("sar") := close / sar]
      if (!is.null(at_)) {
        dt_sar = ohlcv[at_, .SD, .SDcols = c(ids, "sar")]
        ohlcv[, ("sar") := NULL]
      }
      # WPR
      new_cols = paste0("wpr_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) WPR(cbind(high, low, close))), by = symbol]
      if (!is.null(at_)) {
        dt_wpr = ohlcv[at_, .SD, .SDcols = c(ids, new_cols)]
        ohlcv[, (new_cols) := NULL]
      }
      # AROON
      new_cols = expand.grid("aroon", c("aroonUp", "aroonDn", "oscillator"), windows_)
      new_cols = paste(new_cols$Var1, new_cols$Var2, new_cols$Var3, sep = "_")
      ohlcv[, (new_cols) := do.call(cbind, lapply(windows_, function(w) as.data.frame(aroon(cbind(high, low), n = w)))), by = symbol]
      if (!is.null(at_)) {
        dt_aroon = ohlcv[at_, .SD, .SDcols = c(ids, new_cols)]
        ohlcv[, (new_cols) := NULL]
      }

      # percent rank
      new_cols = paste0("percent_rank_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) QuantTools::roll_percent_rank(close, n = w)), by = symbol]
      if (!is.null(at_)) {
        dt_percent_rank = ohlcv[at_, .SD, .SDcols = c(ids, new_cols)]
        ohlcv[, (new_cols) := NULL]
      }

      # trading rules
      print("Calculate trading rules.")
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
      if (!is.null(at_)) {
        cols_ = c("close_above_sma200", "close_above_sma100", "close_above_sma50", "close_above_sma22",
                  "ema_above_sma200", "ema_above_sma100", "close_above_vwap_20", "close_above_vwap_50",
                  "close_above_vwap_100", "close_above_vwap_200")
        dt_trading_rules = ohlcv[at_, .SD, .SDcols = c(ids, cols_)]
        ohlcv[, c(cols_) := NULL]
      }

      # rolling linear regression model: y = 1 + date + e NOT THAT GOOD AFTER ALL ?
      print("Calculate rolling lin regression.")
      new_cols = expand.grid("lm", c("cf1", "cf2", "r2", "std1", "std2"), windows_)
      new_cols = paste(new_cols$Var1, new_cols$Var2, new_cols$Var3, sep = "_")
      ohlcv[, (new_cols) := do.call(cbind,
                                    lapply(windows_,
                                           function(w) as.data.frame(as.data.table(roll::roll_lm(1:length(close), log(close), width = w))))),
            by = symbol]
      cols_ = new_cols[grep("lm_cf", new_cols)]
      ohlcv[, (cols_) := lapply(.SD, function(x) (exp(x)^252-1) * 100), .SDcols = cols_]
      if (!is.null(at_)) {
        cols_ = unique(c(cols_, new_cols))
        dt_lm = ohlcv[at_, .SD, .SDcols = c(ids, cols_)]
        ohlcv[, (cols_) := NULL]
      }

      # rolling sharpe ratio
      print("Rolling sharpe")
      new_cols = paste0("sharpe_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) as.vector(RollingSharpe(returns_1, rep(0, length(close)), window = w,
                                                                                 na_method = "ignore"))), by = symbol]
      if (!is.null(at_)) {
        dt_sharpe = ohlcv[at_, .SD, .SDcols = c(ids, new_cols)]
        ohlcv[, (new_cols) := NULL]
      }

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
      ohlcv = generate_quantile_divergence(ohlcv, p = 0.01)
      ohlcv = generate_quantile_divergence(ohlcv, p = 0.05)
      ohlcv = generate_quantile_divergence(ohlcv, p = 0.25)
      ohlcv = generate_quantile_divergence(ohlcv, p = 0.5)
      ohlcv = generate_quantile_divergence(ohlcv, p = 0.75)
      ohlcv = generate_quantile_divergence(ohlcv, p = 0.95)
      ohlcv = generate_quantile_divergence(ohlcv, p = 0.99)
      if (!is.null(at_)) {
        dt_quantile_divergence = ohlcv[
          at_,
          .SD,
          .SDcols = data.table::patterns("symbol|date|q.*_close_divergence_", cols = names(ohlcv))]
        ohlcv[, names(.SD) := NULL,
              .SDcols = data.table::patterns("q.*_close_divergence_", cols = names(ohlcv))]
      }

      # OPENSOURCE AP --------------------------------------------------------
      print("Open source asset pricing daily variation.")

      # 52 week
      print("52 week")
      w_ = c(22 * 1:12)
      new_cols = paste0("ath_", w_)
      ohlcv[, (new_cols) := lapply(w_,
                                   function(w) close / frollapply(high, w, max, na.rm = TRUE)),
            by = symbol]

      # maximum return
      print("Rolling max returns")
      ohlcv[, max_ret := frollapply(returns_1, 22, max, na.rm = TRUE), by = symbol]
      if (!is.null(at_)) {
        cols_ = c("max_ret", new_cols)
        dt_opensource_1 = ohlcv[at_, .SD, .SDcols = c(ids, cols_)]
        ohlcv[, c(cols_) := NULL]
      }

      # dolvol
      print("DolVol")
      ohlcv[, dolvolm := close * volume, by = symbol]
      ohlcv[, dolvolm := frollsum(dolvolm, 22, na.rm=TRUE), by = symbol]

      # dolvol on windows
      w_ = c(1,22 * 1:3)
      new_cols = paste0("dolvol_", w_)
      ohlcv[, (new_cols) := lapply(w_, function(y) shift(x=dolvolm, n=y))]
      if (!is.null(at_)) {
        cols_ = c("max_ret", "dolvolm", new_cols)
        dt_opensource_2 = ohlcv[at_, .SD, .SDcols = c(ids, new_cols)]
        ohlcv[, (new_cols) := NULL]
      }

      # mom12m already caluclated in my predictors

      # MERGE ALL ---------------------------------------------------------------
      # keep only relevant columns
      if (!is.null(at_)) {
        # merge all tables if at is used
        ohlcv = Reduce(
          function(x, y)
            merge(x, y, all = TRUE),
          list(
            dt_returns,
            dt_ath,
            dt_minret,
            dt_volumes,
            dt_bidask,
            dt_pretty,
            dt_volatility,
            dt_parkinson,
            dt_rogers,
            dt_gkyz,
            dt_yangzhang,
            dt_skew,
            dt_kurtosis,
            dt_atr,
            dt_bbands,
            dt_chaikin_clv,
            dt_cmf,
            dt_cmo,
            dt_cmo_volume,
            dt_dochian,
            dt_dvi,
            dt_gmma,
            dt_keltnerchannels,
            dt_kst,
            dt_mfi,
            dt_rsi,
            dt_adx,
            dt_adx_22_3,
            dt_cci,
            dt_obv,
            dt_sar,
            dt_wpr,
            dt_aroon,
            dt_percent_rank,
            dt_trading_rules,
            dt_lm,
            dt_sharpe,
            dt_quantile_divergence,
            dt_opensource_1,
            dt_opensource_2
          )
        )
      }
      return(ohlcv)
    }
  )
)
