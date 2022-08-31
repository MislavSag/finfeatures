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
      # ohlcv <- as.data.table(stocks)
      # windows_ = c(8 * 5, 8 * 22)
      # at_ <- c(500, 1000)
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
        keep_indecies <- lapply(at_, function(x) (max(x - max(windows_), 1)):x)
        keep_indecies <- unique(unlist(keep_indecies))
        ohlcv <- ohlcv[keep_indecies]
      }

      # close ATH
      ohlcv[, close_ath := (cummax(high) - close) / cummax(high), by = symbol]
      new_cols <- paste0("ath_", windows_)
      ohlcv[, (new_cols) := lapply(c(1, windows_), function(w) (shift(frollapply(high, w, max), 1L) / close) - 1), by = symbol]

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
      new_cols <- paste0("skew_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) as.vector(RollingSkew(returns_1, window = w, na_method = "ignore"))), by = symbol]

      # rolling kurtosis
      new_cols <- paste0("kurtosis_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) as.vector(RollingKurt(returns_1, window = w, na_method = "ignore"))), by = symbol]

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
      ohlcv[, ema_above_sma200 := (ema(close, n = 50) - sma(close, n = 200)) / sma(close, n = 200),
            by = symbol]
      ohlcv[, ema_above_sma100 := (ema(close, n = 50) - sma(close, n = 100)) / sma(close, n = 100),
            by = symbol]
      ohlcv[, close_above_vwap_20 := (close - TTR::VWAP(close, volume, n = 20)) / TTR::VWAP(close, volume, n = 20),
            by = symbol]
      ohlcv[, close_above_vwap_50 := (close - TTR::VWAP(close, volume, n = 50)) / TTR::VWAP(close, volume, n = 50),
            by = symbol]

      # rolling volume
      # new_cols <- paste0("volume_", windows_)
      # ohlcv[, (new_cols) := lapply(windows_, function(w) frollmean(volume / 1000, n = w, na.rm = TRUE)), by = symbol]
      new_cols <- paste0("volume_rate_", windows_)
      ohlcv[, (new_cols) := lapply(windows_, function(w) volume / shift(volume, n = w) - 1), by = symbol]

      # rolling linear regression model: y = 1 + date + e NOT THAT GOOD AFTER ALL ?
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

      # support / resistance
      # ohlcv[, close_round := round(close, 0)]
      # windows__ <- unique(c(windows_, 200, 500))
      # new_cols <- paste0("rolling_mode_", windows__)
      # # x <- scale(ohlcv$close_round)
      # # plot(x)
      # ohlcv[, (new_cols) := lapply(windows__, function(w) frollapply(close_round, w, function(x) {
      #   as.integer(names(sort(table(x), decreasing = TRUE))[1])
      #   # TODO: faster way sto calculate mode: https://stackoverflow.com/questions/55212746/rcpp-fast-statistical-mode-function-with-vector-input-of-any-type
      # })), by = symbol]
      # new_cols_close <- paste0(new_cols, "_ratio")
      # ohlcv[, (new_cols_close) := lapply(.SD, function(x) (close / x) - 1), .SDcols = new_cols]

      # TODO: support / resistance VWA
      # new_cols_ <- paste0("rolling_mode_vma_", windows__)
      # ohlcv[, (new_cols_) := lapply(.SD, function(w) TTR::VWAP(w, volume, 10)), by = symbol, .SDcols = new_cols]

      # ohlcv[, rolling_mode_200_vwa := TTR::VWAP(rolling_mode_200, volume, 200)]
      # ohlcv[, rolling_mode_500_vwa := TTR::VWAP(rolling_mode_500, volume, 500)]

      # price / support / resistance
      # ohlcv[, rolling_mode_40_close := (close / rolling_mode_40) - 1]
      # ohlcv[, rolling_mode_200_close := (close / rolling_mode_200) - 1]
      # ohlcv[, rolling_mode_500_close := (close / rolling_mode_500) - 1]
      # ohlcv[, rolling_mode_200_vma_close := (close / rolling_mode_200_vwa) - 1]
      # ohlcv[, rolling_mode_500_vma_close := (close / rolling_mode_500_vwa) - 1]

      ####### DEBUG #######
      # library(ggplot2)
      # ggplot(ohlcv[30000:nrow(ohlcv)], aes(x = date)) +
      #   geom_line(aes(y = close)) +
      #   geom_line(aes(y = rolling_mode_40), color = "red") +
      #   geom_line(aes(y = rolling_mode_176), color = "green") +
      #   geom_line(aes(y = rolling_mode_200), color = "blue") +
      #   geom_line(aes(y = rolling_mode_500), color = "brown")
      #
      # ggplot(ohlcv[20000:nrow(ohlcv)], aes(x = date)) +
      #   geom_line(aes(y = close)) +
      #   geom_line(aes(y = rolling_mode_500), color = "brown") +
      #   geom_line(aes(y = rolling_mode_500_vwa), color = "blue")
      # ggplot(ohlcv[20000:nrow(ohlcv)], aes(x = date)) +
      #   geom_line(aes(y = close)) +
      #   geom_line(aes(y = rolling_mode_200), color = "brown") +
      #   geom_line(aes(y = rolling_mode_200_vwa), color = "blue")
      # ggplot(ohlcv[30000:nrow(ohlcv)], aes(x = date)) +
      #   geom_line(aes(y = close)) +
      #   geom_line(aes(y = rolling_mode_500), color = "brown") +
      #   geom_line(aes(y = rolling_mode_500_vwa), color = "blue")
      #
      # # backtest
      # dates_ <- ohlcv$date
      # returns_ <- ohlcv$returns_1
      # indicator_ <- ohlcv$rolling_mode_200_vma_close
      #
      # backtest <- function(returns, indicator, threshold, return_cumulative = TRUE) {
      #   sides <- vector("integer", length(indicator))
      #   for (i in seq_along(sides)) {
      #     if (i == 1 || is.na(indicator[i-1])) {
      #       sides[i] <- NA
      #     } else if (indicator[i-1] < threshold) {
      #       sides[i] <- 0
      #     } else {
      #       sides[i] <- 1
      #     }
      #   }
      #   sides <- ifelse(is.na(sides), 1, sides)
      #   returns_strategy <- returns * sides
      #   if (return_cumulative) {
      #     return(PerformanceAnalytics::Return.cumulative(returns_strategy))
      #   } else {
      #     return(returns_strategy)
      #   }
      # }
      #
      # x <- backtest(returns_, indicator_, 0, return_cumulative = FALSE)
      # data_ <- cbind(benchmark = ohlcv$returns_1, strategy = xts::xts(x, order.by = dates_))
      # data_ <- na.omit(data_)
      # PerformanceAnalytics::charts.PerformanceSummary(data_)
      #
      # # backtest performance
      # Performance <- function(x) {
      #   cumRetx = Return.cumulative(x)
      #   annRetx = Return.annualized(x, scale=252 * 8)
      #   sharpex = SharpeRatio.annualized(x, scale=252 * 8)
      #   winpctx = length(x[x > 0])/length(x[x != 0])
      #   annSDx = sd.annualized(x, scale=252 * 8)
      #
      #   DDs <- findDrawdowns(x)
      #   maxDDx = min(DDs$return)
      #   maxLx = max(DDs$length)
      #
      #   Perf = c(cumRetx, annRetx, sharpex, winpctx, annSDx, maxDDx, maxLx)
      #   names(Perf) = c("Cumulative Return", "Annual Return","Annualized Sharpe Ratio",
      #                   "Win %", "Annualized Volatility", "Maximum Drawdown", "Max Length Drawdown")
      #   return(Perf)
      # }
      #
      # library(PerformanceAnalytics)
      # Performance(data_$strategy)
      # Performance(data_$benchmark)
      ####### DEBUG #######


      # estimate changepoints breaks
      # for (i in c(370, 500, 1000, 5000)) {
      #   # ohlcv[, paste(c('breaks', 'changes'), i, sep = '_') := self$get_changepoints(returns, method = 'Mood', i), by = .(symbol)]
      #   ohlcv[, paste(c('breaks', 'changes'), i, sep = '_') := self$get_changepoints(returns, method = 'Mood', i), by = .(symbol)]
      # }

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

# timeSeries = ohlcv$close[1:1000]
# tolerance=0.01
# nChunks=10
# nPoints=3
# plotChart=TRUE
# detectSupportResistance <- function(timeSeries, tolerance=0.01, nChunks=10, nPoints=3, plotChart=TRUE)
# {
#   # detect maximums and minimums
#   N = length(timeSeries)
#   stp = floor(N / nChunks)
#   minz = array(0.0, dim=nChunks)
#   whichMinz = array(0, dim=nChunks)
#   maxz = array(0.0, dim=nChunks)
#   whichMaxz = array(0, dim=nChunks)
#   for(j in 1:(nChunks-1)) {
#     lft = (j-1)*stp + 1  #left and right elements of each chunk
#     rght = j*stp
#     whichMinz[j] = which.min(timeSeries[lft:rght]) + lft
#     minz[j] = min(timeSeries[lft:rght])
#     whichMaxz[j] = which.max(timeSeries[lft:rght]) + lft
#     maxz[j] = max(timeSeries[lft:rght])
#   }
#   # last chunk
#   lft = j*stp + 1  #left and right elements of each chunk
#   rght = N
#   whichMinz[nChunks] = which.min(timeSeries[lft:rght]) + lft
#   minz[nChunks] = min(timeSeries[lft:rght])
#   whichMaxz[nChunks] = which.max(timeSeries[lft:rght]) + lft
#   maxz[nChunks] = max(timeSeries[lft:rght])
#
#   result = list()
#   result[["minima"]] = NULL
#   result[["minimaAt"]] = NULL
#   result[["maxima"]] = NULL
#   result[["maximaAt"]] = NULL
#   span = tolerance * (max(maxz) - min(minz))
#
#   rang = order(minz)[1:nPoints]
#   if((minz[rang[nPoints]] - minz[rang[1]]) <= span) {
#     result[["minima"]] = minz[rang[1:nPoints]]
#     result[["minimaAt"]] = whichMinz[rang[1:nPoints]]
#   }
#
#   rang = order(maxz, decreasing = TRUE)[1:nPoints]
#   if((maxz[rang[1]] - maxz[rang[nPoints]]) <= span) {
#     result[["maxima"]] = maxz[rang[1:nPoints]]
#     result[["maximaAt"]] = whichMaxz[rang[1:nPoints]]
#   }
#
#   if(plotChart) {
#     ts.plot(timeSeries)
#     points(whichMinz, minz, col="blue")
#     points(whichMaxz, maxz, col="red")
#     if(!is.null(result[["minima"]])  &&  !is.null(result[["minimaAt"]]))
#       abline(lm(result[["minima"]] ~  result[["minimaAt"]]))
#     if(!is.null(result[["maxima"]])  &&  !is.null(result[["maximaAt"]]))
#       abline(lm(result[["maxima"]] ~  result[["maximaAt"]]))
#   }
#
#   return(result)
# }
