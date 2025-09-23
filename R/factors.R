# library(data.table)
# library(finutils)
# library(arrow)
# library(ggplot2)
#
#
# # DATA --------------------------------------------------------------------
# # Prices
# prices = qc_daily_parquet(
#   file_path = "F:/lean/data/all_stocks_daily",
#   min_obs = 252,
#   duplicates = "fast",
#   market_cap_fmp_file = "F:/data/equity/us/fundamentals/market_cap.parquet",
#   profiles_fmp = Sys.getenv("APIKEY")
# )
# prices[, month := data.table::yearmon(date)]
#
# # Downsample prices to monthly data
# pricesm = prices[, .(
#   fmp_symbol = data.table::last(fmp_symbol),
#   date       = data.table::last(date),
#   open       = data.table::first(open),
#   high       = max(high),
#   low        = min(low),
#   close      = data.table::last(close),
#   close_raw  = data.table::last(close_raw),
#   volume     = sum(volume, na.rm = TRUE),
#   market_cap = data.table::last(marketCap)
# ), by = .(symbol, month)]
#
# # Remove duplciates fro symbol FMP
# pricesm = unique(pricesm, by = c("fmp_symbol", "date"))
# anyDuplicated(pricesm, by = c("fmp_symbol", "date"))
#
# # Fundamentals
# URIFACTORS = "F:/data/equity/us/predictors_daily/factors"
# fundamentals = read_parquet(file.path(URIFACTORS, "fundamental_factors.parquet"))
# fundamentals[, month := data.table::yearmon(as.Date(acceptedDate))]
# dim(fundamentals)
#
# # Plot n through t
# na.omit(fundamentals[, .N, by = month][order(month)]) |>
#   ggplot(aes(month, TTR::SMA(N), 4)) +
#   geom_line()
#
# # Balance sheet n
# x = lapply(list.files("F:/data/equity/us/fundamentals/balance-sheet-statement-bulk/quarter", full.names = TRUE), fread)
# x = rbindlist(x)
# x[, month := data.table::yearmon(acceptedDate)]
# na.omit(x[, .N, by = month][order(month)]) |>
#   ggplot(aes(month, TTR::SMA(N), 4)) +
#   geom_line()
# na.omit(x[, .N, by = month][order(month)][month > 2020]) |>
#   ggplot(aes(month, TTR::SMA(N), 4)) +
#   geom_line()
#
#
# # Select columns
# colnames(fundamentals)[grepl("price", colnames(fundamentals), ignore.case = TRUE)]
# fund_sample = fundamentals[, .(
#   symbol, acceptedDate, date = as.Date(acceptedDate), month,
#   bm,               # value
#   pbRatio,          # value
#   returnOnEquity,   # profitability
#   returnOnAssets,   # profitability
#   marketCap,        # size
#   freeCashFlowYield)]
#
# # Remove duplicates for fundamentals
# fund_sample = unique(fund_sample, by = c("symbol", "date"))
# anyDuplicated(fund_sample, by = c("symbol", "date"))
#
# # Merge pricesm and fundamentals
# fund_sample[, date_fund := date]
# dt = fund_sample[pricesm, on = c("symbol" = "fmp_symbol", "date"), roll = Inf]
# dt[, report_days_lag := as.integer(date - date_fund)]
# dt = na.omit(dt, cols = "acceptedDate")
# dt = dt[report_days_lag < 150]
# setnames(dt, c("i.symbol", "i.month"), c("symbol_qc", "month_prices"))
#
# # Checks
# anyDuplicated(dt, by = c("symbol", "date"))
#
# # Check market_cap
# dt_[, .(marketCap, market_cap)]
# na.omit(dt_[, .(marketCap, market_cap)])
# x_ = which(na.omit(dt_[, .(marketCap, market_cap)])[, (marketCap / market_cap) > 10000000])
# dt_[x_]
#
#
#
# # DESCRIPTIVE -------------------------------------------------------------
# # Number of firms
# dt[, .N, by = month_prices][order(month_prices)] |>
#   _[, .(date = zoo::as.yearmon(month_prices), N)] |>
#   plot()
#
#
#
# # FACTOR ------------------------------------------------------------------
# # Parameters
# VAR = "marketCap"
#
# # Remove NA values for factor
# dt_ = na.omit(dt, cols = VAR)
#
# # Remove outliers
# # If value of indicator is greater that 98% of all values, remove it.
# dt_[, outlier := (x / sum(x)) > 0.2, by = month_prices, env = list(x = VAR)]
# symbols = dt_[outlier == 1, unique(symbol)]
# setkey(dt_, symbol)
# setorder(dt_, symbol, month_prices)
# dt_ = dt_[!.(symbols)]
#
# # Plot deciles
# dt_[, let(
#   top01 = fifelse(x >= quantile(x, 0.99), 1, 0),
#   top05 = fifelse(x >= quantile(x, 0.95), 1, 0),
#   top10 = fifelse(x >= quantile(x, 0.90), 1, 0),
#   top25 = fifelse(x >= quantile(x, 0.75), 1, 0)
# ), by = month_prices, env = list(x = VAR)]
# dt_[, .(
#   `Largest 1% of stocks`  = sum(x[top01 == 1]) / sum(x),
#   `Largest 5% of stocks`  = sum(x[top05 == 1]) / sum(x),
#   `Largest 10% of stocks` = sum(x[top10 == 1]) / sum(x),
#   `Largest 25% of stocks` = sum(x[top25 == 1]) / sum(x)
# ), by = month_prices, env = list(x = VAR)] |>
#   melt(data = _, id.vars = "month_prices") |>
#   ggplot(aes(
#     x = month_prices,
#     y = value,
#     color = variable,
#     linetype = variable)) +
#   geom_line() +
#   scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
#   labs(
#     x = NULL, y = NULL, color = NULL, linetype = NULL,
#     title = "Percentage of total market capitalization in largest stocks"
#   )
#
#
# # Check highest by month
# setorder(dt_, month, -market_cap)
# tail(dt_[, head(.SD), by = month], 80)
#
#
#
#
#
# # ARCHIVE -----------------------------------------------------------------
# # # Remove duplicates for fundamentals
# # anyDuplicated(fundamentals, by = c("symbol", "acceptedDate"))
# # anyDuplicated(fundamentals, by = c("symbol", "month"))
# # fundamentals[duplicated(fundamentals, by = c("symbol", "acceptedDate")) |
# #                duplicated(fundamentals, by = c("symbol", "acceptedDate"), fromLast = TRUE),
# #              1:10]
# # fundamentals = unique(fundamentals, by = c("symbol", "acceptedDate"))
# # fundamentals = unique(fundamentals, by = c("symbol", "month"))
# # anyDuplicated(fundamentals, by = c("symbol", "acceptedDate"))
# # anyDuplicated(fundamentals, by = c("symbol", "month"))
# #
# # # Downsample fundamentals to monthly data
# # fundamentals[, month_fundamentals := month]
# # fundm = CJ(
# #   symbol = fundamentals[, unique(symbol)],
# #   month_fundm = fundamentals[, unique(data.table::yearmon(acceptedDate))]
# # )
# # fundm = fundamentals[fundm, on = c("symbol", "month" = "month_fundm")]
# # fundm[, .(symbol, date, acceptedDate, month, month_fundamentals)]
# # fundm[symbol == "AAPL", .(symbol, date, acceptedDate, month, month_fundamentals)][1:99]
# # fundm[symbol == "AAPL"][1:99][, lapply(.SD, nafill, type = "locf"), by = "symbol", .SDcols = is.numeric][, 1:10]
# # fundm[, names(.SD) := lapply(.SD, nafill, type = "locf"), by = "symbol", .SDcols = is.numeric]
# # fundm[symbol == "AAPL", .(symbol, date, acceptedDate, month, month_fundamentals)][100:199]
# # fundm[, 1:14]
# # fundm[, (ncol(fundm)-10):ncol(fundm)]
# # fundm[, .SD, .SDcols = !is.numeric]
# # fundm[, let(
# #   date             = NULL,
# #   reportedCurrency = NULL,
# #   fillingDate      = NULL,
# #   acceptedDate     = NULL,
# #   period           = NULL
# # )]
# # fundm = na.omit(fundm, cols = "month")
# # dim(fundm)
# #
# # # Handle NA'a
# # sum_na = fundm[, sort(vapply(.SD, function(x) sum(is.na(x)) / length(x), FUN.VALUE = numeric(1L)))]
# # hist(sum_na)
# # fundm[is.na(debtRepayment_ttm)]
# # fundm[symbol == "AAPL"][is.na(debtRepayment_ttm), .(symbol, cik, month)]
# # fundm[symbol == "AAPL"][is.na(shortTermInvestments), .(symbol, cik, month, shortTermInvestments)]
# # fundm[symbol == "AAPL"][is.na(cik), .(symbol, cik, month)]
# # fundm = na.omit(fundm, cols = "cik")
# # sum_na = fundm[, sort(vapply(.SD, function(x) sum(is.na(x)) / length(x), FUN.VALUE = numeric(1L)))]
# # hist(sum_na)
# # dim(fundm)
# # fundm[symbol == "AAPL"]
# #
# # # Merge prices and fundamentals
# # pricesm
# # fundm[, 1:14]
# # colnames(fundm)
# # fund_sample = fundm[, .(symbol, weightedAverageShsOut, month, month_fundamentals, marketCap)]
# # dt = fund_sample[pricesm, on = c("symbol" = "fmp_symbol", "month")]
# # setnames(dt, c("symbol", "i.symbol"), c("fmp_symbol", "symbol"))
#
#
