context("roll-exuber")  # Our file is called "test-check_output.R"
library(testthat)        # load testthat package
library(finfeatures)       # load our package


# Test whether the output is a data table
# test_that(
#   "RollingExuber returns an dataframa",
#   {
#     data(spy_hour)
#     OhlcvInstance = Ohlcv$new(spy_hour, date_col = "datetime")
#     RollingExuberInit = RollingExuber$new(windows = 200,
#                                           workers = 1L,
#                                           at = c(300:310, 500:510),
#                                           lag = 1L,
#                                           na_pad = TRUE,
#                                           simplify = FALSE)
#     x = RollingExuberInit$get_rolling_features(OhlcvInstance)
#     expect_s3_class(x, "data.table")
#   })

# Test if table contains symbol and date column
# test_that(
#   "RollingExuber object contains symbol and date column",
#   {
#     data(spy_hour)
#     OhlcvInstance = Ohlcv$new(spy_hour, date_col = "datetime")
#     RollingExuberInit = RollingExuber$new(windows = 200,
#                                           workers = 1L,
#                                           at = c(300:310, 500:510),
#                                           lag = 1L,
#                                           na_pad = TRUE,
#                                           simplify = FALSE)
#     x = RollingExuberInit$get_rolling_features(OhlcvInstance)
#     expect_true(all(c("symbol", "date") %in% colnames(x)))
#   })
