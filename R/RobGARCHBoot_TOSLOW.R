#' # library(finfeatures)
#' # library("RobGARCHBoot")
#' #
#' #
#' # data("spy_hour")
#' #
#' #
#' #
#' # boot = RobGARCHBoot(returnsexample[1:500], n.boot = 100, n.ahead = 1)
#' #
#' # # Robust bootstrap forecast densities for returns and volatilities
#' # boot = RobGARCHBootParallel(returnsexample[1:500], n.boot = 100, n.ahead = 1, ncl = 4L)
#' # # Obtaining the forecast intervals for returns (95%)
#' # quantile(boot[[1]], prob = c(0.025, 0.975))
#' # # Obtaining the forecast intervals for volatilities (95%)
#' # quantile(boot[[2]], prob = c(0.025, 0.975))
#' # # Risk measures can also be obtained
#' # VaR1 = quantile(boot[[1]], prob = 0.01)
#' #
#' #
#' # (50000 * 1) / 60 / 60
#' #
#' #
#' #' @title RollingExuber Class
#' #'
#' #' @description
#' #' Function calculates radf values from exuber package on rolling window.
#' #'
#' #' @export
#' #' @examples
#' #' data(spy_hour)
#' #' OhlcvInstance = Ohlcv$new(spy_hour, date_col = "datetime")
#' #' RollingExuberInit = RollingExuber$new(windows = 200,
#' #'                                       workers = 1L,
#' #'                                       at = c(300:310, 500:510),
#' #'                                       lag = 1L,
#' #'                                       na_pad = TRUE,
#' #'                                       simplify = FALSE)
#' #' x = RollingExuberInit$get_rolling_features(OhlcvInstance)
#' #' head(x)
#' #' # parallel and multiple windows
#' #' RollingExuberInit = RollingExuber$new(windows = c(200, 400),
#' #'                                       workers = 2L,
#' #'                                       at = c(300:310, 500:510),
#' #'                                       lag = 1L,
#' #'                                       na_pad = TRUE,
#' #'                                       simplify = FALSE)
#' #' x = RollingExuberInit$get_rolling_features(OhlcvInstance)
#' #' head(x)
#' RollingExuber = R6::R6Class(
#'   "RollingExuber",
#'   inherit = RollingGeneric,
#'
#'   public = list(
#'
#'     #' @field exuber_lag LAg to use in exuber, see exuber package.
#'     exuber_lag = NULL,
#'
#'     #' @description
#'     #' Create a new RollingExuber object.
#'     #'
#'     #' @param windows Vector of windows that will be applied on features.
#'     #' @param workers Number of workers. Greater than 1 for parallle processing
#'     #' @param lag Lag variable in runner package.
#'     #' @param at Argument at in runner package.
#'     #' @param na_pad Argument na_pad in runner package.
#'     #' @param simplify Argument simplify in runner package.
#'     #' @param exuber_lag Exuber lag, see exuber package
#'     #'
#'     #' @return A new `RollingExuber` object.
#'     initialize = function(windows, workers, lag, at, na_pad, simplify, exuber_lag = 0L) {
#'       self$exuber_lag = exuber_lag
#'
#'       super$initialize(
#'         windows,
#'         workers,
#'         lag,
#'         at,
#'         na_pad,
#'         simplify
#'       )
#'     },
#'
#'     #' @description
#'     #' Function calculates radf values from exuber package on rolling window.
#'     #'
#'     #' @param data X field of Ohlcv object
#'     #' @param window window length. This argument is given internaly
#'     #'
#'     #' @return Calculate rolling radf features from exuber package.
#'     rolling_function = function(data, window) {
#'
#'       # check if there is enough data
#'       if (length(unique(data$symbol)) > 1) {
#'         print(paste0("not enough data for symbol ", data$symbol[1]))
#'         return(NA)
#'       }
#'
#'       # # Robust bootstrap forecast densities for returns and volatilities
#'       # boot = RobGARCHBoot(returnsexample, n.boot = 100, n.ahead = 1)
#'       # # Obtaining the forecast intervals for returns (95%)
#'       # quantile(boot[[1]], prob = c(0.025, 0.975))
#'       # # Obtaining the forecast intervals for volatilities (95%)
#'       # quantile(boot[[2]], prob = c(0.025, 0.975))
#'       # # Risk measures can also be obtained
#'       # VaR1 = quantile(boot[[1]], prob = 0.01)
#'
#'
#'
#'       # calculate radf valuies and save
#'       y <- tryCatch(radf(data$close, lag = self$exuber_lag, minw = psy_minw(window)), error = function(e) NA)
#'       if (all(is.na(y))) {
#'         return(NULL)
#'       } else {
#'         stats <- exuber::tidy(y)
#'         bsadf <- data.table::last(exuber::augment(y))[, 4:5]
#'         result <- cbind(symbol = data$symbol[1], date = data$date[length(data$date)], stats, bsadf)
#'         result$id <- NULL
#'         colnames(result)[3:ncol(result)] <- paste("exuber", window, self$exuber_lag, colnames(result)[3:ncol(result)], sep = "_")
#'         return(as.data.table(result))
#'       }
#'     }
#'   )
#' )
