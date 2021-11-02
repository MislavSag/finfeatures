#' @title RollingGpd Class
#'
#' @description
#' Function calculates GPD risk measures on rolling window from ptsuite pakcage.
#'
#' @export
#' @examples
#' data(spy_hour)
#' OhlcvInstance = Ohlcv$new(spy_hour, date_col = "datetime")
#' RollingGpdInit = RollingGpd$new(windows = 200,
#'                                 workers = 1L,
#'                                 at = c(300, 500),
#'                                 lag = 0L,
#'                                 na_pad = TRUE,
#'                                 simplify = FALSE,
#'                                 threshold = 0.01)
#' x = RollingGpdInit$get_rolling_features(OhlcvInstance)
#' head(x)
#' # OHLCV features
#' # parallel and multiple windows
#' RollingGasInit = RollingGpd$new(windows = c(200, 300),
#'                                 workers = 4L,
#'                                 at = c(300:310, 500:510),
#'                                 lag = 1L,
#'                                 na_pad = TRUE,
#'                                 simplify = FALSE,
#'                                 threshold = 0.01)
#' x = RollingGasInit$get_rolling_features(OhlcvInstance)
#' head(x)
RollingGpd = R6::R6Class(
  "RollingGpd",
  inherit = RollingGeneric,

  public = list(

    #' @field threshold Threshold parameter for returns. Returns below/above this threshold will be input.
    threshold = NULL,

    #' @description
    #' Create a new RollingGpd object.
    #'
    #' @param windows Vector of windows that will be applied on features.
    #' @param workers Number of workers. Greater than 1 for parallle processing
    #' @param lag Lag variable in runner package.
    #' @param at Argument at in runner package.
    #' @param na_pad Argument na_pad in runner package.
    #' @param simplify Argument simplify in runner package.
    #' @param threshold Threshold parameter for returns. Returns below/above this threshold will be input.
    #'
    #' @return A new `RollingGpd` object.
    initialize = function(windows, workers, lag, at, na_pad, simplify,
                          threshold = 0.03) {

      self$threshold = threshold

      super$initialize(
        windows,
        workers,
        lag,
        at,
        na_pad,
        simplify
      )
    },

    #' @description
    #' Help function for calculating GPD features.
    #'
    #' @param x vector of (filtered) returns
    #' @param hill_threshold look at ptsuite package
    #' @param suffix suffix for column names
    #'
    #' @return Data.table with new features
    estimate_gpd = function(x, hill_threshold = 0.02, suffix = '_right') {
      columns_names <- c(
        'ptest',
        'hill_shape',
        'scales_geometric_percentiles_method',
        'scales_least_squares',
        'scales_method_of_moments',
        'scales_modified_percentiles_method',
        'scales_weighted_least_squares',
        'shapes_geometric_percentiles_method',
        'shapes_least_squares',
        'shapes_method_of_moments',
        'shapes_modified_percentiles_method',
        'shapes_weighted_least_squares'
      )
      columns_names <- paste0(columns_names, suffix)
      if (length(x) == 0) {
        risks <- data.table(t(rep(0, length(columns_names))))
        setnames(risks, colnames(risks), columns_names)
      } else {
        ptest <- pareto_test(x)$`p-value`
        estimates <- as.data.table(generate_all_estimates(x))
        shapes <- data.table::dcast(estimates[, 1:2], . ~ `Method.of.Estimation`, value.var = 'Shape.Parameter')
        shapes <- shapes[, 2:ncol(shapes)]
        colnames(shapes) <- paste0('shapes_', gsub(" ", "", tolower(colnames(shapes))))
        scales <- data.table::dcast(estimates[, c(1, 3)], . ~ `Method.of.Estimation`, value.var = 'Scale.Parameter')
        scales <- scales[, 2:ncol(scales)]
        colnames(scales) <- paste0('scales_', gsub(" ", "", tolower(colnames(scales))))
        hill_estimate <- alpha_hills(x, hill_threshold, FALSE)
        hill_shape <- hill_estimate$shape
        risks <- as.data.table(data.frame(as.list(c(scales, shapes))))
        risks <- cbind(ptest, hill_shape, risks)
        colnames(risks) <- paste0(colnames(risks), suffix)
      }
      return(risks)
    },

    #' @description
    #' Function calculates GPD risk measures on rolling window from ptsuite pakcage.
    #'
    #' @param data X field of Ohlcv object
    #' @param window window length. This argument is given internaly
    #'
    #' @return Calculate rolling features from ptsuite package.
    rolling_function = function(data, window) {

      # check if there is enough data
      if (length(unique(data$symbol)) > 1) {
        print(paste0("not enough data for symbol ", data$symbol[1]))
        return(NA)
      }

      # returns
      r <- na.omit(data$returns)

      # pareto left tail
      x_left <- r[r < -self$threshold] * -1
      risks_left <- self$estimate_gpd(x_left, hill_threshold = 0.02, suffix = '_left')

      # pareto test right tail
      x_right <- r[r > self$threshold]
      risks_right <- self$estimate_gpd(x_right, hill_threshold = 0.02)

      # estimate shape parameters
      pareto_tests <- cbind(risks_left, risks_right)
      results <- data.table(symbol = data$symbol[1], date = data$date[length(data$date)], pareto_tests)
      colnames(results)[3:ncol(results)] <- paste(colnames(results)[3:ncol(results)], window, self$threshold * 100, sep = "_")
      return(results)

    }
  )
)
