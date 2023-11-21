#' #' @title RollingGpd2 Class
#' #'
#' #' @description
#' #' Function calculates GPD risk measures on rolling window from gpd package.
#' #'
#' #' @export
#' #' @examples
#' #' data(spy_hour)
#' #' OhlcvInstance = Ohlcv$new(spy_hour, date_col = "datetime")
#' #' RollingGpdInit = RollingGpd2$new(windows = 200,
#' #'                                  workers = 1L,
#' #'                                  at = c(300, 500),
#' #'                                  lag = 0L,
#' #'                                  threshold = 0.03)
#' #' x = RollingGpdInit$get_rolling_features(OhlcvInstance)
#' #' head(x)
#' #' RollingGpdInit = RollingGpd2$new(windows = 200,
#' #'                                  workers = 2L,
#' #'                                  at = c(300, 500),
#' #'                                  lag = 0L,
#' #'                                  threshold = c(0,02, 0.03))
#' #' x = RollingGpdInit$get_rolling_features(OhlcvInstance)
#' #' head(x)
#' RollingGpd2 = R6::R6Class(
#'   "RollingGpd2",
#'   inherit = RollingGeneric,
#'
#'   public = list(
#'
#'     #' @field threshold Threshold parameter for returns. Returns below/above this threshold will be input.
#'     threshold_q = NULL,
#'
#'     #' @description
#'     #' Create a new RollingGpd object.
#'     #'
#'     #' @param windows Vector of windows that will be applied on features.
#'     #' @param workers Number of workers. Greater than 1 for parallle processing
#'     #' @param lag Lag variable in runner package.
#'     #' @param at Argument at in runner package.
#'     #' @param threshold Threshold parameter for returns. Returns below/above this threshold will be input.
#'     #'
#'     #' @return A new `RollingGpd` object.
#'     initialize = function(windows,
#'                           workers,
#'                           lag,
#'                           at,
#'                           threshold_q = 0.05) {
#'
#'       # define all params combination
#'       private$params <- expand.grid(threshold_q = threshold_q,
#'                                     stringsAsFactors = FALSE)
#'       colnames(private$params) <- c("threshold_q")
#'
#'       # super initialize from RollingGeneric
#'       super$initialize(
#'         windows,
#'         workers,
#'         lag,
#'         at,
#'         private$packages
#'       )
#'     },
#'
#'     #' @description
#'     #' Help function for calculating GPD features.
#'     #'
#'     #' @param x vector of (filtered) returns
#'     #' @param hill_threshold look at ptsuite package
#'     #' @param suffix suffix for column names
#'     #'
#'     #' @return Data.table with new features
#'     estimate_gpd = function(r, threshold = 0.03, suffix = '_left') {
#'       columns_names <- c(
#'         'ptest',
#'         'hill_shape',
#'         'scales_geometric_percentiles_method',
#'         'scales_least_squares',
#'         'scales_method_of_moments',
#'         'scales_modified_percentiles_method',
#'         'scales_weighted_least_squares',
#'         'shapes_geometric_percentiles_method',
#'         'shapes_least_squares',
#'         'shapes_method_of_moments',
#'         'shapes_modified_percentiles_method',
#'         'shapes_weighted_least_squares'
#'       )
#'       columns_names <- paste0(columns_names, suffix)
#'       if (length(x) == 0) {
#'         risks <- data.table(t(rep(0, length(columns_names))))
#'         setnames(risks, colnames(risks), columns_names)
#'       } else {
#'         gpd_model <- evir::gpd(r, threshold)
#'         tp <- evir::tailplot(gpd_model)
#'         estimates <- evir::riskmeasures(gpd_model, 0.01)
#'         evir::
#'         evir::gpd.sfall(gpd_model, 0.01)
#'
#'         estimates <- as.data.table(ptsuite::generate_all_estimates(x))
#'         shapes <- data.table::dcast(estimates[, 1:2], . ~ `Method.of.Estimation`, value.var = 'Shape.Parameter')
#'         shapes <- shapes[, 2:ncol(shapes)]
#'         colnames(shapes) <- paste0('shapes_', gsub(" ", "", tolower(colnames(shapes))))
#'         scales <- data.table::dcast(estimates[, c(1, 3)], . ~ `Method.of.Estimation`, value.var = 'Scale.Parameter')
#'         scales <- scales[, 2:ncol(scales)]
#'         colnames(scales) <- paste0('scales_', gsub(" ", "", tolower(colnames(scales))))
#'         hill_estimate <- ptsuite::alpha_hills(x, hill_threshold, FALSE)
#'         hill_shape <- hill_estimate$shape
#'         risks <- as.data.table(data.frame(as.list(c(scales, shapes))))
#'         risks <- cbind(ptest, hill_shape, risks)
#'         colnames(risks) <- paste0(colnames(risks), suffix)
#'       }
#'       return(risks)
#'     },
#'
#'     #' @description
#'     #' Function calculates radf values from exuber package on rolling window.
#'     #'
#'     #' @param x Ohlcv object.
#'     #' @param window Rolling window lengths.
#'     #' @param price_col Prcie column in Ohlcv
#'     #' @param params Vector of parameters
#'     #'
#'     #' @return Calculate rolling radf features from exuber package.
#'     rolling_function = function(x, window, price_col, params) {
#'
#'       # check if there is enough data
#'       if (length(unique(x$symbol)) > 1) {
#'         return(NA)
#'       }
#'
#'       # returns
#'       r <- na.omit(x$returns)
#'
#'       # pareto left tail
#'       threshold <- quantile(r, params$threshold_q)
#'       risks_left <- evir::gpd(-r, -threshold)
#'
#'       # pareto test right tail
#'       threshold <- quantile(r, 1 - params$threshold_q)
#'       risks_right <- evir::gpd(r, threshold)
#'
#'       # estimate shape parameters
#'       pareto_tests <- cbind(risks_left, risks_right)
#'       results <- as.data.table(pareto_tests)
#'       colnames(results) <- paste(colnames(results), window, params * 100, sep = "_")
#'       return(results)
#'     }
#'   ),
#'
#'   private = list(
#'     packages = "evd",
#'     params = NULL
#'   )
#' )
