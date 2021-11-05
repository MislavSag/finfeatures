#' @import data.table
#' @import checkmate
#' @import TTR
#' @import forecast
#' @import runner
#' @import theft
#' @import zoo
#' @import parallel
#' @import GAS
#' @import ptsuite
#' @import reticulate
#' @import doParallel
#' @import backCUSUM
#' @import RollingWindow
#' @import mlr3misc
#' @import cpm
#' @importFrom R6 R6Class is.R6
#' @importFrom roll roll_sd roll_lm roll_quantile
#' @importFrom QuantTools roll_percent_rank sma ema rsi
#' @importFrom bidask spread
#' @importFrom parallel makeCluster clusterExport clusterCall stopCluster
#' @importFrom exuber radf psy_minw augment tidy
#' @importFrom utils data head tail getFromNamespace
#' @importFrom stats predict rnorm runif sd na.omit
#' @importFrom future nbrOfWorkers plan
#' @importFrom stats as.formula
"_PACKAGE"
# # dummy_import = function() {
# #   # nocov start
# #   # this function is required to silence R CMD check
# #   mlbench::mlbench.xor
# #   mlr3measures::mse
# # } # nocov end
#
#
# .onLoad = function(libname, pkgname) {
#   # nocov start
#   backports::import(pkgname)
#
#   # setup logger
#   lg = lgr::get_logger(pkgname)
#   assign("lg", lg, envir = parent.env(environment()))
#   f = function(event) {
#     event$msg = paste0("[mlr3] ", event$msg)
#     TRUE
#   }
#   # lg$set_filters(list(f))
#   # if (Sys.getenv("IN_PKGDOWN") == "true") {
#   #   lg$set_threshold("warn")
#   # }
#
# } # nocov end
#
# mlr3misc::leanify_package()
