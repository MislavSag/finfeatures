#' Communicate to R the Python virtual environment containing the relevant libraries for calculating features
#'
#' @importFrom reticulate use_virtualenv
#' @param python_path \code{string} specifying the filepath to the version of Python you wish to use
#' @param python_path \code{string} specifying the filepath to the Python virtual environment where "tsfresh", "tsfel", and/or "kats" are installed
#' @return no return value; called for side effects
#' @author Mislav Sagovac
#' @export
#'
init_finfeatures <- function(python_path = "C:/Users/Mislav/.conda/envs/mlfinlabenv/python.exe"){
  stopifnot(is.character(python_path))
  reticulate::use_python(python_path, required = TRUE)
}


# install packages
# conda install -c conda-forge dcor
# pip install numpy_ext

# install talib package
# conda install -c conda-forge ta-lib
# follow installation instructions from the talib here https://github.com/TA-Lib/ta-lib-python



### TO SLOW, MAYBE I CAN CALCULATE FOR DAILY DATA AND SAVE ON NAS
# library(data.table)S
# library(reticulate)
# library(findata)
# init_finfeatures()
#
# # reticulate::source_python(system.file("python", "alpha01_main.py", package = "finfeatures")) # Ships with package
# reticulate::source_python("C:/Users/Mislav/Documents/GitHub/finfeatures/inst/python/alphas101.py") # Ships with package
# # reticulate::source_python(system.file("python", "alphas101.py", package = "finfeatures")) # Ships with package
#
# py <- import_builtins()
#
#
# # test function
# data("stocks")
# head(stocks)
# colnames(stocks)[1:2] = c("ticker", "date")
# head(stocks)
#
# df = reticulate::r_to_py(stocks[1:5000,])
# df = df$set_index(c("ticker", "date"))
# system.time({alpha101_df = alpha01_main(df)})
#
#
# DT = as.data.table(alpha101_df)
# table(DT$alpha001)
# table(DT$alpha002)
# table(DT$alpha003)
#
# 140
