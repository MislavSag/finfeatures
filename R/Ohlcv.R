#' @title Ohlcv Class
#'
#' @description
#' This function helps preparing data for feature calculation.
#'
#' @export
Ohlcv = R6::R6Class(
  "Ohlcv",

  public = list(

    #' @field X Input data.
    X = NULL,

    #' @field id_col I column.
    id_col = NULL,

    #' @field date_col Date columnh
    date_col = NULL,

    #' @field price Price column
    price = NULL,

    #' @field ohlcv Ohlcv column
    ohlcv = NULL,

    #' @description
    #' Create a new Ohlcv object.
    #'
    #' @param X Input data.
    #' @param id_col Id column.
    #' @param date_col Date column.
    #' @param price Price column
    #' @param ohlcv (optional) Ohlcv columns
    #' @return A new `Person` object.
    initialize = function(X, id_col = "symbol", date_col = "date", price = "close",
                          ohlcv = c("open", "high", "low", "close", "volume")) {

      # checks
      self$X = assert_data_frame(X)
      self$id_col = assert_string(id_col, min.chars = 1L)
      self$date_col = assert_string(date_col, min.chars = 1L)
      self$price = assert_string(price, min.chars = 1L)
      if (!is.null(ohlcv)) {
        self$ohlcv = assert_character(ohlcv, min.chars = 1L, min.len = 5L)
      }

      # convert to data.table
      X_ = as.data.table(self$X)
      self$X = copy(X_)

      # create  returns columns
      self$X[, returns := get(price) / shift(get(price)) - 1 ]

      # change columns names
      if (is.null(ohlcv)) {
        setnames(self$X, c(id_col, date_col), c("symbol", "date"))
      } else {
        setnames(self$X, c(id_col, date_col, ohlcv), c("symbol", "date", "open", "high", "low", "close", "volume"))
      }

      # check price
      if (self$price == "close") {
        assert_double(self$X[, get(price)], lower = 1e-005)
      }
    }
  )
)
