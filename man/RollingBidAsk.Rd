% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RollingBidAsk.R
\name{RollingBidAsk}
\alias{RollingBidAsk}
\title{RollingBidAsk Class}
\arguments{
\item{windows}{Vector of windows that will be applied on features.}

\item{workers}{Number of workers. Greater than 1 for parallle processing}

\item{lag}{Lag variable in runner package.}

\item{at}{Argument at in runner package.}

\item{methods}{Argument methods in Bidask package.}

\item{x}{Ohlcv object.}

\item{window}{Rolling window lengths.}

\item{price_col}{Price column in Ohlcv}

\item{params}{Vector of parameters}
}
\value{
A new `RollingBidAsk` object.

Calculate rolling radf features from exuber package.
}
\description{
Function calculates spread from exuber bidask package.

Create a new RollingBidAsk object.

Function calculates radf values from exuber package on rolling window.
}
\section{Fields}{

\describe{
\item{\code{methods}}{Bidask methods.}
}}

\examples{
data(spy_hour)
OhlcvInstance = Ohlcv$new(spy_hour, date_col = "datetime")
RollingBidAskInstance <- RollingBidAsk$new(windows = 200,
                                           workers = 2L,
                                           at = c(300, 500),
                                           lag = 1L,
                                           method = "EDGE")
x = RollingBidAskInstance$get_rolling_features(OhlcvInstance)
head(x)
# multiple windows and parallel and vector arguments
RollingBidAskInstance <- RollingBidAsk$new(windows = 200,
                                           workers = 2L,
                                           at = c(300, 500),
                                           lag = 1L,
                                           method = c("EDGE", "Roll"))
x = RollingBidAskInstance$get_rolling_features(OhlcvInstance)
head(x)
}
