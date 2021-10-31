# finfeatures

There are lots of packages in R and other progamming languages for machine learning and econometrics modeling. 
It seems to me that most popular ML methods works pretty well on well defined problems.
For example in R you can use tidymodels or mlr3 to construct AutoML pipeline very fast.
This can be used than on financial time series to make predictions of financial outcomes.
But what is ussually missing is easy way to construct financial features for the prediction.
Raw prices (OHLCV) are not enough to achieve good accuracy. We have to make some kind of transformation of OHLCV or use some alternative data which again can be transformed in multiple ways.

This main goal of this package is to make the process of generating features as easy as possible.

I follow very simple procedure:

1. Found a package in R that already imlements feature development.
2. Calculate features in riling window with different windows and lags.
3. Derive various moments and transformatins from features.
4. Combine all features together.

Issues rports and contributions are more than welcomed.
