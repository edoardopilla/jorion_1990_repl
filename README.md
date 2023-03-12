# jorion_1990_repl

**_Replicating results from the paper by Philippe Jorion, published in 1990 in the Journal of Banking and Finance._**

In his paper, Jorion tracks the performance of different estimators for future returns, along with the out of sample performance of tangency portfolios constructed based on such estimators. He concludes that CAPM based estimators provide the best out of sample performance by means of mean squared error and Sharpe ratio metrics.

This replication attempt uses monthly data from the S&P500 market index, along with monthly data about Tbills yields, to construct sample average and CAPM based estimators, and to then build tangency portfolios, finally highlighting how their weights evolve in time when using different types of estimators to forecast the following monthly return, on a rolling window of 60 months, consistently with Jorion.

Conclusions are mixed, since the lowest MSE is obtained with the capweighted S&P500 returns, whereas the largest Sharpe ratio is obtained with the naive sample average estimator.
