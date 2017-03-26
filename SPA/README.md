# Financial Econometrics Project - Hansen's test for Superior Predictive Ability (SPA)

Computing project by Daniel Bestard, Michael Cameron and Akhil Lohia for the course 12F005 at Barcelona GSE.

This project implements Hansen's test for Superior Predictive Ability (SPA) of models in `R`.

This test is an alternative to the reality check (RC) for data snooping, it is more powerful and less sensitive to poor and irrelevant alternatives. The improvements are achieved by two modifications of the RC: A studentized test statistic and a sample-dependent null distribution. The advantages of the new test are confirmed by an empirical exercise in which we compare a few regression-based forecasts of US GDP growth to a simple random-walk forecast. The random-walk forecast is found to be inferior to regression-based forecasts.
When testing for SPA, the question of interest is whether any alternative forecast is better than the benchmark forecast. Hence, the null hypothesis is that “the benchmark is not inferior to any alternative forecast.” This testing problem is relevant for applied econometrics, because several ideas and specifications are often used before a model is selected.

The function `spa` takes the following parameters as input:
- `bench` : Vector of losses using the benchmark model
- `models` : Matrix of losses using the alternative models
- `B` : Number of bootstrap samples desired
- `w` : Size of the block for block bootstrap or average size of the blocks for stationary bootstrap
- `type`: Type of test statistic. STUDENTIZED or STANDARD. The default option is STUDENTIZED
- `boot` : The desired bootstrap method - STATIONARY or BLOCK. The default option is STATIONARY

The output is a list with the following elements:
- C : Hansen’s consistent p-val, which adjusts the Reality Check p-val in the case of high variance but low mean models
- U : Upper P-val(White) (Original RC P-vals)
- L : Lower P-val(Hansen)

Inline comments in the code of the function provide necessary steps for the test.
The functions `block_bootstrap` and `stat_bootstrap` perform block and stationary bootstrap respectively for a given vector of observations and desired block length.

### Demo:

The demo of the function is provided in the file `demo.R`. Here we fit different models on the US GDP growth time series. The benchmark model is a random walk forecast and the set of alternative models includes AR(1), MA(1), MA(2), and ARMA(1,1). After running the SPA test, we get a p-value < 0.05 which indicates that the benchmark model is not superior to the set of alternative models for forecasting which is as expected.

### Reference:
MFE Toolbox by Kevin Sheppard
