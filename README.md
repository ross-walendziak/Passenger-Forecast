# Passenger Volume Forecast for Logan Airport | Project Overview:
* Created a tool that scapes PDF documents from Massport for monthly passenger volumes at Boston Logan Airport using the pdftools package in r for the Jan 1999 to Oct 2021 period.
* Raw data is cleaned and assembed into a time series tsibble object using the various tidyverse packages.
* Exploratory data analysis is preformed to reveal insights on long term passenger terms, monthly seasonality patterns and the relationship between lags (autocorrelations) of successive passenger volume observations.
* Mean, naive, seasonal naive, linear trend, decomposition, harmonic, exponential smoothing, ARIMA, and a piecewise linear model with ARIMA errors are fit to the historial data.
* Best model selection is completed using a testing set and cross validation approach, as assessed by RMSE.
* A final 2-year passenger volume forecast is offered for the Nov 2021 through Oct 2023 period using all available data on the best model selection.

# Code and Resources Used:
* R Version: 4.1.2
* Packages: tidyverse, tsibble, dplyr, feasts, lubridate, fpp3, tidyr, gplot2, tibbletime, stargazer, pdftools
* PDF Scrapper Article: https://crimebythenumbers.com/scrape-table.html
* Text: https://otexts.com/fpp3/

# Data Source:
https://www.massport.com/logan-airport/about-logan/airport-statistics/

# Data Cleaning:
* Parsed monthly passenger volume from numeric data in PDF.
* Parsed date out of PDF sub-title information.
* Cleaned extraneous spaces and punctuation from parsed passenger volume figures and associated dates.
* Combined passenger volumes and associated date stamps into a tsibble object for later time series analysis.

# Exploratory Data Analysis:
The time series generally shows an upward tread with strong seasonality, with the larger trend disrupted by the C19 global pandemic.

![](https://github.com/ross-walendziak/Passenger-Forecast/blob/main/graphics/Raw%20Time%20Series%20Plot.png)

On average, August represents the strongest seasonal travel period at Boston Logan Airport, February the weakest.

![](https://github.com/ross-walendziak/Passenger-Forecast/blob/main/graphics/Seasonal%20Plot.png) 
![](https://github.com/ross-walendziak/Passenger-Forecast/blob/main/graphics/Seasonal%20Subseries.png)

The 1st and 12th lags of the data show strong autocorrelation - representing the upward long-term trend and monthly seasonal components, respectively.

![](https://github.com/ross-walendziak/Passenger-Forecast/blob/main/graphics/Autocorrelation%20Plot.png)

# Model Building:

* Mean, naive, linear trend - base case models without seasonality considerations
* Seasonal naive - Base case model with seasonality
* Seasonal Decomposition, harmonic K6, exponential smoothing and ARIMA - Candidates for best fit inclusive of both trend and seasonality components. 
  * Six fourier terms best fit the historical data in the harmonic model.
  * The exponential smoothing model (ETS) was best fit using ETS(passengers ~ error('M') + trend('N') + season('M')).  
  * The ARIMA model was optimized using an ARIMA(0,1,1)(0,1,1)[12].
* Piecewise linear model with ARIMA errors - Attempt to estimate seperate coefficients for the pre and post crisis periods reflected in the Sept 2001 Terrror Attacks     and the C19 global pandemic sub-periods.

# Model Performance:

* Models were evaluated using root mean squared error (RMSE)
* Cross validation (CV) was performed starting with an initial training bank of 36 (3-year) observations, producing a 2-year forecast, and evaluating RMSE.
  * Subsequent iterations in the CV approach added 3-months of data and again produced a 2-year forecast.  RMSE was evaulationed. Total RMSE was averaged across all 2-     year forecasts for each model considered.
* As expected, the testing set and CV aproaches produced a larger RMSE than than training set. 
* The decompostion model produced the lowest RMSE in the CV approach.  However, it performed poorly during the C19 sub-period, revealing fragility in estimating         crisis periods.
* The exponential smoothing ETS(A,N,A) model performed similarly to the decomposition model - with more robustness during crisis periods.
* Best model selection was achieved with ETS(A,N,A).

![](https://github.com/ross-walendziak/Passenger-Forecast/blob/main/graphics/Model%20Performance.png)

# 2-year Forecast:
![](https://github.com/ross-walendziak/Passenger-Forecast/blob/main/graphics/Final%20ETS%20forecast.png)
