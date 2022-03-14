# Passenger Volume Forecast for Logan Airport | Project Overview:
* Created a tool that scapes PDF documents from Massport for monthly passenger volumes at Boston Logan Airport using the pdftools package in r for the Jan 1999 to Oct 2021 period.
* Raw data is cleaned and assembed into a time series tsibble object using the various tidyverse packages.
* Exploratory data analysis is preformed to reveal insights on long term passenger terms, monthly seasonality and the relationship between lags (autocorrelations) of successive passenger volume observations.
* Mean, naive, seasonal naive, linear trend, decomposition, harmonic, exponential smoothing, ARIMA, and a piecewise linear model with ARIMA errors are fit to the historial data.
* Best model selection is completed using a test set and cross validation approach, as assessed by RMSE.
* A final 2-year passenger volume forecast is offered for the Nov 2021 through Oct 2023 period using all available data on the best model selection.

# Code and Resources Used:
* R Version: 4.1.2
* Packages: tidyverse, tsibble, dplyr, feasts, lubridate, fpp3, tidyr, gplot2, tibbletime, stargazer, pdftools
* PDF Scrapper Article: https://crimebythenumbers.com/scrape-table.html
* Text: https://otexts.com/fpp3/

# Data Source:
https://www.massport.com/logan-airport/about-logan/airport-statistics/

# Data Cleaning:
* Parsed monthly passenger volume from numeric data in PDF
* Parsed date out of PDF sub-title information
* Cleaned extraneous spaces and punctuation from parsed passenger volume and associated dates
* Combined passenger volumne and associated date stamp into tsibble object for later time series analysis

# Exploratory Data Analysis:
The time series generally shows an upward tread with strong seasonality, with the larger trend disrupted by the C19 global pandemic.

![](https://github.com/ross-walendziak/Passenger-Forecast/blob/main/graphics/Raw%20Time%20Series%20Plot.png)

On average, August represents the strongest seasonal travel period at Boston Logan Airport, February the weakest.

![](https://github.com/ross-walendziak/Passenger-Forecast/blob/main/graphics/Seasonal%20Plot.png) 
![](https://github.com/ross-walendziak/Passenger-Forecast/blob/main/graphics/Seasonal%20Subseries.png)

The 1st and 12th lags of the data show strong autocorrelation - representing the upward trend and seasonal components, respectively.

![](https://github.com/ross-walendziak/Passenger-Forecast/blob/main/graphics/Autocorrelation%20Plot.png)
