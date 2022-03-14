# Passenger-Forecast

# Project Overview:
* Created a tool that scapes PDF documents from massport.com for monthly passenger volumes at Boston Logan Airport using the pdftools package in r for the Jan 1999 to Oct 2021 period.
* Raw data is cleaned and assembed into a time series tsibble object using the various tidyverse packages.
* Exploratory data analysis is preformed to reveal insights on long term passenger terms, monthly seasonality and the relationship between lags (autocorrelations) of successive passenger volume observations.
* Mean, naive, seasonal naive, linear trend, decomposition, harmonic, exponential smoothing, ARIMA, and a piecewise linear model with ARIMA errors are fit to the historial data.
* Best model selection is completed using a test set and cross validation approach, as assessed by RMSE.
* A final 2-year passenger volume forecast is offered for the Nov 2021 through Oct 2023 period using all available data on the best model selection.
