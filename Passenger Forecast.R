# Wipe the environment clean
rm(list = ls())
# Clean the console
cat("\f") 

# Prepare needed libraries
packages <- c("tidyverse",
              "tsibble",
              "dplyr",
              "feasts",
              "lubridate",
              "fpp3",
              "tidyr",
              "ggplot2",
              "tibbletime",
              "stargazer",
              "pdftools"
              )

for (i in 1:length(packages)) {
  if (!packages[i] %in% rownames(installed.packages())) {
    install.packages(packages[i], dependencies = TRUE)
  }
  library(packages[i], character.only = TRUE)
}

#set working directory
setwd("/Users/Ross Walendziak/Documents/Predictive Analytics/Final Project Raw Data")
fileLocation = "/Users/Ross Walendziak/Documents/Predictive Analytics/Final Project Raw Data"

#load passenger data
pdfs <- list.files(fileLocation, pattern = "pdf$")
reports <- as.character(lapply(pdfs, pdf_text))

get_data <- function(dat) {
            list(Date = str_split(str_trim(str_extract(dat, "(?<=Monthly Airport Traffic Summary - )(.*?)(?=\n)")),
                                 pattern =  " "),
                 passengers = str_trim(str_match(dat, "Total Airport Passengers\\s*(.*?)\\s* "))
                )
          }

massport <- data.frame(matrix(data = NA, 
                              nrow = length(reports), 
                              ncol = 2, 
                              dimnames = list(c(), c("Date", "passengers"))
                              )
                       )

for (i in 1:length(reports)) {
  
  info = get_data(reports[i])
  
  massport[i,1] = gsub("\\\\n", "", paste(info$Date[[1]][2], info$Date[[1]][1]))
  massport[i,2] = as.numeric(gsub("[,]", "", info$passengers[2]))
  
}


#convert massport data into a tsibble object
massport <- massport %>%
  mutate(Month = yearmonth(Date)) %>%
  select(-Date) %>%
  as_tsibble(index = Month) %>%
  mutate(passengers = passengers/1000)


massport <- tsibble::fill_gaps(massport) #missing observation for April 2002
massport[40,]$passengers = (massport[39,]$passengers + massport[41,]$passengers) / 2


#reorder columns to be in the order of month first, passengers second
massport = massport[, c(2, 1)]

#Save Copy of Completed Time Series
setwd("/Users/Ross Walendziak/Documents/Predictive Analytics")
write.csv(massport, file = 'Complete Time Series.csv')


#Simple autoplot of the massport timeseries
#clear seasonal pattern with upward trend. notable outlyers for Sept 2001 terror attack and March 2020 C19
autoplot(massport, passengers) +
  labs(y = "Monthly Passengers (in thousands)",
       title = "Passenger Volume at Logan Airport")


#######Begin Feature Summary of the time series here#########
#############################################################
#############################################################

#Seasonal plot
massport %>%
  gg_season(passengers, labels = "both") +
  labs(y = "Monthly Passengers (in thousands)",
       title = "Seasonal plot: Passenger Volume at Logan Airport")

#Seasonal subseries
massport %>%
  gg_subseries(passengers) +
  labs(
    y = "Monthly Passengers (in thousands)",
    title = "Passenger Volume at Logan Airport"
  )

#Lag plots
massport %>%
  gg_lag(passengers, geom = "point", lags = 1:12) +
  labs(x = "lag(passengers, k)",
       title = "Passengers vs the Kth Lag of Passengers")

#Autocorrelation Plot
massport %>%
  ACF(passengers) %>%
  autoplot() + labs(title="Passenger Volume at Logan Airport")

#Histogram of monthly passenger observations
hist(massport$passengers,
     breaks = 20,
     col = "blue",
     main = "Monthly Passenger Volume at Logan Airport: Jan 1999 - Sept 2021",
     xlab = "Passenger Volume (1,000's of passengers)"
     )


#Basic Summary stats for montly passenger data (mean, median, quartile, min, max)
#this will satisfy the "basic" features requirement.
write.csv(t(as.data.frame(apply(massport[,2], 2, summary)))
          , file = 'Descriptive Stats.csv')

#its clear from the autoplot that the magnitude of the seasonal component increases,
#on average throught time.  Therefore, we have non-constant variance.
#Below we use a box-cox transformation using an optimal lambda value
#to achieve a more constant variance of the seasonal component of the the time series.

massport %>%
  autoplot(box_cox(passengers, .12)) +
  labs(y = "",
       title = paste(
         "Transformed Logan Airport Passenger Volume with lambda = ",
         round(0.12, 2)))

#Use STL decomposition (Seasonal and Trend Decomposition using Losses)
#trend window is the number of consecutive observations to be used when estimating the trend-cycle
#season window is the number of consecutive years to be used in estimating each value in the seasonal component
 #season window = "periodic" means consider all years for seasonality component

#This one is prob the best STL decomposition
massport %>%
  model(STL(box_cox(passengers, 0.12) ~ trend(window = 60) +
            season(window = "periodic"),
            robust = TRUE)) %>% #robust to outiers so that unusual obs do not effect trend-cycle and seasonality estimates
  components() %>% #Note, robust removes seasonally of apr to may since sometimes up, sometimes down
  autoplot()

#ACF features
massport %>% features(passengers, feat_acf)

#Complex features
massport_features <- massport %>%
  features(passengers, feature_set(pkgs = "feasts"))

massport_features


######### Model Building ##############
#######################################
#######################################

#Build a new column in massport for "crisis" dummy variable (Sep 2001 and C19)
massport$Crisis_dummy = 0

massport$Crisis_dummy[massport$Month %in% yearmonth(c("2001 Sep", "2001 Oct", "2001 Nov",
                                                "2001 Dec", "2002 Jan")
                                              )] = 1

massport$Crisis_dummy[massport$Month %in% yearmonth(c("2020 Mar", "2020 Apr", "2020 May",
                                                      "2020 Jun", "2020 Jul", "2020 Aug",
                                                      "2020 Sep", "2020 Oct", "2020 Nov",
                                                      "2020 Dec", "2021 Jan", "2021 Feb",
                                                      "2021 Mar", "2021 Apr", "2021 May",
                                                      "2021 Jun", "2021 Jul", "2021 Aug",
                                                      "2021 Sep", "2021 Oct")
                                                    )] = 1

#Build monthly seasonal dummy variables
massport$Jan_dummy = append(rep(c(1,0,0,0,0,0,0,0,0,0,0,0), 22), c(1,0,0,0,0,0,0,0,0,0))
massport$Feb_dummy = append(rep(c(0,1,0,0,0,0,0,0,0,0,0,0), 22), c(0,1,0,0,0,0,0,0,0,0))
massport$Mar_dummy = append(rep(c(0,0,1,0,0,0,0,0,0,0,0,0), 22), c(0,0,1,0,0,0,0,0,0,0))
massport$Apr_dummy = append(rep(c(0,0,0,1,0,0,0,0,0,0,0,0), 22), c(0,0,0,1,0,0,0,0,0,0))
massport$May_dummy = append(rep(c(0,0,0,0,1,0,0,0,0,0,0,0), 22), c(0,0,0,0,1,0,0,0,0,0))
massport$Jun_dummy = append(rep(c(0,0,0,0,0,1,0,0,0,0,0,0), 22), c(0,0,0,0,0,1,0,0,0,0))
massport$Jul_dummy = append(rep(c(0,0,0,0,0,0,1,0,0,0,0,0), 22), c(0,0,0,0,0,0,1,0,0,0))
massport$Aug_dummy = append(rep(c(0,0,0,0,0,0,0,1,0,0,0,0), 22), c(0,0,0,0,0,0,0,1,0,0))
massport$Sep_dummy = append(rep(c(0,0,0,0,0,0,0,0,1,0,0,0), 22), c(0,0,0,0,0,0,0,0,1,0))
massport$Oct_dummy = append(rep(c(0,0,0,0,0,0,0,0,0,1,0,0), 22), c(0,0,0,0,0,0,0,0,0,1))
massport$Nov_dummy = append(rep(c(0,0,0,0,0,0,0,0,0,0,1,0), 22), c(0,0,0,0,0,0,0,0,0,0))


#Define the Training and Testing Data Sets
  #The training data is assigned to allow for 2 years of testing assessment


train = massport %>% filter_index("1999 Jan" ~ "2019 Oct")
test = massport %>% filter_index("2019 Nov" ~ "2021 Oct")

#build models
linear_trend = TSLM(box_cox(passengers, 0.12) ~ trend()) #simple linear trend model
mean = MEAN(box_cox(passengers, 0.12)) #forecast is the mean obs of the training set
naïve = NAIVE(box_cox(passengers, 0.12)) #Naive Model (aka last obs model)
seasonal_naïve = SNAIVE(box_cox(passengers, 0.12) ~ lag('year')) #Seasonal Naive Model (Last Obs + seasonal pattern)

decomposition = decomposition_model(STL(box_cox(passengers, 0.12) ~ trend(window = 60), 
                                            robust = TRUE),
                           NAIVE(season_adjust)
                           )
piecewise_crisis_seasonDummy = TSLM(passengers ~ trend(knots = yearmonth(c("2001 Sep", "2002 Jan"))) +
                                                  Crisis_dummy +
                                                  Jan_dummy + Feb_dummy + Mar_dummy +
                                                  Apr_dummy + May_dummy + Jun_dummy +
                                                  Jul_dummy + Aug_dummy + Sep_dummy +
                                                  Oct_dummy + Nov_dummy)
harmonic_K6 = TSLM(box_cox(passengers, 0.12) ~ trend() + fourier(K = 6))
ETS = ETS(box_cox(passengers, 0.12))
ARIMA = ARIMA(box_cox(passengers, 0.12))
piecewise_LM_ARIMA_errors = ARIMA(box_cox(passengers, 0.12) ~ 
                                    trend(knots = yearmonth(c("2001 Sep", "2002 Jan"))
                                          )
                                  )

#Examine Basic Models (linear trend, mean, naive and seasonal naive models)
passengers_fit <- train %>%
  model(
    linear_trend,
    mean,
    naïve,
    seasonal_naïve
    )

passengers_fc = passengers_fit %>% forecast(h = "2 years")

#Basic Models - Plot of fitted vs Actual Values
ggplot(fitted.values(passengers_fit), aes(x = Month
                                          , y = .fitted
                                          , color = .model)
       ) +
  geom_line(aes(group = .model), size = 1) + 
  autolayer(train, passengers, colour = "black") +
  labs(title = "Fitted Values of Passenger Volume vs Historical Data"
       , x = "Month"
       , y = "Passenger Volume (1,000's)"
      )

#Basic Models - Plot of 2-year forecasted values vs Covid Outcomes
passengers_fc %>%
  autoplot(train %>% filter_index("2010 Jan" ~ .), level = NULL) +
  autolayer(
    test,
    colour = "black"
  ) +
  labs(
    y = "Passenger Volume (1,000's)",
    title = "Forecasts for monthly Passenger Volume at Logan Airport"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

(passengers_fit %>% select(mean)) %>% gg_tsresiduals() +
  labs(title = "Residuals from the Mean Model")

#Time series decomposition model: Naive seasonality (repeat prior year seasonality
#plus the trend component establish by the last 24 month observations.

fit_dcmp <- train %>%
  model(decomposition)

fit_dcmp %>%
  forecast(h ="2 years", colour = "blue") %>%
  autoplot(train %>% filter_index("2013 Jan" ~ .))+
  autolayer(test,
            colour = "black")+
  labs(y = "Passenger Volume (1,000's)",
       title = "Decomposition Forecasts for monthly Passenger Volume at Logan Airport")

fit_dcmp %>% gg_tsresiduals() +
  labs(title = "Residuals from the Decomposition Model")


#Piecewise with Dummy Seasonal and Crisis variables
fit_piecewise_crisis_seasonDummy <- train %>%
  model(piecewise_crisis_seasonDummy)

test_future <- new_data(train, 24) %>%
  mutate(Crisis_dummy = c(0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)) %>%
  mutate(Jan_dummy = rep(c(0,0,1,0,0,0,0,0,0,0,0,0), 2)) %>%
  mutate(Feb_dummy = rep(c(0,0,0,1,0,0,0,0,0,0,0,0), 2)) %>%
  mutate(Mar_dummy = rep(c(0,0,0,0,1,0,0,0,0,0,0,0), 2)) %>%
  mutate(Apr_dummy = rep(c(0,0,0,0,0,1,0,0,0,0,0,0), 2)) %>%
  mutate(May_dummy = rep(c(0,0,0,0,0,0,1,0,0,0,0,0), 2)) %>%
  mutate(Jun_dummy = rep(c(0,0,0,0,0,0,0,1,0,0,0,0), 2)) %>%
  mutate(Jul_dummy = rep(c(0,0,0,0,0,0,0,0,1,0,0,0), 2)) %>%
  mutate(Aug_dummy = rep(c(0,0,0,0,0,0,0,0,0,1,0,0), 2)) %>%
  mutate(Sep_dummy = rep(c(0,0,0,0,0,0,0,0,0,0,1,0), 2)) %>%
  mutate(Oct_dummy = rep(c(0,0,0,0,0,0,0,0,0,0,0,1), 2)) %>%
  mutate(Nov_dummy = rep(c(1,0,0,0,0,0,0,0,0,0,0,0), 2))

piecewise_fc = forecast(fit_piecewise_crisis_seasonDummy, 
                  new_data = test_future)

autoplot(piecewise_fc) +
autolayer(train %>% filter_index("2013 Jan" ~ .)) +
autolayer(test, colour = "black") +
labs(y = "Passenger Volume (1,000's)",
     title = "Piecewise Forecasts for monthly Passenger Volume at Logan Airport")

fit_piecewise_crisis_seasonDummy %>% gg_tsresiduals() +
  labs(title = "Residuals from the Piecewise Model")


#Harmonic Model with Six Fourier Terms for seasonality

fit_harmonic_k6 <- train %>%
  model(harmonic_K6)

autoplot(massport, passengers, color = "grey") +
  autolayer(fitted.values(fit_harmonic_k6), .fitted, colour = "blue") +
  labs(title = "Passenger Volume data plotted with Harmonic Fourier K = 6 model")

fit_harmonic_k6 %>%
  forecast(h ="2 years", colour = "blue") %>%
  autoplot(train %>% filter_index("2013 Jan" ~ .))+
  autolayer(test,
            colour = "black")+
  labs(y = "Passenger Volume (1,000's)",
       title = "Harmonic K = 6 Forecasts for monthly Passenger Volume at Logan Airport")

fit_harmonic_k6 %>% gg_tsresiduals() +
  labs(title = "Residuals from the Harmonic Model with K = 6")


#ETS model ETS(passengers ~ error('M') + trend('N') + season('M'))

fit_ETS <- train %>%
  model(ETS)

fit_ETS %>%
  forecast(h ="2 years", colour = "blue") %>%
  autoplot(train %>% filter_index("2013 Jan" ~ .))+
  autolayer(test,
            colour = "black")+
  labs(y = "Passenger Volume (1,000's)",
       title = "ETS Forecasts for monthly Passenger Volume at Logan Airport")

fit_ETS %>% gg_tsresiduals() +
  labs(title = "Residuals from the ETS(A,N,A) Model")


#ARIMA Model - Best fit is: ARIMA(0,1,1)(0,1,1)[12]
#Notes to self:
#ARIMA(p,d,q)
#p = order of the autoregressive parts (number of lags of y)
#d = degree of differencing involved (yi - yi-1)
#q = order of the moving average part (lags of the error terms)

fit_ARIMA <- train %>%
  model(ARIMA)

fit_ARIMA %>%
  forecast(h ="2 years", colour = "blue") %>%
  autoplot(train %>% filter_index("2013 Jan" ~ .))+
  autolayer(test,
            colour = "black")+
  labs(y = "Passenger Volume (1,000's)",
       title = "ARIMA Forecasts for monthly Passenger Volume at Logan Airport")

fit_ARIMA %>% gg_tsresiduals() +
  labs(title = "Residuals from the ARIMA(0,1,1)(0,1,1) Model")

#Piecewise linear model (knots @ 9/11 Terror attacks and C19 Turning Points) with ARIMA errors
#LM w/ ARIMA(3,0,0)(1,0,0)[12] errors

fit_piecewise_LM_ARIMA_errors <- train %>%
  model(piecewise_LM_ARIMA_errors)

fit_piecewise_LM_ARIMA_errors %>%
  forecast(h ="2 years", colour = "blue") %>%
  autoplot(train %>% filter_index("2013 Jan" ~ .))+
  autolayer(test,
            colour = "black")+
  labs(y = "Passenger Volume (1,000's)",
       title = "Piecewise LM with ARIMA errors - Forecasts for monthly Passenger Volume at Logan Airport")

fit_piecewise_LM_ARIMA_errors %>% gg_tsresiduals() +
  labs(title = "Residuals from the Piecewise LM with ARIMA(3,0,0)(1,0,0)[12] Errors")


#Aggregate the advanced fit models for Collective Plots of Fitted and Forecasted Values

fit_advanced <- train %>%
  model(
    decomposition,
    harmonic_K6,
    ETS,
    ARIMA,
    piecewise_LM_ARIMA_errors
  )


advanced_fc = fit_advanced %>% forecast(h = "2 years")


#Advanced Models - Plot of fitted vs Actual Values
ggplot(fitted.values(fit_advanced), aes(x = Month
                                          , y = .fitted
                                          , color = .model)
) +
  geom_line(aes(group = .model), size = 1) + 
  autolayer(train, passengers, colour = "black") +
  labs(title = "Fitted Values of Passenger Volume vs Historical Data"
       , x = "Month"
       , y = "Passenger Volume (1,000's)"
  )

#Advanced Models - Plot of forecasted vs actual test values (C19)
advanced_fc %>%
  autoplot(train %>% filter_index("2017 Jan" ~ .), level = NULL) +
  autolayer(
    test,
    colour = "black"
  ) +
  labs(
    y = "Passenger Volume (1,000's)",
    title = "Forecasts for monthly Passenger Volume at Logan Airport"
  ) +
  guides(colour = guide_legend(title = "Forecast"))


##### Assess the Accuracy of models #####
#########################################
########################################

#basic model accuracy using RMSE on single TEST set

#Training Set Accuracy (much smaller than the testing of cross validation sets)
train_accuracy = train %>%
                  model(
                    linear_trend,
                    mean,
                    naïve,
                    seasonal_naïve,
                    decomposition,
                    piecewise_crisis_seasonDummy,
                    harmonic_K6,
                    ETS,
                    ARIMA,
                    piecewise_LM_ARIMA_errors) %>%
                  accuracy()

train_RMSE = train_accuracy[, c('.model', 'RMSE')]
train_RMSE = train_RMSE[order(train_RMSE$.model), ]
train_RMSE

#Single Testing Set Cross validation

test_accuracy = passengers_fit <- train %>%
                    model(
                      linear_trend,
                      mean,
                      naïve,
                      seasonal_naïve,
                      decomposition,
                      harmonic_K6,
                      ETS,
                      ARIMA,
                      piecewise_LM_ARIMA_errors
                    )

passengers_fc <- passengers_fit %>%
  forecast(h = "2 years")


test_accuracy = accuracy(passengers_fc, test)
test_accuracy_piecewise = accuracy(piecewise_fc, test)

test_accuracy = rbind(test_accuracy, test_accuracy_piecewise)

test_RMSE = test_accuracy[, c('.model', 'RMSE')]
test_RMSE = test_RMSE[order(test_RMSE$.model), ]
test_RMSE


#Time series Cross validation 

passengers_tscv <- massport %>%
  stretch_tsibble(.init = 36, .step = 3) %>%
  relocate(Month, passengers, .id)

#Note: The CV process below takes about 15 mins to run given 
# the 9 models and 80 iterations in the CV approach.
cv_accuracy = passengers_tscv %>%
              model(
                linear_trend,
                mean,
                naïve,
                seasonal_naïve,
                decomposition,
                harmonic_K6,
                ETS,
                ARIMA,
                piecewise_LM_ARIMA_errors
                ) %>%
              forecast(h = "2 years") %>%
              accuracy(massport)

cv_accuracy

cv_RMSE = cv_accuracy[, c('.model', 'RMSE')]# %>% sort(.model)
cv_RMSE = cv_RMSE[order(cv_RMSE$.model), ]

final_accuracy <- merge(train_RMSE,
                        test_RMSE,
                        by = ".model",
                        all.x = TRUE, all.y = FALSE # LEFT join
                        )

final_accuracy <- merge(final_accuracy,
                        cv_RMSE,
                        by = ".model",
                        all.x = TRUE, all.y = FALSE #LEFT join
                        )

colnames(final_accuracy) = c("Model", "Train_RMSE", "Test_RMSE", "CV_RMSE")
final_accuracy

# Final model

ETS_final = ETS(box_cox(passengers, 0.12) ~ error("A") + trend("N") + season("A"))

fit_ETS_final <- massport %>%
  model(ETS_final)

fit_ETS_final %>%
  forecast(h ="2 years", colour = "blue") %>%
  autoplot(massport %>% filter_index("2015 Jan" ~ .))+
  labs(y = "Passenger Volume (1,000's)",
       title = "ETS(A,N,A) Forecast for monthly Passenger Volume at Logan Airport")

fit_ETS_final %>% gg_tsresiduals() +
  labs(title = "Residuals from the ETS(A,N,A) Model")
