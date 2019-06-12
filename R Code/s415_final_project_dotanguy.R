####################################################
##             Guy Dotan - 403-882-450            ##
##             Stats 415 - Final Project          ##
##                    12/01/2018                  ##
####################################################


## Set working directory folder
setwd("~/Documents/UCLA MAS/2018 Fall/STATS 415/Final Project")

library(ggplot2)
library(dplyr)
library(scales)
library(astsa)

# read in files
hist <- read.csv(file = "Data/historical_new_melones.csv", stringsAsFactors = F)
pol <- read.csv(file = "Data/policy_new_melones.csv", stringsAsFactors = F)

# format DATE as date
hist$DATE <- as.Date(hist$DATE, format = "%Y-%m-%d")
pol$DATE <- as.Date(pol$DATE, format = "%Y-%m-%d")

# merge sets
dat <- cbind(hist, pol)
dat <- dat[,c(1,2,3,6)]
names(dat)[3:4] <- c("STORAGE_HISTORICAL", "STORAGE_POLICY")

dat <- dat[dat$DATE >= as.Date("1984-01-01") & dat$DATE <= as.Date("2010-12-10"),]

# plot graph
lims <- as.Date(c("1980-01-01", "2020-01-01"))

# 
ggplot() + 
  geom_line(data = dat, aes(x = DATE, y = STORAGE_POLICY, color = 'Policy'), size = 1) +
  geom_line(data = dat, aes(x = DATE, y = STORAGE_HISTORICAL, color = 'Historical'), size = 1) +
  scale_x_date(breaks = seq(lims[1], lims[2], by="5 years"), 
              labels=date_format("%Y")) +
  xlab('Date') +
  ylab('Storage') + 
  scale_colour_manual(name='', values=c('Policy'='#8BB8E8', 'Historical'='#FFD100')) +
  scale_y_continuous(labels=comma) +
  theme(text = element_text(size=15), 
        legend.position = c(.1,.1), 
        legend.title = element_blank(),
        panel.background = element_rect(fill = "#F0F0F0"))


## de-trend the data ##
lm1 <- lm(dat$STORAGE_POLICY ~ dat$DATE)
predicted_pol <- data.frame(storage_pred = predict(lm1, dat), date=dat$DATE)

mod_spline_pol <- smooth.spline(dat$INDEX, dat$STORAGE_POLICY, df = 25)
pred_spline_pol <- data.frame(storage_pred = predict(mod_spline_pol, dat$INDEX)$y, date=dat$DATE)

lm2 <- lm(dat$STORAGE_HISTORICAL ~ dat$DATE)
predicted_hist <- data.frame(storage_pred = predict(lm2, dat), date=dat$DATE)

mod_spline_hist <- smooth.spline(dat$INDEX, dat$STORAGE_HISTORICAL, df = 25)
pred_spline_hist <- data.frame(storage_pred = predict(mod_spline_hist, dat$INDEX)$y, date=dat$DATE)


# plot with regresion line
ggplot() + 
  geom_line(data = dat, aes(x = DATE, y = STORAGE_POLICY, color = 'Policy'), size = 1) +
  geom_line(data = predicted_pol, aes(x=date, y=storage_pred, color = "Regression: Policy"), size = 1) +
  geom_line(data = pred_spline_pol, aes(x=date, y=storage_pred, color = "Spline: Policy"), size = 1) +
  scale_x_date(breaks = seq(lims[1], lims[2], by="5 years"), 
              labels=date_format("%Y")) +
  xlab('Date') +
  ylab('Storage') + 
  scale_colour_manual(name='', values=c('Policy'='#FFD100', 
                                        'Regression: Policy'='Brown',
                                        "Spline: Policy"="sienna1")) +
  scale_y_continuous(labels=comma) +
  theme(text = element_text(size=15), 
        legend.position = c(.1,.1), 
        legend.title = element_blank(),
        panel.background = element_rect(fill = "#F0F0F0"))

ggplot() + 
  geom_line(data = dat, aes(x = DATE, y = STORAGE_HISTORICAL, color = 'Historical'), size = 1) +
  geom_line(data = predicted_hist, aes(x=date, y=storage_pred, color = "Regression: Historical"), size = 1) +
  geom_line(data = pred_spline_hist, aes(x=date, y=storage_pred, color = "Spline: Historical"), size = 1) +
  scale_x_date(breaks = seq(lims[1], lims[2], by="5 years"), 
               labels=date_format("%Y")) +
  xlab('Date') +
  ylab('Storage') + 
  scale_colour_manual(name='', values=c('Historical'='#8BB8E8',
                                        'Regression: Historical'='Purple3',
                                        "Spline: Historical"="turquoise" )) +
  scale_y_continuous(labels=comma) +
  theme(text = element_text(size=15), 
        legend.position = c(.1,.1), 
        legend.title = element_blank(),
        panel.background = element_rect(fill = "#F0F0F0"))

detrended <- data.frame("INDEX" = dat$INDEX,
                        "DATE" = dat$DATE, 
                        "Pol.Resid" = lm1$residuals,
                        "Hist.Resid" = lm2$residuals)


# STEP BY STEP #
hist.ts <- ts(as.numeric(dat$STORAGE_HISTORICAL), frequency = 365.25, start=1984)
summary(hist.ts)

pol.ts <- ts(as.numeric(dat$STORAGE_POLICY), frequency = 365.25, start=1984)
summary(pol.ts)

# regression line
plot(hist.ts)
fit = lm(hist.ts~time(hist.ts), na.action=NULL)

# compare detrend vs first diff to see if stationary
par(mfrow=c(2, 1))
plot(resid(fit), type='o', main="Detrended")
plot(diff(hist.ts), type='o', main='First Difference')

# ACF/PACF - policy
par(mfrow=c(3, 1))
acf(pol.ts, lag.max = length(pol.ts), main='policy')
acf(resid(lm1), length(pol.ts), main='detrended - linear')
acf(resid(mod_spline_pol), length(pol.ts), main='detrended - spline')

par(mfrow=c(3, 1))
pacf(pol.ts, 100:length(pol.ts), main='policy')
pacf(resid(lm1), 100:length(pol.ts), main='detrended - linear')
pacf(resid(mod_spline_pol), 100:length(pol.ts), main='detrended - spline')

# ACF/PACF - historical
par(mfrow=c(3, 1))
acf(hist.ts, lag.max = length(hist.ts), main='historical')
acf(resid(lm2), length(hist.ts), main='detrended - linear')
acf(resid(mod_spline_pol), length(hist.ts), main='detrended - spline')

par(mfrow=c(3, 1))
pacf(hist.ts, 100:length(hist.ts), main='historical')
pacf(resid(lm2), 100:length(hist.ts), main='detrended - linear')
pacf(resid(mod_spline_hist), 100:length(hist.ts), main='detrended - spline')


sarima (resid(mod_spline_hist), 2, 0, 2 )



fit_h_sp <- auto.arima(resid(mod_spline_hist), trace=TRUE)
fit_p_sp <- auto.arima(resid(mod_spline_pol), trace=TRUE)

acf(fit_h_auto, lag.max = length(fit_h_auto), main='historical')

fit_h_auto <- auto.arima(hist.ts), trace=TRUE)
fit_p_auto <- auto.arima(pol.ts, trace=TRUE)

tsdiag(fit_h_sp)

tsdiag(fit_h_auto)
tsdiag(fit_p_auto)


plot(forecast(fit_h_auto), h=365)

plot(forecast(fit_h_auto), h=30)


#train and test - historical
train <- head(dat,-365)
train.hist.ts <- ts(as.numeric(train$STORAGE_HISTORICAL), frequency = 365.25, start=1984)
hist.train <- Arima(train.hist.ts, order=c(3,1,1))
plot(forecast(hist.train, h=365))
lines(hist.ts)

#train and test - policy
train.pol.ts <- ts(as.numeric(train$STORAGE_POLICY), frequency = 365.25, start=1984)
pol.train <- Arima(train.pol.ts, order=c(2,1,3))
plot(forecast(pol.train, h=365))
lines(pol.ts)



plot(forecast(auto.arima(hist.ts,D=1),h=200))

plot(hist.ts)
pred <- predict(hist.train, n.ahead = 365)$pred
lines(pred)

hist.train %>%
  forecast(h=365) %>%
  autoplot() + autolayer(testing.h)

training.p <- pol.ts[1:(length(pol.ts)-365)]
testing.p <- pol.ts[9477:length(pol.ts)]
pol.train <- Arima(training.p, order=c(2,1,3))
pol.train %>%
  forecast(h=364) %>%
  autoplot() + autolayer(test)



adf.test(resid(mod_spline_hist), k=1000)



plot(hist.ts, type='p', ylab='Mortality')
lines(smooth.spline(time(hist.ts), hist.ts))
lines(smooth.spline(time(hist.ts), hist.ts, spar=1))

hist.spline <- smooth.spline(time(hist.ts), hist.ts)

par(mfrow=c(3, 1))
acf(hist.ts, 48, main='hist')
acf(resid(hist.spline), 48, main='detrended')
acf(diff(hist.ts), 48, main='first differences')


adf.test(resid(hist.spline), k=1000)


# cycles 
cycle(hist.ts)
plot(aggregate(hist.ts,FUN=mean))

adf.test(hist.ts)

hist.dc <- decompose(hist.ts)
adf.test(hist.dc$random)


# divide into train and test #
train = data[1:3000 , c(2,3)]
test = data[3001:nrow(data), c(2,3)]


train$DATE = NULL

#training model
model = auto.arima(train)

summary(model)

forecast = predict(model,997)

plot(forecast(model), 10)


  rmse(test$STORAGE_HISTORICAL, forecast$pred)

mod <- lm1; coef(mod)
mean(diff())

#test of stationarity: adf
#augmented dickey-fuller test
library(tseries)
adf.test(dat[,4], k=100)

## ACF and PACF
acf(resid(lm1), 100)
pacf(resid(lm1))

plot(dat$DATE, lm1$residuals, col= 'red', type='l')
lines(dat$DATE, dat$STORAGE_HISTORICAL, col ='purple')


library(xts)
dat_asxts <- as.xts(dat$STORAGE_POLICY , order.by = as.Date(dat$DATE))
weekly <- apply.weekly(dat_asxts, mean)



#create time series variable
hist.ts<-ts(as.numeric(dat$STORAGE_HISTORICAL),frequency = 365, start=2009)

# dam.ts<-ts(dat$STORAGE_POLICY, start=1984, frequency = 365)
# summary(dam.ts)
#> usnim_ts = ts(usnim_2002[, 2], start = c(2002, 1), frequency = 4)

#decomposition
hist.dc<-decompose(hist.ts)
plot(hist.dc)


hist.diff <- diff(hist.ts)


#train and test
training <- subset(hist.ts, end=length(hist.ts)-365)
test <- subset(hist.ts, start=length(hist.ts)-364)
cafe.train <- Arima(training, order=c(3,0,1), lambda=0)
cafe.train %>%
  forecast(h=365) %>%
  autoplot() + autolayer(test)



plot(forecast(arima(test, order = c(3, 0, 1)), 365))


# forecast
library(forecast)
fit2<-auto.arima(hist.ts, trace=TRUE)
plot(forecast(fit2,365))

adf.test(dam.dc$random, k=100)


acf(dam.dc$random,lag.max=(length(dam.dc$random)))


aut_ar <- auto.arima( dam.ts , trace=TRUE)
forecast(aut_ar, h=20)


arima <- auto.arima( dam.ts )
summary(arima)
forecast(arima,10)

plot(forecast(aut_ar, h= 365))

xx<-forecast(aut_ar, )
