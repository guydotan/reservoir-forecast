# load packages
library(ggplot2)   # pretty plots
library(gridExtra) # ggplot formating
library(dplyr)     # transform data
library(tseries)   # time series
library(scales)    # scaling
library(astsa)     # time series analysis
library(TSA)       # more time series analysis
library(knitr)     # knitr format
library(kableExtra) # table building
library(FitAR)      # ts modeling
library(forecast)   # ts forecasing

## Set working directory folder
setwd("~/Documents/UCLA MAS/2018 Fall/STATS 415/Final Project")

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

# create time series - historical
hist.ts <- ts(as.numeric(dat$STORAGE_HISTORICAL), frequency = 365.25, start=1984)
summary(hist.ts)

# create time series - policy
pol.ts <- ts(as.numeric(dat$STORAGE_HISTORICAL), frequency = 365.25, start=1984)
summary(pol.ts)


### DETRENDING ###
# create spline - historical
mod_spline_hist <- smooth.spline(dat$INDEX, dat$STORAGE_HISTORICAL, df = 25)
pred_spline_hist <- data.frame(storage_pred = predict(mod_spline_hist, dat$INDEX)$y, date=dat$DATE)

# create spline - policy
mod_spline_pol <- smooth.spline(dat$INDEX, dat$STORAGE_POLICY, df = 25)
pred_spline_pol <- data.frame(storage_pred = predict(mod_spline_pol, dat$INDEX)$y, date=dat$DATE)

hist.spline <- dat[,c("DATE","STORAGE_HISTORICAL")] %>%
  mutate(line_fit = as.numeric(pred_spline_hist$storage_pred)) %>%
  mutate(resids = STORAGE_HISTORICAL - line_fit) %>%
  select(resids)

detrended_hist <- as.ts(hist.spline$resids)
adf.test(detrended_hist)

pol.spline <- dat[,c("DATE","STORAGE_POLICY")] %>%
  mutate(line_fit = as.numeric(pred_spline_pol$storage_pred)) %>%
  mutate(resids = STORAGE_POLICY - line_fit) %>%
  select(resids)

detrended_pol <- as.ts(pol.spline$resids)
adf.test(detrended_pol)

# test Ljung-Box test
LBQPlot(detrended_pol)
LBQPlot(detrended_hist)


## ACF/PACF detrended
# ACF/PACF - policy
par(mfrow=c(2, 2))
acf(pol.ts, lag.max = 365, main='policy')
pacf(pol.ts, 365, main='policy')
acf(detrended_pol, 365, main='detrended - spline')
pacf(detrended_pol, 365, main='detrended - spline')

# ACF/PACF - historical
par(mfrow=c(2, 2))
acf(hist.ts, lag.max = 365, main='historical')
pacf(hist.ts, 365, main='historical')
acf(detrended_hist, 365, main='detrended - spline')
pacf(detrended_hist, 365, main='detrended - spline')


### SPECTRAL ANALYSIS ###
# Periodogram
pgram_hist <- spec.pgram(detrended_hist, taper = 0, log ="no", detrend = FALSE , main = "")
frequencies_h <- pgram_hist$freq
spectrum_h <- pgram_hist$spec

# Spectral values to find high frequency points
freqdf <- data.frame(freq = frequencies_h, spectrum = spectrum_h) %>%
  arrange(desc(spectrum_h)) %>%
  head(100) %>%
  mutate(cycle = 1/freq)


# now our top cycles are all weekly
# HISTORICAL
yearly_ts_hist <-  dat[,c(2,3)] %>%
  mutate(line_fit = as.numeric(pred_spline_hist$storage_pred)) %>%
  mutate(resids = STORAGE_HISTORICAL - line_fit) %>%
  mutate(yearly = year(DATE)) %>%
  group_by(yearly) %>%
  summarise(average_resid = mean(resids), average_storage = mean(STORAGE_HISTORICAL))
yearly_ts_hist

#remove yearly cycle 
yearly_removets_hist <- dat[,c(2,3)] %>%
  mutate(line_fit = pred_spline_hist$storage_pred) %>%
  mutate(resids = STORAGE_HISTORICAL - line_fit) %>%
  mutate(yearly = year(DATE))

test <- left_join(x = yearly_removets_hist, y=yearly_ts_hist[,c(1,2)], by = "yearly")
yearly_removets_hist <- test %>%
  mutate(resids_nocycle = resids - average_resid)

detrend_decyclets_hist <- yearly_removets_hist$resids_nocycle


# look at series
yearly_removets_hist %>% 
  ggplot(aes(x=DATE, y = resids_nocycle)) +
  geom_line()

#adf test
adf.test(detrend_decyclets_hist)


#bar chart of avgs
xxx <- as.data.frame(yearly_ts_hist)
g <- ggplot(xxx, aes(yearly,average_storage))
g+geom_col(fill = rainbow(n = length(xxx$yearly)), alpha = .7)


#cycle removed
acf(detrend_decyclets_hist, lag.max = 365)
pacf(detrend_decyclets_hist, lag.max = 365)

acf(detrend_decyclets_hist, lag.max = 365)
pacf(detrend_decyclets_hist, lag.max = 365)

LBQPlot(detrend_decyclets_hist)


### ARIMA SELECTION ###
# Loop to search for best ARIMA model
n = length(dat[,2])
AIC = matrix(cbind(rep(0,5), 5), nrow = 5, ncol = 5) -> AICc -> BIC
for (p in 1:5){
  for (q in 1:5){
    fit = arima(detrend_decyclets, order = c(p, 0, q))
    AIC[p,q] = AIC(fit)
    BIC[p,q] = AIC(fit, k = log(length(n)))
  }
}
hist_models <- as.data.frame(AIC)

# model selecting
model_hist <- auto.arima(detrend_decyclets_hist, stepwise = F, approximation = F, max.order = 5, allowdrift = T, trace=T)
model_pol <- auto.arima(detrend_decyclets_pol, stepwise = F, approximation = F, max.order = 5, allowdrift = T, trace=T)
thearima <- sarima(detrend_decyclets_hist, 3,0,1)

# model diagnostics
tsdiag(model_hist)
standardresids <- rstandard(model_hist)
qqnorm(standardresids)
qqline(standardresids, col = "red")
hist(standardresids, breaks= 50)


### FORECAST ###
#train and test
training <- subset(hist.ts, end=length(hist.ts)-365)
test <- subset(hist.ts, start=length(hist.ts)-364)
mod.train <- Arima(training, order=c(3,0,1), lambda=0)

fore <- predict(model, 365)

mod.train %>%
  forecast(h=365) %>%
  autoplot() + autolayer(test)