####################################################
##             Guy Dotan - 403-882-450            ##
##             Stats 415 - Final Project          ##
##                    12/01/2018                  ##
####################################################


## Set working directory folder
setwd("~/Documents/UCLA MAS/2018 Fall/STATS 415/Final Project")

library("readxl")  # package to read in Excel files
options(digits=12)  # set number of numeric digits

# read in full data set
dam <- read_xlsx("Data/New Melones-last-corrected.xlsx") 

# split data into two sets
hist <- dam[-c(1,2,3), c(1,2)]  # historical data
pol <- dam[-c(1,2,3), c(1,3)]  # polosed policy

# assign new headers
col_names <- c("DATE","STORAGE")
names(hist) <- col_names
names(pol) <- col_names

# add index columns
hist$INDEX = 1:nrow(hist)
pol$INDEX = 1:nrow(pol)
hist <- hist[,c("INDEX","DATE","STORAGE")]
pol <- pol[,c("INDEX","DATE","STORAGE")]

# check column types
str(hist)
str(pol)

# format columns
hist$DATE <- as.Date(hist$DATE, format = "%Y-%m-%d")
hist$STORAGE <- as.numeric(hist$STORAGE)
pol$DATE <- as.Date(pol$DATE, format = "%Y-%m-%d")
pol$STORAGE <- as.numeric(pol$STORAGE)

plot(hist$INDEX,hist$STORAGE,type="l",col="red")
lines(pol$INDEX,pol$STORAGE,col="green")

write.csv(x = hist, file = "Data/historical_new_melones.csv", row.names = FALSE)
write.csv(x = pol, file = "Data/policy_new_melones.csv", row.names = FALSE)
