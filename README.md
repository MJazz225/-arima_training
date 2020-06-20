# -arima_training
learn to code for arima in different style in R language  


x <- cbind(rnorm(10, mean = 0, sd = 1))
n <- 8

a_roll <- NULL
for (i in 1:8){
  a <- x[i:i+1,]
  b <- forecast(auto.arima(a))$mean[1]
  a_roll <- c(a_roll, a)
}

a <- x[1:3,]
for (i in nrow(window2)) {
  a <- data_1row[i:999+i,]
  a_update <- forecast(auto.arima(a)$mean[1])
  a_rolling <- rbind(a_rolling, a_update)
}

##計量的作業tejapi用不了就用quantmod

## Author: Ming Jue ##

#try to write a code to download data, and forecast return for each company and save 
#the forecast value, next week need to check the code

#how to forecast every period, require use 1,000 sample in every company
rm(list=ls())
if(!require(Tejapi))install.packages("Tejapi")
if(!require(forecast))install.packages("forecast")
if(!require(quantmod))install.packages("quantmod")
if(!require(PerformanceAnalytics))install.packages("PerformanceAnalytics")

##資料 2891中信金 2330臺積電 6005群益證 3008大力光 2317鴻海 2327國巨 2412中華電
Tickers <- as.matrix(read.table("Tickers2.txt"))
#pb <- winProgressBar(title = "progress bar", min = 0, max = length(Tickers))

data_raw1 <- list()
for(i in seq(1, nrow(Tickers))){
  data_raw1[[i]] <- getSymbols(Symbols=Tickers[i], from = "2016-03-01", to = Sys.Date(), 
                               auto.assign = FALSE)
}

data1 <- NULL
##na omit
##只拿adjusted price
for(i in seq(1,length(data_raw1))){
  
  data_raw1[[i]] <- na.omit(data_raw1[[i]])
  temp <- data_raw1[[i]]
  return <- diff(log(temp[,6]))
  
  data1[i] <- list(return)
  data1[[i]] <- na.omit(data1[[i]])
}
names(data1) <- Tickers

#把多重list全部合并成panel data
data_return <- NULL
for (i in seq(1,length(data1))) {
  tem <- data1[[i]]
  data_return <- cbind(data_return, tem)  
}

##rolling window 分割資料
window1 <- data_return[1:1000,]
window2 <- data_return[1001:nrow(data_return),]
##rolling
startdate <- as.Date(as.character("2016-03-02"))
enddate <- as.Date(as.character("2020-04-09"))

###########################test
len1 <- nrow(data_return)
a_rolling <- NULL
data_1row <- as.data.frame(as.xts(data_return[,1]))
data_1row <- as.xts(data_1row)
full_data <- NULL

for (j in 1:ncol(window2)) {
  for (i in 1:nrow(data_return)) {
    a_update <- forecast(auto.arima(data_return[i-999:i,j]))$mean[1]
  }
  full_data <- cbind(full_data, a_update)
}
a_update <- forecast(auto.arima(data_return[1:1000,1]))$mean[1]
