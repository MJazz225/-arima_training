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
