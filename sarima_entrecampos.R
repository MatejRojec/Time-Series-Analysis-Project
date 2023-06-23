library(readxl)
library(tsibble)
library(dplyr)
library(ggplot2)
library(imputeTS)
library(patchwork)
library(tseries)
library(forecast)


####################################### Functions #############################################################################################


# Set the working directory to the parent directory
setwd("C:/Users/matej/Documents/FMF mag/2.semester/Time Series Analysis/Research Project")


p_values <- function(model){
  (1-pnorm(abs(model$coef)/sqrt(diag(model$var.coef))))*2 * 100
}

############################################################################### #############################################################################################


Entrecampos <- readxl::read_excel("Data1/2021 Entrecampos.xlsx") %>%
  dplyr::rename(time = 'Entrecampos', ozon = 'Ozono (µg/m3)') %>%
  as.data.frame() 

Entrecampos$missing <- ifelse(is.na(Entrecampos$ozon), "Missing", "Known")
# fill in the missing data
Entrecampos$ozon <- na_interpolation(Entrecampos$ozon)
# plot the data
Entrecampos$time <- as.POSIXct(Entrecampos$time, format = "%Y-%m-%d %H:%M:%S")

column <- Entrecampos$ozon

# Perform Min-Max scaling
# normalized_column <- scale(column)

# Replace the original column with the normalized values


ts_Entrecampos <- ts(Entrecampos$ozon, start = c(2021, 1, 1, 0), frequency = 24)
ts_log_Entrecampos <- ts(log(Entrecampos$ozon + 0.001), start = c(2021, 1, 1, 0), frequency = 24)

lambda = 1/2
ts_lambda_Entrecampos <- ts(( (Entrecampos$ozon + 0.001)^lambda -1)/ lambda , start = c(2021, 1, 1, 0), frequency = 24)

ts_norm <- ts(normalized_column, start = c(2021, 1, 1, 0), frequency = 24)

acf(ts_lambda_Entrecampos, lag = 100)
pacf(ts_lambda_Entrecampos, lag = 100)


y_Entrecampos <- stl(ts_Entrecampos , s.window="period")
autoplot(y_Entrecampos)

lag_data <- diff(column ,24) 
lag_data_lagged <- diff(diff(column ,24)) 
ts_Entrecampos_lag <- ts(lag_data , start = c(2021, 1, 1, 24), frequency = 24)
ts_Entrecampos_lag_lagged <- ts(lag_data_lagged , start = c(2021, 1, 1, 25), frequency = 24)


acf(ts_Entrecampos, lag.max = 200, plot = TRUE, main = "ACF function the Entrecampos Amadora time series")
abline(v = seq(0, 200, 1), lty = 2)
pacf(ts_Entrecampos, lag.max = 200, plot = TRUE, main = "PACF function the Entrecampos Amadora time series")
abline(v = seq(0, 200, 1), lty = 2)


acf(ts_log_Entrecampos, lag.max = 200, plot = TRUE, main = "ACF function the log Entrecampos Amadora time series")
abline(v = seq(0, 200, 1), lty = 2)
pacf(ts_log_Entrecampos, lag.max = 200, plot = TRUE, main = "PACF function the log Entrecampos Amadora time series")
abline(v = seq(0, 200, 1), lty = 2)

acf(ts_Entrecampos_lag, lag.max = 200, plot = TRUE, main = "ACF function the 24-hour lagged Entrecampos Amadora time series")
abline(v = seq(0, 200, 1), lty = 2)
pacf(ts_Entrecampos_lag, lag.max = 200, plot = TRUE, main = "PACF function the 24-hour lagged Entrecampos Amadora time series")
abline(v = seq(0, 200, 1), lty = 2)

acf(ts_Entrecampos_lag_lagged, lag.max = 200, plot = TRUE, main = "PACF function the lagged 24-hour lagged Entrecampos Amadora time series")
abline(v = seq(0, 200, 1), lty = 2)
pacf(ts_Entrecampos_lag_lagged, lag.max = 200, plot = TRUE, main = "PACF function the lagged 24-hour lagged Entrecampos Amadora time series")
abline(v = seq(0, 200, 1), lty = 2)


arima_model <- auto.arima(ts_Entrecampos)
arima_model
arima_model$aic
sqrt(mean((arima_model$residuals^2)))


tsdisplay(residuals(arima_model), lag.max = 60) 
tsdiag(arima_model, gof.lag = 60)

Entrecampos$ozon_log <- log(Entrecampos$ozon + 0.01)
Entrecampos$ozon_lagged <- c(rep(NA, 24), diff(Entrecampos$ozon, lag = 24))
Entrecampos$ozon_lagged_lag <- c(rep(NA, 25), diff(diff(Entrecampos$ozon, lag = 24)))


plot_log <- ggplot(Entrecampos, aes(x = time, y = ozon_log)) +
  geom_line() +
  labs(x = "Time", y = "Log(Ozono)", title = "Entrecampos/Amadora hourly logarithmic ozon levels in 2021") +
  theme_minimal()

plot_lagged <- ggplot(Entrecampos, aes(x = time, y = ozon_lagged)) +
  geom_line() +
  labs(x = "Time", y = "Seasonal differenced ozono", title = "Entrecampos/Amadora hourly ozon levels after a seasonal difference in 2021") +
  theme_minimal()

plot_lagged_lag <- ggplot(Entrecampos, aes(x = time, y = ozon_lagged_lag)) +
  geom_line() +
  labs(x = "Time", y = "Seasonal difference and a non-seasonal difference ozono (µg/m3)", title = "Entrecampos/Amadora hourly ozon levels after a seasonal difference and a non-seasonal difference in 2021") +
  theme_minimal()




y_Entrecampos <- stl(ts_log_Entrecampos , s.window="period")

adf_result <- adf.test(ts_Entrecampos_lag)

arima_model <- auto.arima(ts_Entrecampos_lag)
arima_model
# ARIMA(1,0,2)(2,0,0)[24]

sqrt(mean(arima_model$residuals^2))

arima_model <- arima(x = ts_Entrecampos_lag, order = c(2,0,1), seasonal = list(order = c(1,0,1), period = 24))
arima_model$aic
sqrt(mean((arima_model$residuals^2)))

tsdisplay(residuals(arima_model), lag.max = 60) 
tsdiag(arima_model, gof.lag = 60)
# ARIMA(5,1,0)(2,0,0)[24] (2,0,1)x(1,0,1)

############################################### Model fitting ###############################################

#####  Differenced Models ###########    

model9 <- arima(x = ts_Entrecampos, order = c(0, 1, 0), seasonal = list(order = c(1, 1, 0), period = 24))
model9; 
sqrt(mean(model9$residuals^2));
model9$aic;
p_values(model9);  

model10 <- arima(x = ts_Entrecampos, order = c(0, 0, 0), seasonal = list(order = c(1, 0, 0), period = 24))
model10; 
sqrt(mean(model10$residuals^2));
model10$aic;
p_values(model10); 


model12 <- arima(x = ts_Entrecampos, order = c(2, 0, 1), seasonal = list(order = c(1, 1, 1), period = 24))
model12; 
sqrt(mean(model12$residuals^2));
model12$aic;
p_values(model12)   

model14 <- arima(x = ts_Entrecampos, order = c(1, 0, 1), seasonal = list(order = c(1, 1, 1), period = 24))
model14; 
sqrt(mean(model12$residuals^2));
model14$aic;
p_values(model14)   

model14
tsdiag(model14)
Box.test(model14$residuals,lag=1, type="Ljung")


model13 <- arima(x = ts_Entrecampos, order = c(2, 0, 1), seasonal = list(order = c(1, 1, 2), period = 24))
model13; 
sqrt(mean(model13$residuals^2));
model13$aic;
p_values(model13)  

model17 <- arima(x = ts_Entrecampos, order = c(2, 0, 1), seasonal = list(order = c(1, 1, 1), period = 24))
model17; 
sqrt(mean(model17$residuals^2));
model17$aic;
p_values(model17)   

model18 <- arima(x = ts_Entrecampos, order = c(2, 1, 1), seasonal = list(order = c(1, 1, 1), period = 24))
model18; 
sqrt(mean(model18$residuals^2));
model18$aic;
p_values(model18)   


######  Non-Differenced Models ###########    

model20 <- arima(x = ts_Entrecampos, order = c(1, 0, 1), seasonal = list(order = c(1, 0, 2), period = 24))
model20; 
sqrt(mean(model20$residuals^2));
model20$aic;
p_values(model20)   



model0 <- arima(x = ts_Entrecampos, order = c(1,0,1), seasonal = list(order = c(2,1,0), period = 24))
tsdisplay(residuals(model0), lag.max = 80) 
tsdiag(model0, gof.lag = 40)
model0;
model0$aic
sqrt(mean((model0$residuals^2)))


model000 <- arima(x = ts_Entrecampos, order = c(3,0,1), seasonal = list(order = c(1,0,1), period = 24))
tsdisplay(residuals(model000), lag.max = 80) 
tsdiag(model000, gof.lag = 40)
model000$aic;
sqrt(mean(model000$residuals^2))




model00 <- arima(x = ts_Entrecampos, order = c(2, 0, 0))
model00$aic;
sqrt(mean(model00$residuals^2))
p_values(model00)


model1 <- arima(x = ts_Entrecampos, order = c(3, 0, 0), seasonal = list(order = c(1, 0, 0), period = 24))
model1; 
sqrt(mean(model1$residuals^2))
model1$aic;
tsdiag(model1)
Box.test(model1$residuals,lag=1, type="Ljung")


model17 <- arima(x = ts_Entrecampos, order = c(1, 0, 1), seasonal = list(order = c(2, 1, 0), period = 24))
model17; 
sqrt(mean(model17$residuals^2))
tsdiag(model17)
Box.test(model17$residuals,lag=1, type="Ljung")


model2 <- arima(x = ts_Entrecampos, order = c(2, 0, 0), seasonal = list(order = c(1, 0, 0), period = 24))
model2; 
sqrt(mean(model2$residuals^2))
model2$aic;
p_values(model2)  #  

model3 <- arima(x = ts_Entrecampos, order = c(3, 0, 1), seasonal = list(order = c(1, 0, 0), period = 24))
model3
sqrt(mean(model3$residuals^2))

model32 <- arima(x = ts_Entrecampos, order = c(3, 0, 1), seasonal = list(order = c(1, 0, 1), period = 24))
model32
sqrt(mean(model32$residuals^2))



model4 <- arima(x = ts_Entrecampos, order = c(1, 0, 1), seasonal = list(order = c(1, 0, 1), period = 24))
model4
sqrt(mean(model4$residuals^2))
model4$aic;  

model5 <- arima(x = ts_Entrecampos, order = c(1, 0, 0), seasonal = list(order = c(1, 0, 1), period = 24))
model5;  
sqrt(mean(model5$residuals^2))
p_values(model5) 

model6 <- arima(x = ts_Entrecampos, order = c(2, 0, 1), seasonal = list(order = c(1, 0, 1), period = 24))
model6;  
sqrt(mean(model6$residuals^2))
p_values(model6)

model7 <- arima(x=ts_Entrecampos, order=c(1,0,0), seasonal=list(order=c(1,0,2),period=24))
sqrt(mean(model7$residuals^2))
model7$aic
model7;  
p_values(model7)  

model8 <- arima(x=ts_Entrecampos, order=c(4,0,0), seasonal=list(order=c(1,0,2),period=24),transform.pars = FALSE, fixed= c(NA,0,NA,NA,NA,NA,0,NA))
model8;           
sqrt(mean(model8$residuals^2));
model8$aic;





############################################### Model diagnosis ###############################################

tsdisplay(residuals(model4), lag.max = 60) 
tsdiag(model4)
Box.test(model4$residuals,lag=1, type="Ljung")


############################################### Prediciton #############################################




forec= predict(model4,n.ahead=5)

df <- data.frame(
  datetime = time(ts_Entrecampos),
  value = as.vector(ts_Entrecampos)
)

# Convert forecast to data frame
pred_df <- data.frame(
  datetime = time(forec$pred),
  value = forec$pred,
  ymin = forec$pred - 2 * forec$se,
  ymax = forec$pred + 2 * forec$se
)

# Plotting with ggplot2
library(ggplot2)

p <- ggplot() +
  geom_line(data = df[8720:8760,], aes(x = datetime, y = value, color = "Original"), size = 1) +
  geom_line(data = pred_df, aes(x = datetime, y = value, color = "Predicted"), size = 1) +
  geom_ribbon(data = pred_df, aes(x = datetime, ymin = ymin, ymax = ymax),
              fill = "red", alpha = 0.3) +
  labs(x = "Time", y = "Ozono (µg/m3)", title = "Predicted and original hourly-ground-levels of ozon in Entrecampos") +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = "red", name = "Confidence Interval") +
  theme_minimal()


# Add the legend
p <- p + theme(legend.position = "bottom")

# Display the plot
print(p)
