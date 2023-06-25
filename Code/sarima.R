library(readxl)
library(tsibble)
library(dplyr)
library(ggplot2)
library(imputeTS)
library(patchwork)
library(tseries)
library(forecast)



#   Check to see if every hour is included in the dataset

start_time <- as.POSIXct("2021-01-01 00:00:00")
end_time <- as.POSIXct("2021-12-31 23:00:00")

# Calculate the number of hours between start and end time
num_hours <- as.numeric(difftime(end_time, start_time, units = "hours")) + 1
# number of rows match


Alfragide_Amadora <- readxl::read_excel("Data1/2021 Alfragide-Amadora.xlsx") %>%
          dplyr::rename(time = 'Alfragide/Amadora', ozon = 'Ozono (µg/m3)') %>%
          as.data.frame() 
Alfragide_Amadora$missing <- ifelse(is.na(Alfragide_Amadora$ozon), "Missing", "Known")
# fill in the missing data
Alfragide_Amadora$ozon <- na_interpolation(Alfragide_Amadora$ozon)
# plot the data
Alfragide_Amadora$time <- as.POSIXct(Alfragide_Amadora$time, format = "%Y-%m-%d %H:%M:%S")

plot1 <- ggplot(Alfragide_Amadora, aes(x = time, y = ozon, color = missing, group = 1)) +
  geom_point() +
  scale_color_manual(values = c("blue", "red"), labels = c("Known", "Missing")) +
  labs(x = "Time", y = "Ozono (µg/m3)", title = "Alfragide/Amadora hourly ozon levels in 2021") +
  theme_minimal()

  
Reboleira <-  read_excel("Data1/2021 Reboleira.xlsx") %>% rename(time = 'Reboleira', ozon = 'Ozono (µg/m3)')
Reboleira$missing <- ifelse(is.na(Reboleira$ozon), "Missing", "Known")
Reboleira$ozon <- na_interpolation(Reboleira$ozon)
Reboleira$time <- as.POSIXct(Reboleira$time, format = "%Y-%m-%d %H:%M:%S")

plot2 <- ggplot(Reboleira, aes(x = time, y = ozon, color = missing, group = 1)) +
  geom_point(size = 0.1) +
  scale_color_manual(values = c("blue", "red"), labels = c("Known", "Missing")) +
  labs(x = "Time", y = "Ozono (µg/m3)", title = "Reboleira hourly ozon levels in 2021") +
  theme_minimal()

Beato <-  read_excel("Data1/2021 Beato.xlsx") %>% rename(time = 'Beato', ozon = 'Ozono (µg/m3)')
Beato$missing <- ifelse(is.na(Beato$ozon), "Missing", "Known")
Beato$ozon <- na_interpolation(Beato$ozon)
Beato$time <- as.POSIXct(Beato$time, format = "%Y-%m-%d %H:%M:%S")

plot3 <- ggplot(Beato, aes(x = time, y = ozon, color = missing, group = 1)) +
  geom_point(size = 0.1) +
  scale_color_manual(values = c("blue", "red"), labels = c("Known", "Missing")) +
  labs(x = "Time", y = "Ozono (µg/m3)", title = "Beato hourly ozon levels in 2021") +
  theme_minimal()

Olivais <-  read_excel("Data1/2021 Olivais.xlsx") %>% rename(time = 'Olivais', ozon = 'Ozono (µg/m3)')
Olivais$missing <- ifelse(is.na(Olivais$ozon), "Missing", "Known")
Olivais$ozon <- na_interpolation(Olivais$ozon)
Olivais$time <- as.POSIXct(Olivais$time, format = "%Y-%m-%d %H:%M:%S")

plot4 <- ggplot(Olivais, aes(x = time, y = ozon, color = missing, group = 1)) +
  geom_point(size = 0.1) +
  scale_color_manual(values = c("blue", "red"), labels = c("Known", "Missing")) +
  labs(x = "Time", y = "Ozono (µg/m3)", title = "Olivais hourly ozon levels in 2021") +
  theme_minimal()


Entrecampos <-  read_excel("Data1/2021 Entrecampos.xlsx") %>% rename(time = 'Entrecampos', ozon = 'Ozono (µg/m3)')
Entrecampos$missing <- ifelse(is.na(Entrecampos$ozon), "Missing", "Known")
Entrecampos$ozon <- na_interpolation(Entrecampos$ozon)
Entrecampos$time <- as.POSIXct(Entrecampos$time, format = "%Y-%m-%d %H:%M:%S")

plot5 <- ggplot(Entrecampos, aes(x = time, y = ozon, color = missing, group = 1)) +
  geom_point(size = 0.1) +
  scale_color_manual(values = c("blue", "red"), labels = c("Known", "Missing")) +
  labs(x = "Time", y = "Ozono (µg/m3)", title = "Entrecampos hourly ozon levels in 2021") +
  theme_minimal()


data <- data.frame(Alfragide_Amadora$ozon, Reboleira$ozon, Entrecampos$ozon, Olivais$ozon, Beato$ozon)
cor(data)

combined_plot <- ggplot() +
  geom_line(data = Alfragide_Amadora[1:100,], aes(x = time, y = ozon, color = "Alfragide/Amadora"), size = 0.8) +
  geom_line(data = Reboleira[1:100,], aes(x = time, y = ozon, color = "Reboleira"), size = 0.8) +
  geom_line(data = Beato[1:100,], aes(x = time, y = ozon, color = "Beato"), size = 0.8) +
  geom_line(data = Olivais[1:100,], aes(x = time, y = ozon, color = "Olivais"), size = 0.8) +
  geom_line(data = Entrecampos[1:100,], aes(x = time, y = ozon, color = "Entrecampos"), size = 0.8) +
  scale_color_manual(values = c("Alfragide/Amadora" = "blue", "Reboleira" = "red", "Beato" = "green",
                                "Olivais" = "purple", "Entrecampos" = "orange"),
                     labels = c("Alfragide/Amadora", "Reboleira", "Beato", "Olivais", "Entrecampos")) +
  labs(x = "Time", y = "Ozono (µg/m3)", title = "Hourly Ozone Levels in the first five days of 2021") +
  theme_minimal()

# Display the combined plot
combined_plot

ts_Alfragide_Amadora <- ts(Alfragide_Amadora$ozon, start = c(2021, 1, 1, 0), frequency = 24)
ts_Beato <- ts(Beato$ozon, start = c(2021, 1, 1, 0), frequency = 24)
ts_Entrecampos <- ts(Entrecampos$ozon, start = c(2021, 1, 1, 0), frequency = 24)
ts_Olivais <- ts(Olivais$ozon, start = c(2021, 1, 1, 0), frequency = 24)
ts_Reboleira <- ts(Reboleira$ozon, start = c(2021, 1, 1, 0), frequency = 24)

ts_log_Alfragide_Amadora <- ts(log(Alfragide_Amadora$ozon + 0.001)[1:4000], start = c(2021, 1, 1, 0), frequency = 24)
y_log_Alfragide_Amadora <- stl(ts_log_Alfragide_Amadora , s.window="period")
y_Alfragide_Amadora <- stl(ts_Alfragide_Amadora , s.window="period")
sarima_model <- auto.arima(ts_log_Alfragide_Amadora)
print(sarima_model)

y_Beato <- stl(ts_Beato , s.window="period")
y_Entrecampos <- stl(ts_Entrecampos , s.window="period") 
y_Olivais <- stl(ts_Olivais , s.window="period")
y_Reboleira <-  stl(ts_Reboleira , s.window="period")

autoplot(y_Alfragide_Amadora)
autoplot(y_Beato)
autoplot(y_Entrecampos)
autoplot(y_Olivais)
autoplot(y_Reboleira)
autoplot(y_log_Alfragide_Amadora)

acf(ts_Alfragide_Amadora, lag.max = 200, plot = TRUE)
abline(v = seq(0, 200, 1), lty = 2)
pacf(ts_Alfragide_Amadora, lag.max = 200, plot = TRUE)
abline(v = seq(0, 200, 1), lty = 2)


acf(ts_log_Alfragide_Amadora, lag.max = 200, plot = TRUE)
abline(v = seq(0, 200, 1), lty = 2)
pacf(ts_log_Alfragide_Amadora, lag.max = 200, plot = TRUE)
abline(v = seq(0, 200, 1), lty = 2)

tsdisplay ( diff ( ts_log_Alfragide_Amadora ,24) , lag.max = 200 ,
            main = " Seasonally differenced : (0 ,0 ,0) x (0 ,1 ,0) _ 24 " , xlab = " Time ( hrs ) " )

sarima_model <- auto.arima(ts_Alfragide_Amadora)
print(sarima_model)
print(sarima_model$arma)


acf(y_Alfragide_Amadora$time.series[,1], lag.max = 200, plot = TRUE)
abline(v = seq(0, 200, 1), lty = 2)

pacf(y_Alfragide_Amadora$time.series[,1], lag.max = 200, plot = TRUE)
abline(v = seq(0, 200, 1), lty = 2)

acf(y_log_Alfragide_Amadora$time.series[,1], lag.max = 200, plot = TRUE)
abline(v = seq(0, 200, 1), lty = 2)

pacf(y_log_Alfragide_Amadora$time.series[,1], lag.max = 200, plot = TRUE)
abline(v = seq(0, 200, 1), lty = 2)


acf(ts_Beato, lag.max = 200, plot = TRUE)
abline(v = seq(0, 200, 1), lty = 2)
pacf(ts_Beato, lag.max = 200, plot = TRUE)
abline(v = seq(0, 200, 1), lty = 2)

acf(ts_Entrecampos, lag.max = 200, plot = TRUE)
abline(v = seq(0, 200, 1), lty = 2)
pacf(ts_Entrecampos, lag.max = 200, plot = TRUE)
abline(v = seq(0, 200, 1), lty = 2)

acf(ts_Olivais, lag.max = 200, plot = TRUE)
abline(v = seq(0, 200, 1), lty = 2)
pacf(ts_Olivais, lag.max = 200, plot = TRUE)
abline(v = seq(0, 200, 1), lty = 2)

acf(ts_Reboleira, lag.max = 200, plot = TRUE)
abline(v = seq(0, 200, 1), lty = 2)
pacf(ts_Reboleira, lag.max = 200, plot = TRUE)
abline(v = seq(0, 200, 1), lty = 2)


# ARIMA(3,1,5)(2,0,0)[24] 

