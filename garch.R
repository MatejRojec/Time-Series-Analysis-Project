# Load the packages
library(gdata)
library(rugarch)

setwd("Data2/")

# List the files in the directory
files <- list.files()

# Read each file
data_list <- lapply(files, function(file) {
  data <- read.xls(file, sheet = 1, skip = 1, header = FALSE)
  colnames(data) <- data[3, ]
  data <- data[-c(1:3),]
  data <-  data[rev(row.names(data)), ]
  data$Close <- as.numeric(data$Close)  # Convert Close column to numeric
  data$log_returns <- log(data$Close) - log(lag(data$Close))
  data <- data[,c("Date","log_returns", "Close")]
  data$Date <- as.Date(data$Date)
  na.omit(data)
  data
})


# Access the data for each file
data_ALTRI_SGPS <- data_list[[1]]
data_MOTA_ENGIL <- data_list[[2]]
data_REN <- data_list[[3]]
data_SONAE <- data_list[[4]]
data_THE_NAVIGATOR_COMP <- data_list[[5]]



acf(data_ALTRI_SGPS$log_returns, lag.max = 200, plot = TRUE)
pacf(data_ALTRI_SGPS$log_returns, lag.max = 200, plot = TRUE)



spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(0, 2)))
fit <- ugarchfit(spec, data = data_ALTRI_SGPS$log_returns)
fit

residuals <- residuals(fit)
acf(residuals)
pacf(residuals)










################################## Examination of the data ###################################



all_data <- rbind(
  data.frame(company = "ALTRI SGPS", data_ALTRI_SGPS),
  data.frame(company = "MOTA ENGIL", data_MOTA_ENGIL),
  data.frame(company = "REN", data_REN),
  data.frame(company = "SONAE", data_SONAE),
  data.frame(company = "THE NAVIGATOR COMP", data_THE_NAVIGATOR_COMP)
)

# Convert the "Date" column to a Date format
all_data$Date <- as.Date(all_data$Date)

# Plotting the log returns for all datasets using ggplot2
ggplot(all_data, aes(x = Date, y = log_returns, color = company)) +
  geom_line() +
  labs(x = "Date", y = "Log Returns", title = "Log Returns for Different Companies") +
  theme(legend.position = "bottom")



