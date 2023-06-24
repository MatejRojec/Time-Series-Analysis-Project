# Load the packages
library(gdata)
library(dplyr)
library(fGarch)
library(rugarch)
library(ggplot2)
library(stats)


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
  data <- data[,c("Date","log_returns")]
  data$Date <- as.Date(data$Date)
  na.omit(data)
})


# Access the data for each file
data_ALTRI_SGPS <- data_list[[1]]
data_MOTA_ENGIL <- data_list[[2]]
data_REN <- data_list[[3]]
data_SONAE <- data_list[[4]]
data_THE_NAVIGATOR_COMP <- data_list[[5]]



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

p2 <- ggplot(data_MOTA_ENGIL) 
p2 + geom_histogram(aes(x=log_returns, y=..density..), binwidth = 0.005, color="steelblue", fill="grey") +
  stat_function(fun = dnorm, args = list(mean = mean(data_MOTA_ENGIL$log_returns, na.rm = T), sd = sd(data_MOTA_ENGIL$log_returns, na.rm = T)), size=1)

p3 <- ggplot(data_ALTRI_SGPS) 
p3 + geom_histogram(aes(x=log_returns, y=..density..), binwidth = 0.005, color="steelblue", fill="grey") +
  stat_function(fun = dnorm, args = list(mean = mean(data_ALTRI_SGPS$log_returns, na.rm = T), sd = sd(data_ALTRI_SGPS$log_returns, na.rm = T)), size=1)

p4 <- ggplot(data_REN) 
p4 + geom_histogram(aes(x=log_returns, y=..density..), binwidth = 0.005, color="steelblue", fill="grey") +
  stat_function(fun = dnorm, args = list(mean = mean(data_REN$log_returns, na.rm = T), sd = sd(data_REN$log_returns, na.rm = T)), size=1)

p5 <- ggplot(data_THE_NAVIGATOR_COMP) 
p5 + geom_histogram(aes(x=log_returns, y=..density..), binwidth = 0.005, color="steelblue", fill="grey") +
  stat_function(fun = dnorm, args = list(mean = mean(data_THE_NAVIGATOR_COMP$log_returns, na.rm = T), sd = sd(data_THE_NAVIGATOR_COMP$log_returns, na.rm = T)), size=1)

p6 <- ggplot(data_SONAE) 
p6 + geom_histogram(aes(x=log_returns, y=..density..), binwidth = 0.005, color="steelblue", fill="grey") +
  stat_function(fun = dnorm, args = list(mean = mean(data_SONAE$log_returns, na.rm = T), sd = sd(data_SONAE$log_returns, na.rm = T)), size=1)


var(data_ALTRI_SGPS$log_returns) < 10^(-4) * 9
var(data_MOTA_ENGIL$log_returns) < 10^(-4) * 9 
var(data_REN$log_returns)  < 10^(-4) * 9
var(data_THE_NAVIGATOR_COMP$log_returns) < 10^(-4) * 9 
var(data_SONAE$log_returns)  < 10^(-4) * 9

mean(data_ALTRI_SGPS$log_returns) 
mean(data_MOTA_ENGIL$log_returns)  
mean(data_REN$log_returns)  
mean(data_THE_NAVIGATOR_COMP$log_returns) 
mean(data_SONAE$log_returns)

# acf of initial data 

acf(data_ALTRI_SGPS$log_returns, lag.max = 20, plot = TRUE,
    main = "The sample ACF for Altri-SGPS")
acf(data_MOTA_ENGIL$log_returns, lag.max = 20, plot = TRUE,
    main = "The sample ACF for Mota Engil")
acf(data_REN$log_returns, lag.max = 20, plot = TRUE,
    main = "The sample ACF for Ren")
acf(data_SONAE$log_returns, lag.max = 20, plot = TRUE,
    main = "The sample ACF for Sonae")
acf(data_ALTRI_SGPS$log_returns, lag.max = 20, plot = TRUE,
    main = "The sample ACF for The Navigator Company")

# acf of abs. values

acf((data_ALTRI_SGPS$log_returns)^2, lag.max = 400, plot = TRUE,
    main = "The sample ACF for the squares of Altri-SGPS")
acf((data_MOTA_ENGIL$log_returns)^2, lag.max = 400, plot = TRUE,
    main = "The sample ACF for the squares of Mota Engil")
acf((data_REN$log_returns)^2, lag.max = 400, plot = TRUE,
    main = "The sample ACF for the squares of Ren")
acf((data_SONAE$log_returns)^2, lag.max = 400, plot = TRUE,
    main = "The sample ACF for the squares of Sonae")
acf((data_ALTRI_SGPS$log_returns)^2, lag.max = 400, plot = TRUE,
    main = "The sample ACF for the squares of The Navigator Company")





####################  The Navigator Company time series #################


log_returns_THE_NAVIGATOR_COMP <- as.ts(data_THE_NAVIGATOR_COMP$log_returns)

# ARCH

summary(garchFit(~garch(1,0), data = "log_returns_THE_NAVIGATOR_COMP", trace = FALSE))
summary(garchFit(~garch(2,0), data = "log_returns_THE_NAVIGATOR_COMP", trace = FALSE))
## alpha 3 important
summary(garchFit(~garch(3,0), data = "log_returns_THE_NAVIGATOR_COMP", trace = FALSE))

model_g_3_0 = garchFit(~garch(3,0), data = "log_returns_THE_NAVIGATOR_COMP", trace = FALSE)
residuals_g_3_0 <- residuals(model_g_3_0)
plot(residuals_g_3_0 / sqrt(var(residuals_g_3_0)), type = "l", ylab = "Residuals", main = "Residual Plot for ARCH(3)")

mean(residuals_g_3_0 / sqrt(var(residuals_g_3_0)))
var(residuals_g_3_0 / sqrt(var(residuals_g_3_0)))

acf(residuals_g_3_0, main = "ACF function of residual from model ARCH(3)")

qqnorm(residuals_g_3_0, main = "Normal Q-Q Plot for residual from model ARCH(3)")
qqline(residuals_g_3_0)

summary(garchFit(~garch(4,0), data = "log_returns_THE_NAVIGATOR_COMP", trace = FALSE))
summary(garchFit(~garch(5,0), data = "log_returns_THE_NAVIGATOR_COMP", trace = FALSE))
summary(garchFit(~garch(6,0), data = "log_returns_THE_NAVIGATOR_COMP", trace = FALSE))
summary(garchFit(~garch(7,0), data = "log_returns_THE_NAVIGATOR_COMP", trace = FALSE))
summary(garchFit(~garch(8,0), data = "log_returns_THE_NAVIGATOR_COMP", trace = FALSE))
summary(garchFit(~garch(9,0), data = "log_returns_THE_NAVIGATOR_COMP", trace = FALSE))
summary(garchFit(~garch(10,0), data = "log_returns_THE_NAVIGATOR_COMP", trace = FALSE))
summary(garchFit(~garch(15,0), data = "log_returns_THE_NAVIGATOR_COMP", trace = FALSE))
summary(garchFit(~garch(20,0), data = "log_returns_THE_NAVIGATOR_COMP", trace = FALSE))
summary(garchFit(~garch(40,0), data = "log_returns_THE_NAVIGATOR_COMP", trace = FALSE))


# GARCH

summary(garchFit(~garch(1,1), data = "log_returns_THE_NAVIGATOR_COMP", trace = FALSE))
summary(garchFit(~garch(3,1), data = "log_returns_THE_NAVIGATOR_COMP", trace = FALSE))
summary(garchFit(~garch(1,2), data = "log_returns_THE_NAVIGATOR_COMP", trace = FALSE))
summary(garchFit(~garch(3,2), data = "log_returns_THE_NAVIGATOR_COMP", trace = FALSE))
## alpha 3 important
summary(garchFit(~garch(1,3), data = "log_returns_THE_NAVIGATOR_COMP", trace = FALSE))
summary(garchFit(~garch(3,3), data = "log_returns_THE_NAVIGATOR_COMP", trace = FALSE))
summary(garchFit(~garch(1,4), data = "log_returns_THE_NAVIGATOR_COMP", trace = FALSE))
summary(garchFit(~garch(3,4), data = "log_returns_THE_NAVIGATOR_COMP", trace = FALSE))
summary(garchFit(~garch(1,5), data = "log_returns_THE_NAVIGATOR_COMP", trace = FALSE))
summary(garchFit(~garch(3,5), data = "log_returns_THE_NAVIGATOR_COMP", trace = FALSE))
summary(garchFit(~garch(3,6), data = "log_returns_THE_NAVIGATOR_COMP", trace = FALSE))
summary(garchFit(~garch(3,7), data = "log_returns_THE_NAVIGATOR_COMP", trace = FALSE))
summary(garchFit(~garch(3,8), data = "log_returns_THE_NAVIGATOR_COMP", trace = FALSE))
summary(garchFit(~garch(3,9), data = "log_returns_THE_NAVIGATOR_COMP", trace = FALSE))
summary(garchFit(~garch(3,10), data = "log_returns_THE_NAVIGATOR_COMP", trace = FALSE))
summary(garchFit(~garch(3,15), data = "log_returns_THE_NAVIGATOR_COMP", trace = FALSE))
summary(garchFit(~garch(3,20), data = "log_returns_THE_NAVIGATOR_COMP", trace = FALSE))
summary(garchFit(~garch(3,40), data = "log_returns_THE_NAVIGATOR_COMP", trace = FALSE))


# IGARCH - selected models

model_IG_11 = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0), include.mean=TRUE), distribution.model="norm")
fit_IG_11= ugarchfit(model_IG_11, log_returns_THE_NAVIGATOR_COMP)
fit_IG_11
# -5.3812

model_IG_31 = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(3,1)), mean.model=list(armaOrder=c(0,0), include.mean=TRUE), distribution.model="norm")
fit_IG_31= ugarchfit(model_IG_31, log_returns_THE_NAVIGATOR_COMP)
fit_IG_31
# -5.3607


model_IG_33 = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(3,3)), mean.model=list(armaOrder=c(0,0), include.mean=TRUE), distribution.model="norm")
fit_IG_33= ugarchfit(model_IG_33, log_returns_THE_NAVIGATOR_COMP)
fit_IG_33
# -5.3408


# GARCH-M - selected models


model_M_11 = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0), include.mean=TRUE, archpow = 2),
                   distribution.model="norm")

fit_M_11= ugarchfit(model_M_11, log_returns_THE_NAVIGATOR_COMP)
fit_M_11


model_M_31 = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(3,1)), mean.model=list(armaOrder=c(0,0), include.mean=TRUE, archpow = 2),
                        distribution.model="norm")

fit_M_31= ugarchfit(model_M_31, log_returns_THE_NAVIGATOR_COMP)
fit_M_31


model_M_33 = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(3,3)), mean.model=list(armaOrder=c(0,0), include.mean=TRUE, archpow = 2),
                        distribution.model="norm")

fit_M_33= ugarchfit(model_M_33, log_returns_THE_NAVIGATOR_COMP)
fit_M_33

model_M_11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder=c(1,1)),
                   mean.model = list(armaOrder = c(0, 0)))

fit_M_11 <- ugarchfit(model_M_11, data = log_returns_THE_NAVIGATOR_COMP)
fit_M_11



model_M_31 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder=c(3,1)),
                         mean.model = list(armaOrder = c(0, 0)))

fit_M_31 <- ugarchfit(model_M_31, data = log_returns_THE_NAVIGATOR_COMP)
fit_M_31
# -5.3512

model_M_33 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder=c(0,1)),
                         mean.model = list(armaOrder = c(0, 0)))

fit_M_33 <- ugarchfit(model_M_33, data = log_returns_THE_NAVIGATOR_COMP)
fit_M_33


## apARCH

model_A_11 <- ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(1, 1)),
                                 mean.model = list(armaOrder = c(0, 0)),
                                 distribution.model = "std")


fit_A_11 <- ugarchfit(model_A_11, data = log_returns_THE_NAVIGATOR_COMP)
fit_A_11

residuals_A_11 <- fit_A_11@fit$residuals
plot(residuals_A_11 / sqrt(var(residuals_A_11)), type = "l", ylab = "Residuals", main = "Residual Plot for apARCH(1, 1)")

mean(residuals_A_11 / sqrt(var(residuals_A_11)))
var(residuals_A_11 / sqrt(var(residuals_A_11)))

acf(residuals_A_11, main = "ACF function of residual from model apARCH(1, 1)")

qqnorm(residuals_A_11, main = "Normal Q-Q Plot for residual from model apARCH(1, 1)")
qqline(residuals_A_11)

Box.test(residuals_A_11, type = "Ljung-Box")


# -5.4630

model_A_31 <- ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(3, 1)),
                         mean.model = list(armaOrder = c(0, 0)),
                         distribution.model = "std")


fit_A_31 <- ugarchfit(model_A_31, data = log_returns_THE_NAVIGATOR_COMP)
fit_A_31

#  -5.3855

model_A_13 <- ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(1, 3)),
                         mean.model = list(armaOrder = c(0, 0)),
                         distribution.model = "std")


fit_A_13 <- ugarchfit(model_A_13, data = log_returns_THE_NAVIGATOR_COMP)
fit_A_13
# -5.3889


model_A_12 <- ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(1, 2)),
                         mean.model = list(armaOrder = c(0, 0)),
                         distribution.model = "std")


fit_A_12 <- ugarchfit(model_A_12, data = log_returns_THE_NAVIGATOR_COMP)
fit_A_12




####################  Mota Engil time series #################


log_returns_MOTA_ENGIL <- as.ts(data_MOTA_ENGIL$log_returns)

# ARCH

summary(garchFit(~garch(1,0), data = "log_returns_MOTA_ENGIL", trace = FALSE))
summary(garchFit(~garch(2,0), data = "log_returns_MOTA_ENGIL", trace = FALSE))
summary(garchFit(~garch(3,0), data = "log_returns_MOTA_ENGIL", trace = FALSE))
summary(garchFit(~garch(4,0), data = "log_returns_MOTA_ENGIL", trace = FALSE))
summary(garchFit(~garch(5,0), data = "log_returns_MOTA_ENGIL", trace = FALSE))
summary(garchFit(~garch(6,0), data = "log_returns_MOTA_ENGIL", trace = FALSE))
summary(garchFit(~garch(7,0), data = "log_returns_MOTA_ENGIL", trace = FALSE))
summary(garchFit(~garch(8,0), data = "log_returns_MOTA_ENGIL", trace = FALSE))

model_g_8_0 = garchFit(~garch(8,0), data = "log_returns_MOTA_ENGIL", trace = FALSE)
residuals_g_8_0 <- residuals(model_g_8_0)
plot(residuals_g_8_0 / sqrt(var(residuals_g_8_0)), type = "l", ylab = "Residuals", main = "Residual Plot for ARCH(3)")

mean(residuals_g_8_0 / sqrt(var(residuals_g_8_0)))
var(residuals_g_8_0 / sqrt(var(residuals_g_8_0)))

acf(residuals_g_8_0, main = "ACF function of residual from model ARCH(3)")

qqnorm(residuals_g_8_0, main = "Normal Q-Q Plot for residual from model ARCH(3)")
qqline(residuals_g_8_0)


summary(garchFit(~garch(9,0), data = "log_returns_MOTA_ENGIL", trace = FALSE))
summary(garchFit(~garch(10,0), data = "log_returns_MOTA_ENGIL", trace = FALSE))
summary(garchFit(~garch(15,0), data = "log_returns_MOTA_ENGIL", trace = FALSE))
summary(garchFit(~garch(20,0), data = "log_returns_MOTA_ENGIL", trace = FALSE))
summary(garchFit(~garch(40,0), data = "log_returns_MOTA_ENGIL", trace = FALSE))


# GARCH

summary(garchFit(~garch(1,1), data = "log_returns_MOTA_ENGIL", trace = FALSE))
summary(garchFit(~garch(8,1), data = "log_returns_MOTA_ENGIL", trace = FALSE))
summary(garchFit(~garch(1,2), data = "log_returns_MOTA_ENGIL", trace = FALSE))
summary(garchFit(~garch(3,2), data = "log_returns_MOTA_ENGIL", trace = FALSE))
## alpha 3 important
summary(garchFit(~garch(1,3), data = "log_returns_MOTA_ENGIL", trace = FALSE))
summary(garchFit(~garch(8,3), data = "log_returns_MOTA_ENGIL", trace = FALSE))
summary(garchFit(~garch(1,4), data = "log_returns_MOTA_ENGIL", trace = FALSE))
summary(garchFit(~garch(8,4), data = "log_returns_MOTA_ENGIL", trace = FALSE))
summary(garchFit(~garch(1,5), data = "log_returns_MOTA_ENGIL", trace = FALSE))
summary(garchFit(~garch(8,5), data = "log_returns_MOTA_ENGIL", trace = FALSE))
summary(garchFit(~garch(8,6), data = "log_returns_MOTA_ENGIL", trace = FALSE))
summary(garchFit(~garch(8,7), data = "log_returns_MOTA_ENGIL", trace = FALSE))
summary(garchFit(~garch(8,8), data = "log_returns_MOTA_ENGIL", trace = FALSE))
summary(garchFit(~garch(8,9), data = "log_returns_MOTA_ENGIL", trace = FALSE))
summary(garchFit(~garch(8,10), data = "log_returns_MOTA_ENGIL", trace = FALSE))
summary(garchFit(~garch(8,15), data = "log_returns_MOTA_ENGIL", trace = FALSE))
summary(garchFit(~garch(8,20), data = "log_returns_MOTA_ENGIL", trace = FALSE))
summary(garchFit(~garch(8,40), data = "log_returns_MOTA_ENGIL", trace = FALSE))


# IGARCH - selected models

model_IG_11 = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0), include.mean=TRUE), distribution.model="norm")
fit_IG_11= ugarchfit(model_IG_11, log_returns_MOTA_ENGIL)
fit_IG_11

model_IG_81 = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(8,1)), mean.model=list(armaOrder=c(0,0), include.mean=TRUE), distribution.model="norm")
fit_IG_81= ugarchfit(model_IG_81, log_returns_MOTA_ENGIL)
fit_IG_31

model_IG_88 = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(8,8)), mean.model=list(armaOrder=c(0,0), include.mean=TRUE), distribution.model="norm")
fit_IG_88= ugarchfit(model_IG_88, log_returns_MOTA_ENGIL)
fit_IG_88


# GARCH-M - selected models


model_M_11 = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0), include.mean=TRUE, archpow = 2),
                        distribution.model="norm")

fit_M_11= ugarchfit(model_M_11, log_returns_MOTA_ENGIL)
fit_M_11


model_M_81 = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(8,1)), mean.model=list(armaOrder=c(0,0), include.mean=TRUE, archpow = 2),
                        distribution.model="norm")

fit_M_81= ugarchfit(model_M_81, log_returns_MOTA_ENGIL)
fit_M_81


model_M_88 = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(3,3)), mean.model=list(armaOrder=c(0,0), include.mean=TRUE, archpow = 2),
                        distribution.model="norm")

fit_M_88= ugarchfit(model_M_88, log_returns_MOTA_ENGIL)
fit_M_88



## apARCH

model_A_11 <- ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(0, 0)),
                         distribution.model = "std")


fit_A_11 <- ugarchfit(model_A_11, data = log_returns_MOTA_ENGIL)
fit_A_11

residuals_A_11 <- fit_A_11@fit$residuals
plot(residuals_A_11 / sqrt(var(residuals_A_11)), type = "l", ylab = "Residuals", main = "Residual Plot for apARCH(1, 1)")

mean(residuals_A_11 / sqrt(var(residuals_A_11)))
var(residuals_A_11 / sqrt(var(residuals_A_11)))

acf(residuals_A_11, main = "ACF function of residual from model apARCH(1, 1)")

qqnorm(residuals_A_11, main = "Normal Q-Q Plot for residual from model apARCH(1, 1)")
qqline(residuals_A_11)

Box.test(residuals_A_11, type = "Ljung-Box")


model_A_81 <- ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(8, 1)),
                         mean.model = list(armaOrder = c(0, 0)),
                         distribution.model = "std")


fit_A_81 <- ugarchfit(model_A_81, data = log_returns_MOTA_ENGIL)
fit_A_81


model_A_18 <- ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(1, 8)),
                         mean.model = list(armaOrder = c(0, 0)),
                         distribution.model = "std")


fit_A_18 <- ugarchfit(model_A_18, data = log_returns_MOTA_ENGIL)
fit_A_18


model_A_12 <- ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(1, 2)),
                         mean.model = list(armaOrder = c(0, 0)),
                         distribution.model = "std")


fit_A_12 <- ugarchfit(model_A_12, data = log_returns_MOTA_ENGIL)
fit_A_12





####################  Sonae time series #################


log_returns_SONAE <- as.ts(data_SONAE$log_returns)

# ARCH

summary(garchFit(~garch(1,0), data = "log_returns_SONAE", trace = FALSE))

model_g_1_0 = garchFit(~garch(3,0), data = "log_returns_SONAE", trace = FALSE)
residuals_g_1_0 <- residuals(model_g_1_0)
plot(residuals_g_1_0 / sqrt(var(residuals_g_1_0)), type = "l", ylab = "Residuals", main = "Residual Plot for ARCH(3)")

mean(residuals_g_1_0 / sqrt(var(residuals_g_1_0)))
var(residuals_g_1_0 / sqrt(var(residuals_g_1_0)))

acf(residuals_g_1_0, main = "ACF function of residual from model ARCH(3)")

qqnorm(residuals_g_1_0, main = "Normal Q-Q Plot for residual from model ARCH(3)")
qqline(residuals_g_1_0)


summary(garchFit(~garch(2,0), data = "log_returns_SONAE", trace = FALSE))
summary(garchFit(~garch(3,0), data = "log_returns_SONAE", trace = FALSE))
summary(garchFit(~garch(4,0), data = "log_returns_SONAE", trace = FALSE))
summary(garchFit(~garch(5,0), data = "log_returns_SONAE", trace = FALSE))
summary(garchFit(~garch(6,0), data = "log_returns_SONAE", trace = FALSE))
summary(garchFit(~garch(7,0), data = "log_returns_SONAE", trace = FALSE))
summary(garchFit(~garch(8,0), data = "log_returns_SONAE", trace = FALSE))
summary(garchFit(~garch(9,0), data = "log_returns_SONAE", trace = FALSE))
summary(garchFit(~garch(10,0), data = "log_returns_SONAE", trace = FALSE))
summary(garchFit(~garch(15,0), data = "log_returns_SONAE", trace = FALSE))
summary(garchFit(~garch(20,0), data = "log_returns_SONAE", trace = FALSE))
summary(garchFit(~garch(40,0), data = "log_returns_SONAE", trace = FALSE))


# GARCH

summary(garchFit(~garch(1,1), data = "log_returns_SONAE", trace = FALSE))
summary(garchFit(~garch(3,1), data = "log_returns_SONAE", trace = FALSE))
summary(garchFit(~garch(1,2), data = "log_returns_SONAE", trace = FALSE))
summary(garchFit(~garch(3,2), data = "log_returns_SONAE", trace = FALSE))
## alpha 3 important
summary(garchFit(~garch(1,3), data = "log_returns_SONAE", trace = FALSE))
summary(garchFit(~garch(3,3), data = "log_returns_SONAE", trace = FALSE))
summary(garchFit(~garch(1,4), data = "log_returns_SONAE", trace = FALSE))
summary(garchFit(~garch(3,4), data = "log_returns_SONAE", trace = FALSE))
summary(garchFit(~garch(1,5), data = "log_returns_SONAE", trace = FALSE))
summary(garchFit(~garch(3,5), data = "log_returns_SONAE", trace = FALSE))
summary(garchFit(~garch(3,6), data = "log_returns_SONAE", trace = FALSE))
summary(garchFit(~garch(3,7), data = "log_returns_SONAE", trace = FALSE))
summary(garchFit(~garch(3,8), data = "log_returns_SONAE", trace = FALSE))
summary(garchFit(~garch(3,9), data = "log_returns_SONAE", trace = FALSE))
summary(garchFit(~garch(3,10), data = "log_returns_SONAE", trace = FALSE))
summary(garchFit(~garch(3,15), data = "log_returns_SONAE", trace = FALSE))
summary(garchFit(~garch(3,20), data = "log_returns_SONAE", trace = FALSE))
summary(garchFit(~garch(3,40), data = "log_returns_SONAE", trace = FALSE))


# IGARCH - selected models

model_IG_11 = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0), include.mean=TRUE), distribution.model="norm")
fit_IG_11= ugarchfit(model_IG_11, log_returns_SONAE)
fit_IG_11

model_IG_31 = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(3,1)), mean.model=list(armaOrder=c(0,0), include.mean=TRUE), distribution.model="norm")
fit_IG_31= ugarchfit(model_IG_31, log_returns_SONAE)
fit_IG_31


model_IG_33 = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(3,3)), mean.model=list(armaOrder=c(0,0), include.mean=TRUE), distribution.model="norm")
fit_IG_33= ugarchfit(model_IG_33, log_returns_SONAE)
fit_IG_33


# GARCH-M - selected models


model_M_11 = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0), include.mean=TRUE, archpow = 2),
                        distribution.model="norm")

fit_M_11= ugarchfit(model_M_11, log_returns_SONAE)
fit_M_11


model_M_31 = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(3,1)), mean.model=list(armaOrder=c(0,0), include.mean=TRUE, archpow = 2),
                        distribution.model="norm")

fit_M_31= ugarchfit(model_M_31, log_returns_SONAE)
fit_M_31


model_M_33 = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(3,3)), mean.model=list(armaOrder=c(0,0), include.mean=TRUE, archpow = 2),
                        distribution.model="norm")

fit_M_33= ugarchfit(model_M_33, log_returns_SONAE)
fit_M_33

model_M_11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder=c(1,1)),
                         mean.model = list(armaOrder = c(0, 0)))

fit_M_11 <- ugarchfit(model_M_11, data = log_returns_SONAE)
fit_M_11



model_M_31 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder=c(3,1)),
                         mean.model = list(armaOrder = c(0, 0)))

fit_M_31 <- ugarchfit(model_M_31, data = log_returns_SONAE)
fit_M_31

model_M_33 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder=c(0,1)),
                         mean.model = list(armaOrder = c(0, 0)))

fit_M_33 <- ugarchfit(model_M_33, data = log_returns_SONAE)
fit_M_33


## apARCH

model_A_11 <- ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(0, 0)),
                         distribution.model = "std")


fit_A_11 <- ugarchfit(model_A_11, data = log_returns_SONAE)
fit_A_11

residuals_A_11 <- fit_A_11@fit$residuals
plot(residuals_A_11 / sqrt(var(residuals_A_11)), type = "l", ylab = "Residuals", main = "Residual Plot for apARCH(1, 1)")

mean(residuals_A_11 / sqrt(var(residuals_A_11)))
var(residuals_A_11 / sqrt(var(residuals_A_11)))

acf(residuals_A_11, main = "ACF function of residual from model apARCH(1, 1)")

qqnorm(residuals_A_11, main = "Normal Q-Q Plot for residual from model apARCH(1, 1)")
qqline(residuals_A_11)

Box.test(residuals_A_11, type = "Ljung-Box")


model_A_31 <- ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(3, 1)),
                         mean.model = list(armaOrder = c(0, 0)),
                         distribution.model = "std")


fit_A_31 <- ugarchfit(model_A_31, data = log_returns_SONAE)
fit_A_31


model_A_13 <- ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(1, 3)),
                         mean.model = list(armaOrder = c(0, 0)),
                         distribution.model = "std")


fit_A_13 <- ugarchfit(model_A_13, data = log_returns_SONAE)
fit_A_13


model_A_12 <- ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(1, 2)),
                         mean.model = list(armaOrder = c(0, 0)),
                         distribution.model = "std")


fit_A_12 <- ugarchfit(model_A_12, data = log_returns_SONAE)
fit_A_12


####################  Ren time series #################


log_returns_REN <- as.ts(data_REN$log_returns)

# ARCH

summary(garchFit(~garch(1,0), data = "log_returns_REN", trace = FALSE))

model_g_1_0 = garchFit(~garch(3,0), data = "log_returns_REN", trace = FALSE)
residuals_g_1_0 <- residuals(model_g_1_0)
plot(residuals_g_1_0 / sqrt(var(residuals_g_1_0)), type = "l", ylab = "Residuals", main = "Residual Plot for ARCH(3)")

mean(residuals_g_1_0 / sqrt(var(residuals_g_1_0)))
var(residuals_g_1_0 / sqrt(var(residuals_g_1_0)))

acf(residuals_g_1_0, main = "ACF function of residual from model ARCH(3)")

qqnorm(residuals_g_1_0, main = "Normal Q-Q Plot for residual from model ARCH(3)")
qqline(residuals_g_1_0)


summary(garchFit(~garch(2,0), data = "log_returns_REN", trace = FALSE))
summary(garchFit(~garch(3,0), data = "log_returns_REN", trace = FALSE))
summary(garchFit(~garch(4,0), data = "log_returns_REN", trace = FALSE))
summary(garchFit(~garch(5,0), data = "log_returns_REN", trace = FALSE))
summary(garchFit(~garch(6,0), data = "log_returns_REN", trace = FALSE))
summary(garchFit(~garch(7,0), data = "log_returns_REN", trace = FALSE))
summary(garchFit(~garch(8,0), data = "log_returns_REN", trace = FALSE))
summary(garchFit(~garch(9,0), data = "log_returns_REN", trace = FALSE))
summary(garchFit(~garch(10,0), data = "log_returns_REN", trace = FALSE))
summary(garchFit(~garch(15,0), data = "log_returns_REN", trace = FALSE))
summary(garchFit(~garch(20,0), data = "log_returns_REN", trace = FALSE))
summary(garchFit(~garch(40,0), data = "log_returns_REN", trace = FALSE))


# GARCH

summary(garchFit(~garch(1,1), data = "log_returns_REN", trace = FALSE))
summary(garchFit(~garch(3,1), data = "log_returns_REN", trace = FALSE))
summary(garchFit(~garch(1,2), data = "log_returns_REN", trace = FALSE))
summary(garchFit(~garch(3,2), data = "log_returns_REN", trace = FALSE))
## alpha 3 important
summary(garchFit(~garch(1,3), data = "log_returns_REN", trace = FALSE))
summary(garchFit(~garch(3,3), data = "log_returns_REN", trace = FALSE))
summary(garchFit(~garch(1,4), data = "log_returns_REN", trace = FALSE))
summary(garchFit(~garch(3,4), data = "log_returns_REN", trace = FALSE))
summary(garchFit(~garch(1,5), data = "log_returns_REN", trace = FALSE))
summary(garchFit(~garch(3,5), data = "log_returns_REN", trace = FALSE))
summary(garchFit(~garch(3,6), data = "log_returns_REN", trace = FALSE))
summary(garchFit(~garch(3,7), data = "log_returns_REN", trace = FALSE))
summary(garchFit(~garch(3,8), data = "log_returns_REN", trace = FALSE))
summary(garchFit(~garch(3,9), data = "log_returns_REN", trace = FALSE))
summary(garchFit(~garch(3,10), data = "log_returns_REN", trace = FALSE))
summary(garchFit(~garch(3,15), data = "log_returns_REN", trace = FALSE))
summary(garchFit(~garch(3,20), data = "log_returns_REN", trace = FALSE))
summary(garchFit(~garch(3,40), data = "log_returns_REN", trace = FALSE))


# IGARCH - selected models

model_IG_11 = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0), include.mean=TRUE), distribution.model="norm")
fit_IG_11= ugarchfit(model_IG_11, log_returns_REN)
fit_IG_11

model_IG_31 = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(3,1)), mean.model=list(armaOrder=c(0,0), include.mean=TRUE), distribution.model="norm")
fit_IG_31= ugarchfit(model_IG_31, log_returns_REN)
fit_IG_31


model_IG_33 = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(3,3)), mean.model=list(armaOrder=c(0,0), include.mean=TRUE), distribution.model="norm")
fit_IG_33= ugarchfit(model_IG_33, log_returns_REN)
fit_IG_33


# GARCH-M - selected models


model_M_11 = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0), include.mean=TRUE, archpow = 2),
                        distribution.model="norm")

fit_M_11= ugarchfit(model_M_11, log_returns_REN)
fit_M_11


model_M_31 = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(3,1)), mean.model=list(armaOrder=c(0,0), include.mean=TRUE, archpow = 2),
                        distribution.model="norm")

fit_M_31= ugarchfit(model_M_31, log_returns_REN)
fit_M_31


model_M_33 = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(3,3)), mean.model=list(armaOrder=c(0,0), include.mean=TRUE, archpow = 2),
                        distribution.model="norm")

fit_M_33= ugarchfit(model_M_33, log_returns_REN)
fit_M_33

model_M_11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder=c(1,1)),
                         mean.model = list(armaOrder = c(0, 0)))

fit_M_11 <- ugarchfit(model_M_11, data = log_returns_REN)
fit_M_11



model_M_31 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder=c(3,1)),
                         mean.model = list(armaOrder = c(0, 0)))

fit_M_31 <- ugarchfit(model_M_31, data = log_returns_REN)
fit_M_31

model_M_33 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder=c(0,1)),
                         mean.model = list(armaOrder = c(0, 0)))

fit_M_33 <- ugarchfit(model_M_33, data = log_returns_REN)
fit_M_33


## apARCH

model_A_11 <- ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(0, 0)),
                         distribution.model = "std")


fit_A_11 <- ugarchfit(model_A_11, data = log_returns_REN)
fit_A_11

qqnorm(residuals_A_11, main = "Normal Q-Q Plot for residual from model apARCH(1, 1)")
qqline(residuals_A_11)

Box.test(residuals_A_11, type = "Ljung-Box")


model_A_31 <- ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(3, 1)),
                         mean.model = list(armaOrder = c(0, 0)),
                         distribution.model = "std")


fit_A_31 <- ugarchfit(model_A_31, data = log_returns_REN)
fit_A_31


model_A_13 <- ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(1, 3)),
                         mean.model = list(armaOrder = c(0, 0)),
                         distribution.model = "std")


fit_A_13 <- ugarchfit(model_A_13, data = log_returns_REN)
fit_A_13


model_A_12 <- ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(1, 2)),
                         mean.model = list(armaOrder = c(0, 0)),
                         distribution.model = "std")


fit_A_12 <- ugarchfit(model_A_12, data = log_returns_REN)
fit_A_12


residuals_A_12 <- fit_A_12@fit$residuals
plot(residuals_A_12 / sqrt(var(residuals_A_12)), type = "l", ylab = "Residuals", main = "Residual Plot for apARCH(1, 1)")

mean(residuals_A_12 / sqrt(var(residuals_A_12)))
var(residuals_A_12 / sqrt(var(residuals_A_12)))

acf(residuals_A_12, main = "ACF function of residual from model apARCH(1, 1)")


####################  Altri-SGPS time series #################


log_returns_ALTRI_SGPS <- as.ts(data_ALTRI_SGPS$log_returns)

# ARCH

summary(garchFit(~garch(1,0), data = "log_returns_ALTRI_SGPS", trace = FALSE))

model_g_1_0 = garchFit(~garch(3,0), data = "log_returns_ALTRI_SGPS", trace = FALSE)
residuals_g_1_0 <- residuals(model_g_1_0)
plot(residuals_g_1_0 / sqrt(var(residuals_g_1_0)), type = "l", ylab = "Residuals", main = "Residual Plot for ARCH(3)")

mean(residuals_g_1_0 / sqrt(var(residuals_g_1_0)))
var(residuals_g_1_0 / sqrt(var(residuals_g_1_0)))

acf(residuals_g_1_0, main = "ACF function of residual from model ARCH(3)")

qqnorm(residuals_g_1_0, main = "Normal Q-Q Plot for residual from model ARCH(3)")
qqline(residuals_g_1_0)


summary(garchFit(~garch(2,0), data = "log_returns_ALTRI_SGPS", trace = FALSE))
summary(garchFit(~garch(3,0), data = "log_returns_ALTRI_SGPS", trace = FALSE))
summary(garchFit(~garch(4,0), data = "log_returns_ALTRI_SGPS", trace = FALSE))
summary(garchFit(~garch(5,0), data = "log_returns_ALTRI_SGPS", trace = FALSE))
summary(garchFit(~garch(6,0), data = "log_returns_ALTRI_SGPS", trace = FALSE))
summary(garchFit(~garch(7,0), data = "log_returns_ALTRI_SGPS", trace = FALSE))
summary(garchFit(~garch(8,0), data = "log_returns_ALTRI_SGPS", trace = FALSE))
summary(garchFit(~garch(9,0), data = "log_returns_ALTRI_SGPS", trace = FALSE))
summary(garchFit(~garch(10,0), data = "log_returns_ALTRI_SGPS", trace = FALSE))
summary(garchFit(~garch(15,0), data = "log_returns_ALTRI_SGPS", trace = FALSE))
summary(garchFit(~garch(20,0), data = "log_returns_ALTRI_SGPS", trace = FALSE))
summary(garchFit(~garch(40,0), data = "log_returns_ALTRI_SGPS", trace = FALSE))


# GARCH

summary(garchFit(~garch(1,1), data = "log_returns_ALTRI_SGPS", trace = FALSE))
summary(garchFit(~garch(3,1), data = "log_returns_ALTRI_SGPS", trace = FALSE))
summary(garchFit(~garch(1,2), data = "log_returns_ALTRI_SGPS", trace = FALSE))
summary(garchFit(~garch(3,2), data = "log_returns_ALTRI_SGPS", trace = FALSE))
## alpha 3 important
summary(garchFit(~garch(1,3), data = "log_returns_ALTRI_SGPS", trace = FALSE))
summary(garchFit(~garch(3,3), data = "log_returns_ALTRI_SGPS", trace = FALSE))
summary(garchFit(~garch(1,4), data = "log_returns_ALTRI_SGPS", trace = FALSE))
summary(garchFit(~garch(3,4), data = "log_returns_ALTRI_SGPS", trace = FALSE))
summary(garchFit(~garch(1,5), data = "log_returns_ALTRI_SGPS", trace = FALSE))
summary(garchFit(~garch(3,5), data = "log_returns_ALTRI_SGPS", trace = FALSE))
summary(garchFit(~garch(3,6), data = "log_returns_ALTRI_SGPS", trace = FALSE))
summary(garchFit(~garch(3,7), data = "log_returns_ALTRI_SGPS", trace = FALSE))
summary(garchFit(~garch(3,8), data = "log_returns_ALTRI_SGPS", trace = FALSE))
summary(garchFit(~garch(3,9), data = "log_returns_ALTRI_SGPS", trace = FALSE))
summary(garchFit(~garch(3,10), data = "log_returns_ALTRI_SGPS", trace = FALSE))
summary(garchFit(~garch(3,15), data = "log_returns_ALTRI_SGPS", trace = FALSE))
summary(garchFit(~garch(3,20), data = "log_returns_ALTRI_SGPS", trace = FALSE))
summary(garchFit(~garch(3,40), data = "log_returns_ALTRI_SGPS", trace = FALSE))


# IGARCH - selected models

model_IG_11 = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0), include.mean=TRUE), distribution.model="norm")
fit_IG_11= ugarchfit(model_IG_11, log_returns_ALTRI_SGPS)
fit_IG_11

model_IG_31 = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(3,1)), mean.model=list(armaOrder=c(0,0), include.mean=TRUE), distribution.model="norm")
fit_IG_31= ugarchfit(model_IG_31, log_returns_ALTRI_SGPS)
fit_IG_31


model_IG_33 = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(3,3)), mean.model=list(armaOrder=c(0,0), include.mean=TRUE), distribution.model="norm")
fit_IG_33= ugarchfit(model_IG_33, log_returns_ALTRI_SGPS)
fit_IG_33


# GARCH-M - selected models


model_M_11 = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0), include.mean=TRUE, archpow = 2),
                        distribution.model="norm")

fit_M_11= ugarchfit(model_M_11, log_returns_ALTRI_SGPS)
fit_M_11


model_M_31 = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(3,1)), mean.model=list(armaOrder=c(0,0), include.mean=TRUE, archpow = 2),
                        distribution.model="norm")

fit_M_31= ugarchfit(model_M_31, log_returns_ALTRI_SGPS)
fit_M_31


model_M_33 = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(3,3)), mean.model=list(armaOrder=c(0,0), include.mean=TRUE, archpow = 2),
                        distribution.model="norm")

fit_M_33= ugarchfit(model_M_33, log_returns_ALTRI_SGPS)
fit_M_33

model_M_11 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder=c(1,1)),
                         mean.model = list(armaOrder = c(0, 0)))

fit_M_11 <- ugarchfit(model_M_11, data = log_returns_ALTRI_SGPS)
fit_M_11



model_M_31 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder=c(3,1)),
                         mean.model = list(armaOrder = c(0, 0)))

fit_M_31 <- ugarchfit(model_M_31, data = log_returns_ALTRI_SGPS)
fit_M_31

model_M_33 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder=c(0,1)),
                         mean.model = list(armaOrder = c(0, 0)))

fit_M_33 <- ugarchfit(model_M_33, data = log_returns_ALTRI_SGPS)
fit_M_33


## apARCH

model_A_11 <- ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(1, 1)),
                         mean.model = list(armaOrder = c(0, 0)),
                         distribution.model = "std")


fit_A_11 <- ugarchfit(model_A_11, data = log_returns_ALTRI_SGPS)
fit_A_11

residuals_A_11 <- fit_A_11@fit$residuals
plot(residuals_A_11 / sqrt(var(residuals_A_11)), type = "l", ylab = "Residuals", main = "Residual Plot for apARCH(1, 1)")

mean(residuals_A_11 / sqrt(var(residuals_A_11)))
var(residuals_A_11 / sqrt(var(residuals_A_11)))

acf(residuals_A_11, main = "ACF function of residual from model apARCH(1, 1)")

qqnorm(residuals_A_11, main = "Normal Q-Q Plot for residual from model apARCH(1, 1)")
qqline(residuals_A_11)

Box.test(residuals_A_11, type = "Ljung-Box")


model_A_31 <- ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(3, 1)),
                         mean.model = list(armaOrder = c(0, 0)),
                         distribution.model = "std")


fit_A_31 <- ugarchfit(model_A_31, data = log_returns_ALTRI_SGPS)
fit_A_31


model_A_13 <- ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(1, 3)),
                         mean.model = list(armaOrder = c(0, 0)),
                         distribution.model = "std")


fit_A_13 <- ugarchfit(model_A_13, data = log_returns_ALTRI_SGPS)
fit_A_13


model_A_12 <- ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(1, 2)),
                         mean.model = list(armaOrder = c(0, 0)),
                         distribution.model = "std")


fit_A_12 <- ugarchfit(model_A_12, data = log_returns_ALTRI_SGPS)
fit_A_12











