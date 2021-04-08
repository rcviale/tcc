########## Starting point I (from raw imported matrix data) ##########
path = 'C:\\Users\\rodri\\OneDrive\\Documents\\Academics\\Trabalho de Conclusão de Curso\\'
coin = 'XRP'

# Load data in matrix form
load(paste0(path, 'binix_', coin, '.RData'))

# Sort data
binix <- binix[order(binix[, 1]), ]

# Convert open_time column to second-based UNIX timestamp
binix[, 1] <- binix[, 1] / 1000

# Convert open to close time
binix[, 1] <- binix[, 1] + 60

# Convert to data.frame
binframe <- as.data.frame(binix)

rm(binix)

# Compute date and close times in 'YYYY-MM-DD HH:MM:SS' format
binframe[, 1] <- anytime::anytime(binframe[, 1], asUTC = TRUE)

# Separate date and time
binframe <- tidyr::separate(binframe, col = open_time, into = c('date', 'time'), sep = ' ')

# Separate day, month and year
binframe <- tidyr::separate(binframe, col = date, into = c("year", "month", "day"), sep = "-")

# Separate hour and minute
binframe <- tidyr::separate(binframe, col = time, into = c("hour", "minute"), sep = ":")

# Complete implicitly missing observations
binframe <- tidyr::complete(binframe, year, month, day, hour, minute)

# Recreate date column
binframe[, 1] <- lubridate::make_date(
  year = unlist(binframe[, 'year']),
  month = unlist(binframe[, 'month']),
  day = unlist(binframe[, 'day'])
)

# Recreate time column
binframe[, 2] <- format(lubridate::make_datetime(
  hour = as.numeric(unlist(binframe[, 'hour'])),
  min = as.numeric(unlist(binframe[, 'minute'])),
  sec = 00
), format = '%H:%M:%S')

# Delete unwanted columns
binframe <- binframe[, c(1:2, 6:12)]

# Eliminate first rows (before the data actually starts) and last rows (after the
# data actually ends)
binframe <- binframe[(which(binframe[, 1] == '2018-06-01')[2] : nrow(binframe)), ]
binframe <- binframe[(1 :  which(binframe[, 1] == '2021-04-01')[1]), ]

# Convert back to data.frame 
binframe <- as.data.frame(binframe)

# Rename columns 
colnames(binframe) <- c('date', 'time', colnames(binframe)[3 : ncol(binframe)])

# Create a sequence with all the days that should exist in the data set
days <- seq(as.Date("2018-06-01"), as.Date("2021-03-31"), by = "days")

# Actual number of days in the data set
nrow(binframe) / 1440

# The complete function added more rows than there should be (with NA date values)
# Creating a vector with the row numbers that should be deleted
del <- vector()
del <- which(is.na(binframe[, 1]))

# Delete the rows
binframe <- binframe[-del, ]

rm(del, days)

# Save the new data frame
save(binframe, file = paste0(path, 'binframe_', coin, '.RData'))

########## Starting point II (from data frame) ##########
path = 'C:\\Users\\rodri\\OneDrive\\Documents\\Academics\\Trabalho de Conclusão de Curso\\'
coin = 'BTC'

# Load data
load(paste0(path, 'binframe_', coin, '.RData'))

# Load data.table package
library(data.table)

# Transform into data.table
bintable <- setDT(binframe)

rm(binframe)

# Set keys for data table
setkey(bintable, date, time)


########## PLOTS ##########
# Logs of total financial volume by time of day and date
fv_time <- bintable[, .(fvol = log(mean(as.numeric(na.omit(quote_asset_vol))))), by = list(time)]
fv_date <- bintable[, .(fvol = log(mean(as.numeric(na.omit(quote_asset_vol))))), by = list(date)]

# Plots for financial volume by time of day and date
plot.ts(fv_time[, 2], main = 'Log of $ Volume by Time of Day', ylab = '$ volume')
plot.ts(fv_date[, 2], main = 'Log of $ Volume by Date', ylab = '$ volume')

# Aggregation of time of day by hour
x <- seq(1, 1440, 60)
y <- vector()
for (i in 1 : length(x)){
  y[i] <- sum(fv_time[x[i] : (x[i] + 59), 2])
}

# Labels for each hour in the barplot
xlabs <- seq(4, 24, 4)
xpos <- seq(13, 85, 14.4)

# Barplot
barplot(height = y, width = 3, ylim = c(0.99 * min(y), 1.01 * max(y)), xpd = FALSE,
        main = 'Log of Mean $ Volume by Hour of Day', ylab = 'Volume',
        cex.names = 1, xlab = 'Hour')
box()
axis(1, at = xpos, labels = xlabs)

rm(xlabs, x, y, fv_time, fv_date, i)


########## DATA PREP FOR TABLES ##########
## 1 MIN RETURNS ##
f <- 1

# New data table with log difs and indicator functions for positive/negative
lr <- bintable[, .(ret = diff(log(close), lag = f)), by = list(date)]
lr <- lr[, Ipos:= ifelse(ret > 0, 1, 0)][,N:= .N, by = date]
lr <- lr[, Ineg:= ifelse(ret < 0, 1, 0)]

# Computation of BPV
bpv <- tapply(lr[, ret], lr[, date], function(x){
  (length(x)/(length(x) - 2)) / (2 / pi) * sum(abs(x[3:length(x)]) * 
                                                 abs(x[1:(length(x) - 2)]), na.rm = TRUE)
})

# Computation of TQ statistic (fourth moment)
tq <- tapply(lr[, ret], lr[, date], function(x){ # TQ estimates
  length(x)^2/(length(x) - 4) / (0.8313)^3 * sum(abs(x[1 : (length(x) - 4)])^(4/3) * 
                                                   abs(x[2 : (length(x) - 3)]^(4/3)) * 
                                                   abs(x[3 : (length(x) - 2)]^(4/3)), na.rm = TRUE)
})

# New data table with all the computed data
ntable <- lr[, .(ret = sum(ret, na.rm = TRUE) / f, RV = sum(ret^2, na.rm = TRUE) / f, 
                 RSP = sum((ret^2) * Ipos, na.rm = TRUE), RVOL = sqrt(sum(ret^2, na.rm = TRUE)), 
                 RSN = sum((ret^2) * Ineg, na.rm = TRUE), .N), 
             by = list(date)][, BPV := bpv][, TQ := tq]

rm(bpv, tq, lr)

# Save data table
save(ntable, file = paste0(path, 'bintable_', coin, f, '.RData'))

rm(ntable)

## 5 MIN RETURNS ##
f <- 5

# New data table with log difs and indicator functions for positive/negative
lr <- bintable[, .(ret = diff(log(close), lag = f)), by = list(date)]
lr <- lr[, Ipos:= ifelse(ret > 0, 1, 0)][,N:= .N, by = date]
lr <- lr[, Ineg:= ifelse(ret < 0, 1, 0)]

# Computation of BPV
bpv <- tapply(lr[, ret], lr[, date], function(x){
  (length(x)/(length(x) - 2)) / (2 / pi) * sum(abs(x[3:length(x)]) * 
                                                 abs(x[1:(length(x) - 2)]), na.rm = TRUE)
})

# Computation of TQ statistic (fourth moment)
tq <- tapply(lr[, ret], lr[, date], function(x){ # TQ estimates
  length(x)^2/(length(x) - 4) / (0.8313)^3 * sum(abs(x[1 : (length(x) - 4)])^(4/3) * 
                                                   abs(x[2 : (length(x) - 3)]^(4/3)) * 
                                                   abs(x[3 : (length(x) - 2)]^(4/3)), na.rm = TRUE)
})

# New data table with all the computed data
ntable <- lr[, .(ret = sum(ret, na.rm = TRUE) / f, RV = sum(ret^2, na.rm = TRUE) / f, 
                 RSP = sum((ret^2) * Ipos, na.rm = TRUE), RVOL = sqrt(sum(ret^2, na.rm = TRUE)), 
                 RSN = sum((ret^2) * Ineg, na.rm = TRUE), .N), 
             by = list(date)][, BPV := bpv][, TQ := tq]

rm(bpv, tq, lr)

# Save data table
save(ntable, file = paste0(path, 'bintable_', coin, f, '.RData'))

#





########## Starting point III (from data table) ##########
path = 'C:\\Users\\rodri\\OneDrive\\Documents\\Academics\\Trabalho de Conclusão de Curso\\'
coin = 'BTC'

# Load the data
load(paste0(path, 'bintable_', coin, '.RData'))

# Plot histogram of the standardized returns
hist(ntable[, ret], main = 'Distribution of BTC Standardized Log Returns', xlab = NA, prob = TRUE, 
     breaks = 30, font.main = 1, cex.main = 1, xlim = c(-0.2, 0.2))
curve(dnorm(x, mean = mean(ntable[, ret]), sd = sd(ntable[, ret])), col = 'darkblue', 
      lwd = 2, add = TRUE, yaxt = 'n')

# Number of rows in ntable matrix
L <- nrow(ntable)

# 5-days and 22-days moving average of RV
nd5 <- unlist(lapply(lapply(5 : L, function(t){return(ntable[(t - 4) : t, RV])}), mean))
nd22 <- unlist(lapply(lapply(22 : L, function (t) {return(ntable[(t - 21) : t, RV])}), mean))

# 5-days and 22-days moving average of volatility
rvol5 <- sqrt(nd5) 
rvol22 <- sqrt(nd22)

rm(nd5, nd22)

L5 <- length(rvol5)
L22 <- length(rvol22)

# HAR model
HAR <- lm(sqrt(ntable[23 : L, RV]) ~ sqrt(ntable[22 : (L - 1), RV]) + 
            rvol5[18 : (L5 - 1)] + rvol22[1 : (L22 - 1)])
summary(HAR)

# Plot of estimated volatility compared to realized volatility
plot.ts(HAR$fitted.values, ylab = NA, main = 'BTC Estimated Volatility', font.main = 1)
lines(sqrt(ntable$RV[23 : nrow(ntable)]), col = 'red', lty = 2)

# Histogram of the residuals
hist(residuals(HAR), breaks = 50, font.main = 1, main = 'Histogram of the Residuals of the HAR Model',
     prob = TRUE, xlab = NA)
curve(dnorm(x, mean = mean(residuals(HAR)), sd = sd(residuals(HAR))), col = "darkblue", add = TRUE, lwd = 2)

# Normal Q-Q Plot for the residuals
qqnorm(residuals(HAR), font.main = 1, cex.main = 1.1,
       main = 'Normal Q-Q Plot for the Residuals of the HAR Model')
qqline(residuals(HAR))

# Jarque Bera test for the residuals
tseries::jarque.bera.test(residuals(HAR))

# ACF of the residuals
acf(residuals(HAR), main = 'ACF for Residuals of HAR Model', ylab = NA)
# PACF of the residuals
pacf(residuals(HAR), main = 'PACF for Residuals of HAR Model', ylab = NA)

# Ljung-Box test for the residuals (H0: independently distributed data)
Box.test(residuals(HAR), type = 'Ljung-Box', lag = 5)

# Breusch-Pagan test (H0: homoskedasticity)
lmtest::bptest(HAR)

########## GARCH(1, 1) Comparison ##########
# GARCH(1, 1) model for the data
garchs <- rugarch::ugarchspec(mean.model = list(armaOrder = c(0, 0)), 
                              variance.model = list(garchOrder = c(1, 1)))
garch <- rugarch::ugarchfit(garchs, ntable[, ret])

rm(garchs)

# Comparison between basic HAR, Assymetric HAR, Jumps HAR and GARCH(1, 1)
plot.ts(sqrt(ntable$RV[23 : L]), ylab = NA, main = 'Gold Estimated Volatility Comparison', font.main = 1)
lines(garch@fit$sigma[23 : L], col = 'blue')
lines(HAR$fitted.values, col = 'red')
legend('top', ncol = 3, col = c('black', 'blue', 'red'), lwd = 2, bty = 'n',
       legend = c('RV', 'GARCH(1, 1)', 'HAR'))

# Mean square error and mean absolute error
mu <- sqrt(ntable$RV[23 : L])

HAR_fv <- HAR$fitted.values
GARCH_fv <- garch@fit$sigma[23 : L]

mse_HAR <- mean((mu - HAR_fv)^2)
mse_GARCH <- mean((mu - GARCH_fv)^2)

mae_HAR <- mean(abs(mu - HAR_fv))
mae_GARCH <- mean(abs(mu - GARCH_fv))

eix <- matrix(ncol = 2, nrow = 2)

colnames(eix) <- c('MSE', 'MAE')
rownames(eix) <- c('HAR', 'GARCH(1, 1)')

eix[1, 1] <- mse_HAR * 1000000
eix[1, 2] <- mae_HAR * 1000000
eix[2, 1] <- mse_GARCH * 1000000
eix[2, 2] <- mae_GARCH * 1000000

View(eix)








########## Neural Network ##########

# Load neural network package
library(neuralnet)

# Create data frame with data for HAR model
rvt <- sqrt(ntable[23 : L, RV])
rvt_1 <- sqrt(ntable[22 : (L - 1), RV])
nrvol5 <- rvol5[18 : (L5 - 1)]
nrvol22 <- rvol22[1 : (L22 - 1)]

df <- data.frame(rvt, rvt_1, nrvol5, nrvol22)

nn <- neuralnet(rvt ~ rvt_1 + nrvol5 + nrvol22, data = df, 
                hidden = 7, act.fct = "logistic", linear.output = FALSE)
plot(nn)


