# Load data in matrix form
load('binix.RData')

# Load anytime package to convert UNIX timestamps
library(data.table)

# Sort data
binix <- binix[order(as.numeric(binix[, 1])), ]

# Transform into data.table
bitable <- as.data.table(binix)
rm(binix)

# Convert close to numeric
bitable <- bitable[, close := as.numeric(unlist(bitable[, 4]))]

# Convert open_time column to second-based UNIX stamp
bitable <- bitable[, open_time := as.numeric(unlist(bitable[, 1]))/1000]

# Compute date and close times in YYYY-MM-DD HH:MM:SS format
bitable <- bitable[, time := anytime::anytime(as.numeric(unlist(bitable[, 1]) + 60), asUTC = TRUE)]

# Separate date
bitable <- bitable[, date := anytime::anydate(unlist(bitable[, 10]), asUTC = TRUE)]

# Format time column as H:M:S
bitable <- bitable[, time := format(anytime::anytime(unlist(bitable[, 10]), asUTC = TRUE), format = '%H:%M:%S')]

# Delete open_time and close_time columns
bitable <- bitable[, !(c('open_time', 'close_time'))]

# TO DO: implicit observations

# Set keys for data table
setkey(bitable, date, time)

# New data table with log difs and indicator functions for positive/negative
lr <- bitable[, .(ret = diff(log(close))), by = list(date)]
lr <- lr[, Ipos:= ifelse(ret > 0, 1, 0)][,N:= .N, by = date]
lr <- lr[, Ineg:= ifelse(ret < 0, 1, 0)]

rm(bitable)

# Computation of BPV
bpv <- tapply(lr[, ret], lr[, date], function(x) # BPV estimates
  (length(x)/(length(x) - 2)) / (2 / pi) * sum(abs(x[3:length(x)]) * abs(x[1:(length(x) - 2)]), na.rm = TRUE))

# Computation of TQ statistic (fourth moment)
tq <- tapply(lr[, ret], lr[, date], function(x){ # TQ estimates
  length(x)^2/(length(x) - 4) / (0.8313)^3 * sum(abs(x[1 : (length(x) - 4)])^(4/3) * abs(x[2 : (length(x) - 3)]^(4/3)) * 
                                                   abs(x[3 : (length(x) - 2)]^(4/3)), na.rm = TRUE)
})

# New data table with all the computed data
nd <- lr[, .(ret = sum(ret, na.rm = TRUE), RV = sum(ret^2, na.rm = TRUE), RSP = sum((ret^2) * Ipos, na.rm = TRUE), 
             RVOL = sqrt(sum(ret^2)), RSN = sum((ret^2) * Ineg, na.rm = TRUE), .N), by = list(date)][, BPV := bpv][, TQ := tq]

save(nd, file = 'bitable_BTC.RData')

rm(bpv, tq, lr)

# Plot histogram of the standardized returns
hist(nd[, ret], main = 'Distribution of BTC Log Returns', xlab = NA, prob = TRUE, breaks = 30, xlim = c(-.01, .01),
     font.main = 1, cex.main = 1)
curve(dnorm(x, mean = mean(nd[, ret]), sd = sd(nd[, ret])), col = 'darkblue', lwd = 2, add = TRUE, yaxt = 'n')

# Number of rows in nd matrix
L <- nrow(nd)

# 5-days and 22-days moving average of RV
nd5 <- unlist(lapply(lapply(5 : L, function(t){return(nd[(t - 4) : t, RV])}), mean))
nd22 <- unlist(lapply(lapply(22 : L, function (t) {return(nd[(t - 21) : t, RV])}), mean))

# 5-days and 22-days moving average of volatility
rvol5 <- sqrt(nd5) 
rvol22 <- sqrt(nd22)

rm(nd5, nd22)

L5 <- length(rvol5)
L22 <- length(rvol22)


# HAR model
HAR <- lm(sqrt(nd[23 : L, RV]) ~ sqrt(nd[22 : (L - 1), RV]) + rvol5[18 : (L5 - 1)] + rvol22[1 : (L22 - 1)])
summary(HAR)

# Plot of estimated volatility compared to realized volatility
plot.ts(HAR$fitted.values, ylab = NA, main = 'BTC Estimated Volatility', font.main = 1)
lines(sqrt(nd$RV[23 : nrow(nd)]), col = 'red', lty = 2)

# Histogram of the residuals
hist(residuals(HAR), breaks = 50, font.main = 1, main = 'Histogram of the Residuals of the HAR Model',
     prob = TRUE, xlab = NA, xlim = c(-0.1, 0.1))
curve(dnorm(x, mean = mean(residuals(HAR)), sd = sd(residuals(HAR))), col = "darkblue", add = TRUE, lwd = 2)

# Normal Q-Q Plot for the residuals
qqnorm(residuals(HAR), font.main = 1, ylim = c(-0.1, 0.1), xlim = c(-3, 3), cex.main = 1.1,
       main = 'Normal Q-Q Plot for the Residuals of the HAR Model')
qqline(residuals(HAR))

# Jarque Bera test for the residuals
tseries::jarque.bera.test(residuals(HAR))

# ACF of the residuals
acf(residuals(HAR), main = 'ACF for Residuals of HAR Model', ylab = NA)
# PACF of the residuals
pacf(residuals(HAR), main = 'PACF for Residuals of HAR Model', ylab = NA)

# Ljung-Box test for the residuals (H0: independently distributed data)
Box.test(residuals(HAR), type = 'Ljung-Box', lag = 7)

# Breusch-Pagan test (H0: homoskedasticity)
lmtest::bptest(HAR)

# Neural network
df <- as.data.frame(nd)
nn <- neuralnet(rvt ~ rvt_1 + nrvol5 + nrvol22, data = df, 
                hidden = 22, act.fct = "logistic", linear.output = FALSE)
plot(nn)

srvt <- sqrt(nd[23 : L, RV])
rvt_1 <- sqrt(nd[22 : (L - 1), RV])
nrvol5 <- rvol5[18 : (L5 - 1)]
nrvol22 <- rvol22[1 : (L22 - 1)]

df <- data.frame(rvt, rvt_1, nrvol5, nrvol22)
