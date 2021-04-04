path = 'C:\\Users\\rodri\\OneDrive\\Documents\\Academics\\Trabalho de Conclusão de Curso\\'

########## Starting point I (from raw matrix) ##########

# Load data in matrix form
load(paste0(path, 'binix.RData'))

# Drop unwanted column (close_time)
binix <- binix[, c(1:6, 8:9)]

# Sort data
binix <- binix[order(as.numeric(binix[, 1])), ]

# Load data.table package
library(data.table)

# Transform into data.table
bitable <- as.data.table(binix)
rm(binix)

a <- nrow(bitable)

# Convert open_time column to second-based UNIX stamp
bitable <- bitable[, open_time := as.numeric(unlist(bitable[, 1]))/1000]

# Compute date and close times in YYYY-MM-DD HH:MM:SS format
bitable <- bitable[, date_time := anytime::anytime(as.numeric(unlist(bitable[, 1]) + 60), asUTC = TRUE)]

# Delete open_time column
bitable <- bitable[, !('open_time')]

# Separate date and time
bitable <- tidyr::separate(bitable, col = date_time, into = c('date', 'time'), sep = ' ')

# Separate day, month and year
bitable <- tidyr::separate(bitable, col = date, into = c("year", "month", "day"), sep = "-")

# Separate hour, minute, minute, second
bitable <- tidyr::separate(bitable, col = time, into = c("hour", "minute", "second"), sep = ":")

# Drop seconds column
bitable <- bitable[, !('second')]

# Complete implicitly missing observations
bitable <- tidyr::complete(bitable, year, month, day, hour, minute)

# Recreate date and time columns
setDT(bitable)

bitable <- bitable[, date := lubridate::make_date(
  year = year, month = month, day = day
)]

bitable <- bitable[, time := format(lubridate::make_datetime(
  hour = as.numeric(hour), min = as.numeric(minute)
), format = '%H:%M:%S')]

# Delete unwanted columns
bitable <- bitable[, !(c('year', 'month', 'day', 'hour', 'minute'))]

# Delete remaining unwanted rows (April-21 forward)
bitable <- bitable[2 : which(as.Date(bitable[, date]) > '2021-03-31')[1],]

load(paste0(path, 'bitable_final.RData'))

# TO DO LIST: convert prices to numeric

cols <- colnames(bitable)
phst <- seq(1, nrow(bitable), by = 100000)
phen <- c(seq(100000, nrow(bitable), by = 100000), nrow(bitable))

# Convert prices columns to numeric
for (j in 1 : 1){
  for (i in 1 : 1){
    new_values <- as.numeric(bitable[phst[i] : phen[i], j])
    
        bitable <- bitable[, cols[j] := as.numeric(unlist(bitable[1 : phases[i], j]))]
  }
}

a <- bitable[phases[1] : phases[3], 1]

View(bitable[1:10, ])

#bitable[, 1] <- lapply(lapply(bitable[1:500, 1], unlist), as.numeric)

#bitable[, close := as.numeric(unlist(bitable[, 4]))]

a[1:10, c('open', 'high')] <- lapply(lapply(bitable[1919501:1919510, 1:2], unlist), as.numeric)

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

########## Starting point II (from raw data table) ##########

# Load the data
load('C:\\Users\\rodri\\OneDrive\\Documents\\Academics\\Trabalho de Conclusão de Curso\\bitable_BTC.RData')

# 



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
