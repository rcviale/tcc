########## Starting point I (from raw imported matrix data) ##########
path <- 'C:\\Users\\rodri\\OneDrive\\Documents\\Academics\\Trabalho de Conclusão de Curso\\'
coin <- 'BTC'

# Load data in matrix form
load(paste0(path, 'binix_', coin, '.RData'))

# Sort data
binix <- binix[order(binix[, 1]), ]

# Convert open_time column to second-based UNIX time stamp
binix[, 1] <- binix[, 1] / 1000

# Convert to data.frame
binframe <- as.data.frame(binix)

rm(binix)

# Compute date and close times in 'YYYY-MM-DD HH:MM:SS' format
binframe[, 1] <- anytime::anytime(binframe[, 1], asUTC = TRUE)

# Separate date and time
binframe <- tidyr::separate(binframe, col = open_time, into = c('date', 'time'), sep = ' ')

# Save initial and ending date
inidate <- binframe[1, 1]
enddate <- binframe[nrow(binframe), 1]

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
binframe <- binframe[(which(binframe[, 1] == inidate)[1]) :
                     (which(binframe[, 1] == as.character(as.Date(enddate)))[1440]), ]

rm(inidate, enddate)

# Convert back to data.frame 
binframe <- as.data.frame(binframe)

# Rename columns 
colnames(binframe) <- c('date', 'time', colnames(binframe)[3 : ncol(binframe)])

# Create a sequence with all the days that should exist in the data set
length(seq(as.Date(inidate), as.Date(enddate), by = "days"))

# Actual number of days in the data set
nrow(binframe) / 1440

# The complete function adds more rows than there should be (with NA date values)
# because it creates rows with non-existing dates (i.e. 31/02)
# Creating a vector with the row numbers that have NA dates
del <- vector()
del <- which(is.na(binframe[, 1]))

# Delete the rows with NA dates
binframe <- binframe[-del, ]

nrow(binframe) / 1440

rm(del)

# Save the new data frame
save(binframe, file = paste0(path, 'binframe_', coin, '.RData'))

rm(list = ls())

########## Starting point II (from data frame) ##########
path <- 'C:\\Users\\rodri\\OneDrive\\Documents\\Academics\\Trabalho de Conclusão de Curso\\'
coin <- 'BTC'

# Load data
load(paste0(path, 'binframe_', coin, '.RData'))

# Load data.table package
library(data.table)

# Create factor variables for days of the week
dow <- as.POSIXlt(seq(as.Date(binframe[1, 1]),
                      as.Date(binframe[nrow(binframe), 1]),
                      by = "days"))$wday
# Create dummy variables for working days
wd <- sapply(1 : length(dow), function(x){
  ifelse(dow[x] == 0 || dow[x] == 6, 0, 1)
})

# Transform into data.table
bintable <- setDT(binframe)

rm(binframe)

# Set keys for data table
setkey(bintable, date, time)


########## PLOTS ##########
# Logs of total financial volume by time of day and date
fv_time <- bintable[, .(fvol = (mean(as.numeric(na.omit(quote_asset_vol))))), by = list(time)]
fv_date <- bintable[, .(fvol = (mean(as.numeric(na.omit(quote_asset_vol))))), by = list(date)]

# Plots for financial volume by time of day and date
plot.ts(fv_time[, 2], main = '$ Volume by Time of Day', ylab = '$ volume')
plot.ts(fv_date[, 2], main = '$ Volume by Date', ylab = '$ volume')

# Aggregation of time of day by hour
x <- seq(1, 1440, 60)
y <- vector()
for (i in 1 : length(x)){
  y[i] <- sum(fv_time[x[i] : (x[i] + 59), 2]) / 60
}

# Labels for each hour in the barplot
xlabs <- seq(1, 24, 1)

# Barplot
barplot(height = y, width = 3, ylim = c(0, 1.01 * max(y)), xpd = FALSE,
        main = 'Mean $ Volume by Hour of the Day', ylab = 'Volume',
        cex.names = 1, xlab = 'Hour', names.arg = xlabs)
box()

rm(xlabs, x, y, fv_time, fv_date, i)


########## DATA PREP FOR TABLES ##########
#### 1 MIN RETURNS ####
# Set sampling frequency
f <- 1

# New data table with log difs and indicator functions for positive/negative
lr <- bintable[, .(ret = diff(log(open), lag = f)), by = list(date)]
lr <- lr[, Ipos:= ifelse(ret > 0, 1, 0)][,N:= .N, by = date]
lr <- lr[, Ineg:= ifelse(ret < 0, 1, 0)]

# Computation of BPV
bpv <- tapply(lr[, ret], lr[, date], function(x){
  (length(x)/(length(x) - 2)) / (2 / pi) * sum(abs(x[3:length(x)]) * 
                                                 abs(x[1:(length(x) - 2)]), na.rm = TRUE)
})

# Computation of TQ statistic (fourth moment)
tq <- tapply(lr[, ret], lr[, date], function(x){
  length(x)^2/(length(x) - 4) / (0.8313)^3 * sum(abs(x[1 : (length(x) - 4)])^(4/3) * 
                                                   abs(x[2 : (length(x) - 3)]^(4/3)) * 
                                                   abs(x[3 : (length(x) - 2)]^(4/3)), na.rm = TRUE)
})

# MedRV Computation
medrv <- tapply(lr[, ret], lr[, date], function(x){
  sum(unlist(lapply(3 : length(x), function(t){
    return(median(abs(x[(t - 2) : t]), na.rm = TRUE)^2) 
  })), na.rm = TRUE) * 
    (pi / (6 - 4 * sqrt(3) + pi)) * (length(x) / (length(x) - 2)) / f
})

# New data table with all the computed data
ntable <- lr[, .(ret = sum(ret, na.rm = TRUE) / f, RV = sum(ret^2, na.rm = TRUE) / f, 
                 RSP = sum((ret^2) * Ipos, na.rm = TRUE), RVOL = sqrt(sum(ret^2, na.rm = TRUE)), 
                 RSN = sum((ret^2) * Ineg, na.rm = TRUE), .N), 
             by = list(date)][, BPV := bpv][, TQ := tq][, medRV := medrv]

rm(bpv, tq, lr, medrv)

# Save data table
save(ntable, file = paste0(path, 'bintable_', coin, f, '.RData'))

### 5 MIN RETURNS ###
f <- 5

# New data table with log difs and indicator functions for positive/negative
lr <- bintable[, .(ret = diff(log(open), lag = f)), by = list(date)]
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

# MedRV Computation
medrv <- tapply(lr[, ret], lr[, date], function(x){
  sum(unlist(lapply(3 : length(x), function(t){
    return(median(abs(x[(t - 2) : t]), na.rm = TRUE)^2) 
  })), na.rm = TRUE) * 
    (pi / (6 - 4 * sqrt(3) + pi)) * (length(x) / (length(x) - 2)) / f
})

medrv5 <- tapply(lr[, ret], lr[, date], function(x){
  sum(unlist(lapply(5 : length(x), function(t){
    return(median(abs(x[(t - 4) : t]), na.rm = TRUE)^2) 
  })), na.rm = TRUE) * 
    (pi / (6 - 4 * sqrt(3) + pi)) * (length(x) / (length(x) - 2)) / f
})

# New data table with all the computed data
ntable <- lr[, .(ret = sum(ret, na.rm = TRUE) / f, RV = sum(ret^2, na.rm = TRUE) / f, 
                 RSP = sum((ret^2) * Ipos, na.rm = TRUE), RVOL = sqrt(sum(ret^2, na.rm = TRUE)), 
                 RSN = sum((ret^2) * Ineg, na.rm = TRUE), .N), 
             by = list(date)][, BPV := bpv][, TQ := tq][, medRV := medrv][, medRV5 := medrv5]
ntable <- ntable[, dow := dow][, wd := wd]

rm(bpv, tq, lr, medrv, medrv5)

# Save data table
save(ntable, file = paste0(path, 'bintable_', coin, f, '.RData'))

## 15 MIN RETURNS ##
f <- 15

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

## 30 MIN RETURNS ##
f <- 30

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

## 60 MIN RETURNS ##
f <- 60

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


########## Starting point III (from data table) ##########
path <- 'C:\\Users\\rodri\\OneDrive\\Documents\\Academics\\Trabalho de Conclusão de Curso\\'
coin <- 'BTC'
f <- 5

library(data.table)

# Load the data
load(paste0(path, 'bintable_', coin, f, '.RData'))

### Descriptive Stats ###
rvol_dow <- as.matrix(ntable[, (.rvol = mean(RV)), by = list(dow)])
rvol_dow <- rvol_dow[order(rvol_dow[, 1]), ]

xlabs <- c('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat')

# Barplot
barplot(height = rvol_dow[, 2], width = 3, xpd = FALSE, names.arg = xlabs,
        ylim = c(0, 1.03 * (max(rvol_dow[, 2]))), cex.names = 1,
        main = 'Realized Volatility by Day of the Week', ylab = 'Realized Volatility',
        xlab = 'Day of the week')
box()

##### HAR Models Construction #####

### HAR 1-step ahead forecast Comparison ###
# Number of rows in ntable matrix
L <- nrow(ntable)

# Function for 1-step ahead forecast
predict1 <- function(spec, data, M){
  P <- nrow(data) - M
  results <- rep(0, P)
  for (i in 1 : P) {
    pred <- data[(M + i), ]
    est <- data[i : (M + i - 1), ]
    results[i] <- predict(lm(spec, data = est), newdata = pred)
  }
  results
}

# Number of days for the initial model estimation
M <- 365 * 2
# Data frame for RMSE and MAE of the models
fcix <- data.frame()
# Auxiliary sequence for combination of models' time windows
cols <- 2:28

#   Loop for model estimation, forecasting, computation of RMSE and MAE, 
# and filling the data frame 
for (i in 2 : 27){
  for (j in cols[i] : 28){
    # Short moving average
    mai <- sqrt(unlist(lapply(lapply(i : L, function(t){return(ntable[(t - (i - 1)) : t, RV])}), mean)))
    # Long moving average
    maj <- sqrt(unlist(lapply(lapply(j : L, function (t) {return(ntable[(t - (j - 1)) : t, RV])}), mean)))
    # Length of short and long
    Li <- length(mai)
    Lj <- length(maj)
    # New data frame with only the data that is needed for the model estimation
    nfr <- as.data.frame(cbind(y = sqrt(ntable[(j + 1) : L, RV]),
                               d1 = sqrt(ntable[j : (L - 1), RV]),
                               mas = mai[(j - i + 1) : (Li - 1)],
                               mal = maj[1 : (Lj - 1)]))
    # Forecast
    fcast <- predict1(y ~ d1 + mas + mal, nfr, M)
    
    # Filling the data frame with specification, RMSE and MAE
    fcix[nrow(fcix) + 1, 'Spec'] <- paste0('(', i, ', ', j, ')')
    fcix[nrow(fcix), 'RMSE'] <- sqrt(mean((fcast - sqrt(ntable[(M + 1 + j) : L, RV]))^2))
    fcix[nrow(fcix), 'MAE'] <- mean(abs(fcast - sqrt(ntable[(M + 1 + j) : L, RV])))
  }
}

rm(mai, maj, Li, Lj, nfr, i, j, fcast, cols)

# Save data frame
save(fcix, file = paste0(path, 'fcix_', M, '_', coin, f, '.RData'))
openxlsx::write.xlsx(fcix, file = paste0(path, 'fcix_', M, '_', coin, f, '.xlsx'))

rm(M, fcix)

### Specific HAR Construction ###
# Time windows do be used
short <- 5
long <- 15

# Moving averages of RVOL
rvols <- sqrt(unlist(lapply(lapply(short : L, function(t){
  return(ntable[(t - (short - 1)) : t, RV])
}), mean)))
rvoll <- sqrt(unlist(lapply(lapply(long : L, function (t) {
  return(ntable[(t - (long - 1)) : t, RV])
}), mean)))

Ls <- length(rvols)
Ll <- length(rvoll)

# Specific HAR model
HAR <- lm(sqrt(ntable[(long + 1) : L, RV]) ~ sqrt(ntable[(long) : (L - 1), RV]) + 
               rvols[(long - short + 1) : (Ls - 1)] + rvoll[1 : (Ll - 1)])
summary(HAR)
BIC(HAR)

# Specific HAR model with day of the week factor
HAR_dow <- lm(sqrt(ntable[(long + 1) : L, RV]) ~ sqrt(ntable[(long) : (L - 1), RV]) + 
            rvols[(long - short + 1) : (Ls - 1)] + rvoll[1 : (Ll - 1)] + ntable[(long + 1) : L, dow])
summary(HAR_dow)
BIC(HAR_dow)

# Specific HAR model with working days dummy
HAR_wd <- lm(sqrt(ntable[(long + 1) : L, RV]) ~ sqrt(ntable[(long) : (L - 1), RV]) + 
                rvols[(long - short + 1) : (Ls - 1)] + rvoll[1 : (Ll - 1)] + ntable[(long + 1) : L, wd])
summary(HAR_wd)
BIC(HAR_wd)

rm(rvols, rvoll)

# Moving averages of MedRV(3)
medvols <- sqrt(unlist(lapply(lapply(short : L, function(t){return(ntable[(t - (short - 1)) : t, medRV])}), mean)))
medvoll <- sqrt(unlist(lapply(lapply(long : L, function (t) {return(ntable[(t - (long - 1)) : t, medRV])}), mean)))

# HAR MedRV(3) model
medHAR <- lm(sqrt(ntable[(long + 1) : L, medRV]) ~ sqrt(ntable[(long) : (L - 1), medRV]) + 
             medvols[(long - short + 1) : (Ls - 1)] + medvoll[1 : (Ll - 1)])
summary(medHAR)
BIC(medHAR)

# HAR MedRV(3) model with day of the week factor
medHAR_dow <- lm(sqrt(ntable[(long + 1) : L, medRV]) ~ sqrt(ntable[(long) : (L - 1), medRV]) + 
                 medvols[(long - short + 1) : (Ls - 1)] + medvoll[1 : (Ll - 1)] + 
                 ntable[(long + 1) : L, dow])
summary(medHAR_dow)
BIC(medHAR_dow)

# HAR MedRV(3) model with working days dummy
medHAR_wd <- lm(sqrt(ntable[(long + 1) : L, medRV]) ~ sqrt(ntable[(long) : (L - 1), medRV]) + 
                medvols[(long - short + 1) : (Ls - 1)] + medvoll[1 : (Ll - 1)] + 
                ntable[(long + 1) : L, wd])
summary(medHAR_wd)
BIC(medHAR_wd)

rm(medvols, medvoll)

# Moving averages of MedRV(5)
medvols <- sqrt(unlist(lapply(lapply(short : L, function(t){return(ntable[(t - (short - 1)) : t, medRV5])}), mean)))
medvoll <- sqrt(unlist(lapply(lapply(long : L, function (t) {return(ntable[(t - (long - 1)) : t, medRV5])}), mean)))

# HAR MedRV(5) model
medHAR5 <- lm(sqrt(ntable[(long + 1) : L, medRV5]) ~ sqrt(ntable[(long) : (L - 1), medRV5]) + 
               medvols[(long - short + 1) : (Ls - 1)] + medvoll[1 : (Ll - 1)])
summary(medHAR5)
BIC(medHAR5)

# HAR MedRV(5) model with day of the week factor
medHAR5_dow <- lm(sqrt(ntable[(long + 1) : L, medRV5]) ~ sqrt(ntable[(long) : (L - 1), medRV5]) + 
                   medvols[(long - short + 1) : (Ls - 1)] + medvoll[1 : (Ll - 1)] + 
                   ntable[(long + 1) : L, dow])
summary(medHAR5_dow)
BIC(medHAR5_dow)

# HAR MedRV(5) model with working days dummy
medHAR5_wd <- lm(sqrt(ntable[(long + 1) : L, medRV5]) ~ sqrt(ntable[(long) : (L - 1), medRV5]) + 
                  medvols[(long - short + 1) : (Ls - 1)] + medvoll[1 : (Ll - 1)] + 
                  ntable[(long + 1) : L, wd])
summary(medHAR5_wd)
BIC(medHAR5_wd)

rm(Ls, Ll, medvols, medvoll)

########## GARCH(1, 1) Comparison ##########
# GARCH(1, 1) model for the data
garchs <- rugarch::ugarchspec(mean.model = list(armaOrder = c(0, 0)), 
                              variance.model = list(garchOrder = c(1, 1)))
garch <- rugarch::ugarchfit(garchs, ntable[, ret])

rm(garchs)

# Comparison between HAR and GARCH(1, 1)
plot.ts(sqrt(ntable$RV[(long + 1) : L]), ylab = NA, 
        main = paste0(coin, ' Estimated Volatility Comparison'), font.main = 1)
lines(garch@fit$sigma[(long + 1) : L], col = 'blue')
lines(HAR$fitted.values, col = 'red')
lines(medHAR$fitted.values, col = 'green')
legend('topleft', ncol = 4, col = c('black', 'blue', 'red', 'green'), lwd = 2, bty = 'n',
       legend = c('RV', 'GARCH(1, 1)', 'HAR', 'MedRV HAR'))

# Mean square error and mean absolute error
mu <- sqrt(ntable$RV[(long + 1) : L])

HAR_fv <- HAR$fitted.values
dowHAR_fv <- HAR_dow$fitted.values
wdHAR_fv <- HAR_wd$fitted.values
medHAR_fv <- medHAR$fitted.values
dowmedHAR_fv <- medHAR_dow$fitted.values
wdmedHAR_fv <- medHAR_wd$fitted.values
GARCH_fv <- garch@fit$sigma[(long + 1) : L]

mse_HAR <- mean((mu - HAR_fv)^2)
mse_dowHAR <- mean((mu - dowHAR_fv)^2)
mse_wdHAR <- mean((mu - wdHAR_fv)^2)
mse_medHAR <- mean((mu - medHAR_fv)^2)
mse_dowmedHAR <- mean((mu - dowmedHAR_fv)^2)
mse_wdmedHAR <- mean((mu - wdmedHAR_fv)^2)
mse_GARCH <- mean((mu - GARCH_fv)^2)

mae_HAR <- mean(abs(mu - HAR_fv))
mae_dowHAR <- mean(abs(mu - dowHAR_fv))
mae_wdHAR <- mean(abs(mu - wdHAR_fv))
mae_medHAR <- mean(abs(mu - medHAR_fv))
mae_dowmedHAR <- mean(abs(mu - dowmedHAR_fv))
mae_wdmedHAR <- mean(abs(mu - wdmedHAR_fv))
mae_GARCH <- mean(abs(mu - GARCH_fv))

rm(long, HAR_fv, dowHAR_fv, wdHAR_fv, medHAR_fv, dowmedHAR_fv, wdmedHAR_fv, GARCH_fv)

eix <- matrix(ncol = 3, nrow = 8)

colnames(eix) <- c('Model', 'MSE', 'MAE')
eix[, 1] <- c('HAR-RV', 'HAR-RV-DoW', 'HAR-RV-WD', 'HAR-MedRV', 'HAR-MedRV-DoW',
              'HAR-MedRV-WD', 'GARCH(1, 1)')

eix[1, 2] <- mse_HAR * 1000
eix[1, 3] <- mae_HAR * 1000
eix[2, 2] <- mse_dowHAR * 1000
eix[2, 3] <- mae_dowHAR * 1000
eix[3, 2] <- mse_wdHAR * 1000
eix[3, 3] <- mae_wdHAR * 1000
eix[4, 2] <- mse_medHAR * 1000
eix[4, 3] <- mae_medHAR * 1000
eix[5, 2] <- mse_dowmedHAR * 1000
eix[5, 3] <- mae_dowmedHAR * 1000
eix[6, 2] <- mse_wdmedHAR * 1000
eix[6, 3] <- mae_wdmedHAR * 1000
eix[7, 2] <- mse_GARCH * 1000
eix[7, 3] <- mae_GARCH * 1000

rm(mse_HAR, mae_HAR, mse_dowHAR, mae_dowHAR, mse_wdHAR, mae_wdHAR, mse_medHAR,
   mae_medHAR, mse_dowmedHAR, mae_dowmedHAR, mse_wdmedHAR, mae_wdmedHAR,
   mse_GARCH, mae_GARCH, mse_RVGARCH, mae_RVGARCH)

View(eix)

openxlsx::write.xlsx(eix, file = paste0(path, 'eix_', coin, f, '.xlsx'))

# Plot histogram of the standardized returns
hist(ntable[, ret], main = 'Distribution of BTC Standardized Log Returns', xlab = NA, prob = TRUE, 
     breaks = 30, font.main = 1, cex.main = 1, xlim = c(-0.2, 0.2))
curve(dnorm(x, mean = mean(ntable[, ret]), sd = sd(ntable[, ret])), col = 'darkblue', 
      lwd = 2, add = TRUE, yaxt = 'n')

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





### HAR Model Selection by BIC ###
# HAR different time windows comparison
cols <- 2:28
crame <- data.frame()

# Loop to fill data frame with Rsq and BIC for different HAR models
for (i in 2 : 27){
  for (j in cols[i] : 28){
    mai <- unlist(lapply(lapply(i : L, function(t){return(ntable[(t - (i - 1)) : t, RV])}), mean))
    maj <- unlist(lapply(lapply(j : L, function (t) {return(ntable[(t - (j - 1)) : t, RV])}), mean))
    
    mai <- sqrt(mai)
    maj <- sqrt(maj)
    
    Li <- length(mai)
    Lj <- length(maj)
    
    # HAR model
    model <- lm(sqrt(ntable[(j + 1) : L, RV]) ~ sqrt(ntable[j : (L - 1), RV]) + 
                  mai[(j - i + 1) : (Li - 1)] + maj[1 : (Lj - 1)])
    
    # HAR model with days of the week factor
    model_d <- lm(sqrt(ntable[(j + 1) : L, RV]) ~ sqrt(ntable[j : (L - 1), RV]) + 
                    mai[(j - i + 1) : (Li - 1)] + maj[1 : (Lj - 1)] + ntable[(j + 1) : L, dow])
    
    # HAR model with working days dummy
    model_w <- lm(sqrt(ntable[(j + 1) : L, RV]) ~ sqrt(ntable[j : (L - 1), RV]) + 
                    mai[(j - i + 1) : (Li - 1)] + maj[1 : (Lj - 1)] + ntable[(j + 1) : L, wd])
    
    # Fill data frame
    crame[(nrow(crame) + 1), 1] <- paste0('(', i, ', ', j, ')')
    crame[nrow(crame), 2] <- summary(model)$r.squared
    crame[nrow(crame), 3] <- BIC(model)
    crame[nrow(crame), 4] <- summary(model_d)$r.squared
    crame[nrow(crame), 5] <- BIC(model_d)
    crame[nrow(crame), 6] <- summary(model_w)$r.squared
    crame[nrow(crame), 7] <- BIC(model_w)
    
  }
}

rm(model, model_d, model_w, maj, mai, Lj, Li, i, j, cols)

# Column names for HAR comparison data frame
colnames(crame) <- c('Spec', 'HAR Rsq', 'HAR BIC', 'HAR-DOW Rsq', 'HAR-DOW BIC',
                     'HAR-WD Rsq', 'HAR-WD BIC')
View(crame)

save(crame, file = paste0(path, 'crame_', coin, f, '.RData'))
openxlsx::write.xlsx(crame, file = paste0(path, 'crame_', coin, f, '.xlsx'))

rm(crame)






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


