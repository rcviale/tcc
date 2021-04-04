library(httr)
library(jsonlite)
library(data.table)

rm(list = ls())

base <- "https://api.binance.com/"
ticker <- 'BTCUSDT'

# Exchange info
get_info <- GET(url = base, path = '/api/v3/exchangeInfo')
info <- fromJSON(content(get_info, "text"), flatten = TRUE)
info$rateLimits

rm(get_info, info)

# Days x time ranges
days <- seq(as.Date("2017-09-01"), as.Date("2021-03-31"), by = "days")

f <- 2
N <- f * length(days)

# Matrix with all start and end dates in ISO8601 format
start <- matrix(ncol = f, nrow = (N/f))
end <- matrix(ncol = f, nrow = (N/f))


# Times for starting ranges
times_start <- rep(c('00:00:00', '12:00:00'), (N/f))

for (i in 1 : length(days)){
  for (j in 1 : f){
    start[i, j] <- paste0(as.numeric(as.POSIXct(paste0(days[i], ' ', times_start[j]), tz = 'UTC'))/10, '0000')
  }
}

# Times for ending ranges
times_end <- rep(c('11:59:00', '23:59:00'), (N/f))

for (i in 1 : length(days)){
  for (j in 1 : f){
    end[i, j] <- paste0(as.numeric(as.POSIXct(paste0(days[i], ' ', times_end[j]), tz = 'UTC'))/10, '0000')
  }
}

# First day and time range
binraw <- GET(url = base, path = '/api/v3/klines', 
              query = list(
                symbol = ticker,
                interval = '1m',
                startTime = start[1, 1],
                endTime = end[1, 1],
                limit = as.integer(1000)
              ))

# Convert from JSON to matrix
binix2 <- fromJSON(content(binraw, "text"), flatten = TRUE)

# Second day and time range + merge 1st and 2nd
binraw2 <- GET(url = base, path = '/api/v3/klines', 
              query = list(
                symbol = ticker,
                interval = '1m',
                startTime = start[1, 2],
                endTime = end[1, 2],
                limit = as.integer(1000)
              ))

# Convert to matrix
binix2 <- fromJSON(content(binraw2, "text"), flatten = TRUE)

# Merge two time ranges of first day
binix <- rbind(binix, binix2)

rm(binraw, binix2, binraw2)

# Download the remaining data, merging it with the first day
for (i in 2 : length(days)){
  for (j in 1 : f){
    binraw <- GET(url = base, path = '/api/v3/klines', 
                  query = list(
                    symbol = ticker,
                    interval = '1m',
                    startTime = start[i, j],
                    endTime = end[i, j],
                    limit = as.integer(1000)
                  ))
    nbinix <- fromJSON(content(binraw, "text"), flatten = TRUE)
    binix <- rbind(binix, nbinix)
    Sys.sleep(0.1)
  }
}

# check for duplicates, number of obs and number of days
anyDuplicated(binix)
nrow(binix)
nrow(binix) / 1440

# Get rid of unwanted columns
nbinix <- binix[, 1:9]

rm(binix)

# Input column names
colnames(nbinix) <- c('open_time', 'open', 'high', 'low', 'close', 'volume',
                       'close_time', 'quote_asset_vol', 'no_trades')

# Load frame with all the data
load("binix.RData")

binix <- rbind(binix, nbinix)

# Save matrix
save(binix, file = 'binix.RData')

rm(binix, nbinix)






# Convert to data.frame
binframe_new <- as.data.frame(binix)

rm(binix, binraw)


# Merge new frame with rest of data
binframe <- rbind(binframe, binframe_new)

# Save new frame
save(binframe, file = "binframe.RData")

rm(binframe_new, binframe)



nrow(binframe)
nrow(binframe) / 1440
anyDuplicated(binframe)

# Eliminate duplicates
binframe <- dplyr::distinct(binframe)


##### TESTS #####
ti <- 6

# First day and time range
testraw <- GET(url = base, path = '/api/v3/klines', 
              query = list(
                symbol = ticker,
                interval = '1m',
                startTime = start[ti, 1],
                endTime = end[ti, 1],
                limit = as.integer(1000)
              ))

# Convert to data.frame
testframe <- fromJSON(content(testraw, "text"), flatten = TRUE)

# Second day and time range + merge 1st and 2nd
testraw <- GET(url = base, path = '/api/v3/klines', 
               query = list(
                 symbol = ticker,
                 interval = '1m',
                 startTime = start[ti, 2],
                 endTime = end[ti, 2],
                 limit = as.integer(1000)
               ))
testframe <- rbind(testframe, fromJSON(content(testraw, "text"), flatten = TRUE))

anyDuplicated(testframe)
nrow(testframe) == 1440
nrow(testframe) / 1440

for (i in 39 : 43){
  for (j in 1 : f){
    binraw <- GET(url = base, path = '/api/v3/klines', 
                  query = list(
                    symbol = ticker,
                    interval = '1m',
                    startTime = start[i, j],
                    endTime = end[i, j],
                    limit = as.integer(1000)
                  ))
    testframe <- rbind(testframe, fromJSON(content(binraw, "text"), flatten = TRUE))
  }
}

anyDuplicated(testframe)
nrow(testframe)
nrow(testframe) / 1440

d <- diff(as.numeric(testframe[, 1]))

which(d > 60000)

# Known NA's: 06/03 (Thu, 03:40), 11/02 (Sat, 01:20)

rm(testframe, testraw, d)


