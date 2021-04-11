library(httr)
library(jsonlite)

rm(list = ls())

base <- "https://api.binance.com/"
coin <- 'XRP'
ticker <- paste0(coin, 'USDT')
inidate <- '2018-06-01'
enddate <- '2021-03-31'

# Days x time ranges
days <- seq(as.Date(inidate), as.Date(enddate), by = "days")

f <- 2
N <- f * length(days)


# Times for starting and ending ranges
times_start <- c('00:00:00', '12:00:00')
times_end <- c('11:59:00', '23:59:00')

# Matrix with all start and end dates in ISO8601 format
start <- matrix(ncol = f, nrow = (N/f))
end <- matrix(ncol = f, nrow = (N/f))

for (i in 1 : length(days)){
  for (j in 1 : f){
    start[i, j] <- paste0(as.numeric(as.POSIXct(paste0(days[i], ' ', times_start[j]), tz = 'UTC'))/10, '0000')
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
binix <- fromJSON(content(binraw, "text"), flatten = TRUE)

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

# Convert from character matrix to numeric
binix <- apply(binix, 2, as.numeric)

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
    nbinix <- apply(nbinix, 2, as.numeric)
    binix <- rbind(binix, nbinix)
    Sys.sleep(0.1)
  }
}

rm(nbinix, binraw)

# check for duplicates, number of obs and number of days
anyDuplicated(binix)
nrow(binix)
nrow(binix) / 1440

# Get rid of unwanted columns
binix <- binix[, c(1:6, 8:9)]

# Input column names
colnames(binix) <- c('open_time', 'open', 'high', 'low', 'close', 'volume',
                     'quote_asset_vol', 'no_trades')

# Save matrix
path = 'C:\\Users\\rodri\\OneDrive\\Documents\\Academics\\Trabalho de Conclusão de Curso\\'

save(binix, file = paste0(path, 'binix_', coin, '.RData'))

rm(list = ls())



# Exchange info
get_info <- GET(url = base, path = '/api/v3/exchangeInfo')
info <- fromJSON(content(get_info, "text"), flatten = TRUE)
info$rateLimits
