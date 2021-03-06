library(httr)
library(jsonlite)

rm(list = ls())

##### Checking each series' start date #####
base <- "https://api.binance.com/"
coins <- c('BTC', 'ETH', 'LTC', 'XRP', 'COMP', 'BNB', 'ADA', 'DOGE', 'DOT', 'PNT',
           'LINK', 'BCH', 'UNI', 'XLM', 'FIL', 'EOS', 'SOL', 'MATIC')

coins <- c('BTC', 'ETH', 'ADA', 'BNB', 'XRP', 'DOGE', 'LTC', 'ALGO', 'ATOM', 'MATIC')
tickers <- paste0(coins, 'USDT')
inidate <- '2017-08-01'
enddate <- '2021-07-31'

# Months
mths <- seq(as.Date(inidate), as.Date(enddate), by = "months")
ini_mths <- matrix(nrow = length(mths))
end_mths <- matrix(nrow = length(mths))

# Loop to create one date in each month of the time period
for (i in 1 : length(mths)){
  ini_mths[i, 1] <- paste0(as.numeric(as.POSIXct(paste0(mths[i], ' 00:00:00'), tz = 'UTC'))/10, '0000')
  end_mths[i, 1] <- paste0(as.numeric(as.POSIXct(paste0(mths[i], ' 11:59:00'), tz = 'UTC'))/10, '0000')
}

rm(mths)

ini_crypto <- data.frame()

# Loop to check initial date of each crypto, creating a data frame with the info
for (j in 1 : length(tickers)){
  for (i in 2 : length(ini_mths)){
    binraw <- GET(url = base, path = '/api/v3/klines', 
                  query = list(
                    symbol = tickers[j],
                    interval = '1m',
                    startTime = ini_mths[i],
                    endTime = end_mths[i],
                    limit = as.integer(1000)
                  ))
    binix <- fromJSON(content(binraw, "text"), flatten = TRUE)
    Sys.sleep(0.3)
    if (length(binix) == 0){
      next
    } else{
      print(anytime::anytime(as.numeric(ini_mths[i])/1000, asUTC = TRUE))
      ini_crypto[nrow(ini_crypto) + 1, 'market'] <- tickers[j]
      ini_crypto[nrow(ini_crypto), 'coin'] <- coins[j]
      ini_crypto[nrow(ini_crypto), 'start date'] <- anytime::anytime(as.numeric(ini_mths[i])/1000, asUTC = TRUE)
      break
    }
  }
}

path <- 'C:\\Users\\rodri\\OneDrive\\Documents\\Academics\\Trabalho de Conclusão de Curso\\'
openxlsx::write.xlsx(ini_crypto, file = paste0(path, 'new_initial_dates.xlsx'))

rm(binix, binraw, end_mths, ini_mths, ini_crypto, i, j, tickers, coins)



##### Start downloading multiple series #####
path <- 'C:\\Users\\rodri\\OneDrive\\Documents\\Academics\\Trabalho de Conclusão de Curso\\tcc\\'
base <- "https://api.binance.com/"
inidate <- '2017-09-01'
enddate <- '2017-09-02'

# Open initial dates
ini_crypto <- as.data.frame(readxl::read_excel(paste0(path, 'new_initial_dates.xlsx')))

# Create times for starting and ending ranges
times_start <- c('00:00:00', '12:00:00')
times_end <- c('11:59:00', '23:59:00')

# Loop through every series
for (z in 1 : 1){
  # Days range
  days <- seq(as.Date(ini_crypto[z, 3]), as.Date(enddate), by = "days")
  # Requests per day and total number of requests
  f <- 2
  N <- f * length(days)
  
  # Matrix with all start and end dates in ISO8601 format
  start <- matrix(ncol = f, nrow = (N/f))
  end <- matrix(ncol = f, nrow = (N/f))
  
  # Loop creating all combinations of dates and times
  for (i in 1 : length(days)){
    for (j in 1 : f){
      start[i, j] <- sub('\\.', '', paste0(as.numeric(as.POSIXct(paste0(days[i], ' ', 
                                                         times_start[j]), tz = 'UTC'))/100, '00000'))
      end[i, j] <- sub('\\.', '', paste0(as.numeric(as.POSIXct(paste0(days[i], ' ', 
                                                       times_end[j]), tz = 'UTC'))/10, '0000'))
    }
  }
  
  print(length(which(nchar(start[, 1]) != 13)))
  print(length(which(nchar(start[, 2]) != 13)))
  print(length(which(nchar(end[, 1]) != 13)))
  print(length(which(nchar(end[, 2]) != 13)))
  
  # First day and time range
  binraw <- GET(url = base, path = '/api/v3/klines', 
                query = list(
                  symbol = ini_crypto[z, 1],
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
                   symbol = ini_crypto[z, 1],
                   interval = '1m',
                   startTime = start[1, 2],
                   endTime = end[1, 2],
                   limit = as.integer(1000)
                 ))
  
  # Convert to matrix
  binix2 <- fromJSON(content(binraw2, "text"), flatten = TRUE)
  
  # Merge two time ranges of first day
  binix <- rbind(binix, binix2)
  
  rm(binix2)
  # Convert from character matrix to numeric
  binix <- apply(binix, 2, as.numeric)
  
  # Download the remaining data, merging it with the first day
  for (i in 2 : nrow(start)){
    for (j in 1 : f){
      binraw <- GET(url = base, path = '/api/v3/klines', 
                    query = list(
                      symbol = ini_crypto[z, 1],
                      interval = '1m',
                      startTime = start[i, j],
                      endTime = end[i, j],
                      limit = as.integer(1000)
                    ))
      nbinix <- fromJSON(content(binraw, "text"), flatten = TRUE)
      if (is.null(dim(nbinix)) == FALSE){
        nbinix <- apply(nbinix, 2, as.numeric)
        binix <- rbind(binix, nbinix)
        Sys.sleep(0.1)
      }
    }
  }
  
  # Get rid of unwanted columns
  binix <- binix[, c(1:6, 8:9)]
  # Input column names
  colnames(binix) <- c('open_time', 'open', 'high', 'low', 'close', 'volume',
                       'quote_asset_vol', 'no_trades')
  # Save matrix
  path = 'C:\\Users\\rodri\\OneDrive\\Documents\\Academics\\Trabalho de Conclusão de Curso\\'
  readr::write_rds(binix, file = paste0(path, ini_crypto[z, 2], '.rds'))

  print(ini_crypto[z, 2])
}

rm(list = ls())



##### Download only one series #####
# Days x time ranges
base <- "https://api.binance.com/"
coin <- 'BTC'
ticker <- paste0(coin, 'USDT')
days <- seq(as.Date(inidate), as.Date(enddate), by = "days")

rm(inidate, enddate)

f <- 2
N <- f * length(days)


# Matrix with all start and end dates in ISO8601 format
start <- matrix(ncol = f, nrow = (N/f))
end <- matrix(ncol = f, nrow = (N/f))

for (i in 1 : length(days)){
  for (j in 1 : f){
    start[i, j] <- paste0(as.numeric(as.POSIXct(paste0(days[i], ' ', times_start[j]), tz = 'UTC'))/10, '0000')
    end[i, j] <- paste0(as.numeric(as.POSIXct(paste0(days[i], ' ', times_end[j]), tz = 'UTC'))/10, '0000')
  }
}

rm(times_start, times_end, days)

# First day and time range
binraw <- httr::GET(url = "https://api.binance.com/", path = '/api/v3/klines', 
              query = list(
                symbol = 'BTCUSDT',
                interval = '1m',
                startTime = start[1, 1],
                endTime = end[1, 1],
                limit = as.integer(1000)
              ))

# Convert from JSON to matrix
bintest <- jsonlite::fromJSON(httr::content(binraw, "text"), flatten = TRUE)

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
for (i in 2 : nrow(start)){
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
    if (is.null(dim(nbinix)) == FALSE){
      nbinix <- apply(nbinix, 2, as.numeric)
      binix <- rbind(binix, nbinix)
      Sys.sleep(0.1)
    }
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
