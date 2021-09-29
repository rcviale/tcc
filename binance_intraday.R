#### Import raw data matrix and unify all series #####
library(tidyverse)

ini_crypto <- readxl::read_excel(paste0('series_initial_dates.xlsx'))

# Deactivate scientific notation
options(scipen = 10000)

# Looping to change file names and save them as RDS
for (z in 1 : nrow(ini_crypto)){
  # Load data matrix
  load(paste0('Data/binix_', ini_crypto[z, 5], '.RData'))
  # Load only timestamps and closing prices as tibble
  tmp <- binix[, c('open_time', 'close')] %>%
    as_tibble()
  # Convert timestamps to seconds UNIX and make all seconds = 0
  tmp <- tmp %>%
    mutate(open_time = lubridate::as_datetime(open_time/1000 + 60) %>%
             lubridate::floor_date(unit = 'minutes')
    )
  # Save as RDS
  readr::write_rds(tmp, file = paste0('Data/', ini_crypto[z, 5], '.rds'))
}

# Function to unify all the series' closing prices in one tibble sorted by time
source('R/unify.R')
all_data <- unify(as.matrix(ini_crypto[, 5]), 'Data/', 'open_time')
all_data <- all_data %>% arrange(open_time) #FIXME
all_data %>% slice_tail(n = 10)

# Save RDS
readr::write_rds(all_data, file = 'Data/all_data.rds')



# Data summary
gazer1 <- ini_crypto[, c(2, 4, 5, 6)]
gazer1 <- gazer1 %>% 
  select(name, coin, date, nobs)

### NA's analysis ###
# Load data set
all_data <- readr::read_rds('Data/all_data.rds')

# Complete implicitly missing observations
cdata <- all_data %>%
  tidyr::separate(col = open_time, into = c('date', 'time'), sep = ' ') %>%
  tidyr::separate(col = date, into = c("year", "month", "day"), sep = "-") %>%
  tidyr::separate(col = time, into = c("hour", "minute", "second"), sep = ":") %>%
  tidyr::complete(year, month, day, hour, minute, second) %>% 
  mutate(datetime = lubridate::make_datetime(year = as.numeric(year), 
                                             month = as.numeric(month), 
                                             day = as.numeric(day), 
                                             hour = as.numeric(hour), 
                                             min = as.numeric(minute))) %>% 
  select(-year, -month, -day, -hour, -minute, -second) %>%
  select(datetime, everything()) %>% 
  dplyr::filter(datetime >= as.Date('2017-09-01'), datetime < as.Date('2021-08-01')) %>% 
  arrange(datetime, BTC) %>% 
  distinct(datetime, .keep_all = TRUE)

# Save RDS
readr::write_rds(cdata, file = "Data/cdata.rds")



# Load complete data
cdata <- readr::read_rds("Data/cdata.rds")

# Amount of NAs in the middle of each series
nas <- t(cdata %>% summarise_if(is.numeric, ~sum(is.na(.x)))) - 
  (t(cdata %>% summarise_if(is.numeric, ~which(is.na(.x) == FALSE)[1])) - 1)

gazer1 <- gazer1 %>% 
  mutate(nas = as.vector(nas),
         nobs = 1430 * 1440 - as.vector(nas),
         perc_nas = round(as.vector(nas) / nobs * 100, digits = 2)) %>%
  # select(name, coin, date, nobs, nas, perc_nas) %>% 
  rename(Name = name, Acronym = coin, `Initial Date` = date,
         N = nobs, NAs = nas, `% NAs` = perc_nas)

stargazer::stargazer(gazer1, summary = F)



#### Computation of Returns and Realized Volatility Matrices ####
# Functions to collapse series in chosen frequency, take log returns, take RV, and collapse in day
source('R/collapse_time.R')
source('R/lrets.R')
source('R/rv.R')
source('R/collapse_date.R')

# Load raw data set
all_data <- readr::read_rds('Data/all_data.rds')

# Convert series to different time frequency
all_data <- all_data %>%
  collapse_time(open_time, 5, tail, 1) # 5min, closing

# Save RDS for the future
readr::write_rds(all_data, file = "Data/all_data5.rds")

rm(collapse_time)



# Load specific data set
all_data <- readr::read_rds('Data/all_data5.rds')

# Tibble with Realized Variances
rvs <- all_data %>%
  rv() %>%
  collapse_date(open_time, 'day', sum, na.rm = TRUE) %>% 
  slice_head(n = nrow(.) -1) %>% # Tira a última linha
  rename(date = open_time)

# Save RVs RDS
readr::write_rds(rvs, file = 'Data/rvs.rds')

# Read RDS
rvs <- readr::read_rds(file = 'Data/rvs.rds')

rvs[rvs == 0] = NA

stargazer::stargazer(as.data.frame(rvs %>% select(-date)), summary = TRUE)

rm(rvs)



# Tibble with Realized Volatilities
rvols <- all_data %>%
  rv() %>%
  collapse_date(open_time, 'day', sum, na.rm = TRUE) %>% 
  slice_head(n = nrow(.) -1) %>% # Tira a última linha
  # Take square root (volatility)
  modify_if(is.numeric, .f = ~sqrt(.x)) %>% 
  rename(date = open_time)

# Save Rvols RDS
readr::write_rds(rvols, file = 'Data/rvols.rds')

# Read RDS
rvols <- readr::read_rds(file = 'Data/rvols.rds')

rvols[rvols == 0] = NA

stargazer::stargazer(as.data.frame(rvols %>% select(-date)), summary = TRUE)

rm(rv, rvols)



# Repeat process for Returns
rets <- all_data %>% 
  lrets() %>% 
  collapse_date(open_time, "day", sum, na.rm = TRUE) %>% 
  slice_head(n = nrow(.) - 1) %>% 
  rename(date = open_time)

# Save returns RDS
readr::write_rds(rets, file = 'Data/rets.rds')

# Read RDS
rets <- readr::read_rds(file = 'Data/rets.rds')

rets[rets == 0] = NA

stargazer::stargazer(as.data.frame(rets %>% select(-date)), summary = TRUE)

rm(rets, all_data, collapse_date)



#### Covariance Matrix and PCA Market Estimates #####
# Load RDS with specific time frequency
all_data <- readr::read_rds("Data/all_data5.rds")

# Take log returns, compute PCA weights and 1st component series
covs_pca <- all_data %>%
  #slice_tail(n = 2880) %>% # Slice last obs of data
  #slice_head(n = 2880) %>%
  lrets() %>% # Take log rets
  mutate(date = lubridate::as_date(open_time)) %>% # Only day column
  select(-open_time) %>%
  select(date, everything()) %>% 
  nest(data = -date) %>% # Nest according to day
  slice_head(n = nrow(.) -1) %>% # Tira a última linha
  mutate(covs    = map(.x = data, .f = ~ cov(.x, use = "pairwise.complete.obs")),
         layout  = map_dbl(.x = covs, .f = ~ sqrt(nrow(.x)^2 - sum(is.na(.x)))),
         pca     = map2(.x = covs, .y = layout, .f = ~ princomp(.x[(1 : .y), (1 : .y)])),
         mkt_ret = map_dbl(.x = pca, .f = ~ factoextra::get_eig(.x)$eigenvalue[1]),
         weights = map2(.x = pca, .y = layout, 
                        .f = ~ tibble(asset = colnames(all_data)[2 : (.y + 1)],
                                      weight = factoextra::get_pca_var(.x)$contrib[1 : .y] / 100))) %>%
  select(-c(pca, data)) %>% 
  unnest(weights) %>% 
  pivot_wider(names_from = asset, values_from = weight) %>% 
  nest(data = -c(date, covs, mkt_ret, layout)) %>% 
  mutate(act_cov  = map2(.x = covs, .y = layout, .f = ~ as.matrix(.x[1:.y, 1:.y])),
         act_wts  = map2(.x = data, .y = layout, .f = ~ as.matrix(.x[1:.y])),
         mkt_rv   = map2_dbl(.x = act_cov, .y = act_wts, .f = ~ .y %*% .x %*% t(.y)),
         mkt_rvol = sqrt(mkt_rv)) %>% 
  select(-c(layout, data, act_cov, act_wts))

# Select covariance matrices only and save RDS
covs_pca %>% 
  select(date, covs) %>% 
  readr::write_rds(file = "Data/covs.rds")

# Select the PCA market computations and save RDS
covs_pca %>% 
  select(-covs) %>% 
  readr::write_rds(file = "Data/pca_mkt.rds")

rm(list = ls())



#### Create Market Cap based Market Measures ####
# Load returns, realized volatilities and market cap based weights
rets <- readr::read_rds("Data/rets.rds")
rvols <- readr::read_rds("Data/rvols.rds")
weights <- readxl::read_excel(paste0('Data/weights.xlsx'))
covs <- readr::read_rds("Data/covs.rds")

# Pivot all longer
rets_long <- rets %>% 
  pivot_longer(-date, values_to = "ret")
weights_long <- weights %>% 
  pivot_longer(-date, values_to = "weights")

rm(rets)

# Load mkt_est function
source("R/mkt_est.R")

### Market Cap based Market Estimates ###
# Market Cap based RVol
mkt_rvol <- covs %>%
  left_join(weights, by = "date") %>% 
  mutate(layout = map_dbl(.x = covs, .f = ~ sqrt(nrow(.x)^2 - sum(is.na(.x))))) %>%
  nest(weights = -c(date, covs, layout)) %>% 
  mutate(act_covs = map2(.x = covs, .y = layout, .f = ~ .x[1:.y, 1:.y]),
         act_wts  = map2(.x = weights, .y = layout, .f = ~ .x[1:.y]),
         mkt_rvol = map2_dbl(.x = act_covs, .y = act_wts,
                             .f = ~ as.matrix(.y) %*% .x %*% t(.y))) %>% 
  select(-c(layout, covs, weights, act_covs, act_wts))

# Save Market Cap based RVol RDS
readr::write_rds(mkt_rvol, "Data/mkt_rvol.rds")

# Create Market Cap based Market Returns
mkt_ret <- mkt_est(rets_long, weights_long, ret) %>% 
  rename(mkt_ret = mkt_est)

# Save Market Cap based Returns RDS
readr::write_rds(mkt_ret, "Data/mkt_ret.rds")

rm(mkt_est, mkt_ret, mkt_rvol, rets_long, rvols, weights, weights_long, covs)


#### Create daily data tibbles ####
# Load Assets' Returns and Market estimates
rets <- readr::read_rds("Data/rets.rds")
mkt_rvol <- readr::read_rds("Data/mkt_rvol.rds")
mkt_ret <- readr::read_rds("Data/mkt_ret.rds")
pca_mkt <- readr::read_rds("Data/pca_mkt.rds")

# GARCH fit for Market estimates
gspec <- rugarch::ugarchspec(distribution.model = "std", mean.model = list(armaOrder = c(0, 0)),
                             variance.model = list(model = "sGARCH", garchOrder = c(1, 1)))
cap_garch <- rugarch::ugarchfit(gspec, mkt_ret$mkt_ret)@fit$sigma
pca_garch <- rugarch::ugarchfit(gspec, pca_mkt$mkt_ret, solver = 'hybrid')@fit$sigma

rm(gspec)

# Left join individual returns, Market Cap based Market Returns and Realized volatility
daily_data <- left_join(rets, left_join(mkt_ret, mkt_rvol, by = "date"), by = "date")

# Left join individual returns, Market Cap based Market Returns and GARCH volatility
daily_data_garch <- left_join(rets, mkt_ret, by = "date") %>% 
  mutate(mkt_rvol = cap_garch)

# Save RDS with unified Market Cap based estimates daily data
readr::write_rds(daily_data, file = "Data/daily_data.rds")
readr::write_rds(daily_data_garch, file = "Data/daily_data_garch.rds")

rm(mkt_rvol, cap_garch, mkt_ret, daily_data, daily_data_garch)


# Left join individual returns, PCA based Market Returns and Realized volatility
daily_data_pca <- left_join(rets, pca_mkt, by = "date") %>% select(-mkt_rv)

# Left join individual returns, PCA based Market Returns and GARCH volatility
daily_data_pca_garch <- left_join(rets, pca_mkt, by = "date") %>% 
  select(-mkt_rv) %>% 
  mutate(mkt_rvol = pca_garch)

# Save RDS with unified PCA Cap based estimates daily data
readr::write_rds(daily_data_pca, file = "Data/daily_data_pca.rds")
readr::write_rds(daily_data_pca_garch, file = "Data/daily_data_pca_garch.rds")

rm(list = ls())



#### Fama-MacBeth Regressions (FINALLY!) ####
# Load Market Cap and PCA based daily data tibbles
daily_data <- readr::read_rds("Data/daily_data.rds")
daily_data_garch <- readr::read_rds("Data/daily_data_garch.rds")
daily_data_pca <- readr::read_rds("Data/daily_data_pca.rds")
daily_data_pca_garch <- readr::read_rds("Data/daily_data_pca_garch.rds")

# Load pre Fama-MacBeth regression functions
source("R/pre_fmb.R")

# Check for any existing zeroes after each series start dates
daily_data %>% pre_fmb
# If returns FALSE, proceed with Fama-MacBeth regression

rm(pre_fmb)

# Load Fama-MacBeth regression functions
source("R/fmb1.R")
source("R/fmb2.R")

### Fama-MacBeth with Realized Market Cap based estimates ###
# Compute first step regressions
regs <- daily_data %>% fmb1()

# Compute second step regressions
regs2 <- daily_data %>% fmb2(beta1 = regs$estimate[seq(2, 53, 3)], 
                             beta2 = regs$estimate[seq(3, 54, 3)])

# t test for the coefficient of interest
mkt_cap_t <- t.test(regs2$estimate[seq(3, nrow(regs2), 3)])


### Fama-MacBeth with Realized PCA based estimates ###
# Compute first step regressions
regs <- daily_data_pca %>% fmb1()

# Compute second step regressions
regs2 <- daily_data_pca %>% fmb2(beta1 = regs$estimate[seq(2, 53, 3)], 
                                 beta2 = regs$estimate[seq(3, 54, 3)])

# t test for the coefficient of interest
pca_t <- t.test(regs2$estimate[seq(3, nrow(regs2), 3)])


### Fama-MacBeth with GARCH Market Cap based estimates ###
# Compute first step regressions
regs <- daily_data_garch %>% fmb1()

# Compute second step regressions
regs2 <- daily_data_garch %>% fmb2(beta1 = regs$estimate[seq(2, 53, 3)], 
                                   beta2 = regs$estimate[seq(3, 54, 3)])

# t test for the coefficient of interest
cap_garch_t <- t.test(regs2$estimate[seq(3, nrow(regs2), 3)])


### Fama-MacBeth with GARCH PCA based estimates ###
# Compute first step regressions
regs <- daily_data_pca_garch %>% fmb1()

# Compute second step regressions
regs2 <- daily_data_pca_garch %>% fmb2(beta1 = regs$estimate[seq(2, 53, 3)], 
                                 beta2 = regs$estimate[seq(3, 54, 3)])

# t test for the coefficient of interest
pca_garch_t <- t.test(regs2$estimate[seq(3, nrow(regs2), 3)])

rm(regs, regs2, fmb1, fmb2, daily_data, daily_data_garch, daily_data_pca, daily_data_pca_garch)

mkt_cap_t
cap_garch_t
pca_t
pca_garch_t

rm(list = ls())



# mu_g2 <- mean(gammas2)
# sd_g2 <- sd(gammas2)
# test_stat <- sqrt(nrow(regs2) / 3) * mu_g2 / sd_g2
# p_value <- 2 * pt(-abs(test_stat), df = nrow(regs2) - 1)










#### PLOTS ####
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
# Data frame with all combinations of lags
fcix <- as.data.frame(t(combn(1:28, 3)))
# Data frame to input RMSE and MAE
feix <- data.frame()

#   Loop for model estimation, forecasting, computation of RMSE and MAE, 
# and filling the data frame 
for (i in 1 : 1){
  # First lag
  ma1 <- sqrt(unlist(lapply(lapply(fcix[i, 3] : L, 
                                   function(t){return(ntable[(t - (fcix[i, 1] - 1)) : t, RV])}), mean)))
  # Second lag
  #ma2 <- sqrt(unlist(lapply(lapply(fcix[i, 2] : L, 
  #                          function(t){return(ma1[(t - (fcix[i, 2] - 1)) : t])}), mean)))
  ma2 <- sqrt(unlist(lapply(lapply(fcix[i, 3] : L, 
                                   function(t){return(ntable[(t - (fcix[i, 2] - 1)) : t, RV])}), mean)))
  # Third lag
  ma3 <- sqrt(unlist(lapply(lapply(fcix[i, 3] : L, 
                                   function(t){return(ntable[(t - (fcix[i, 3] - 1)) : t, RV])}), mean)))
  # Length of each lag vector
  Lma <- length(ma1)
#  L2 <- length(ma2)
 # L3 <- length(ma3)
  # New data frame with only the data that is needed for the model estimation
  nfr <- as.data.frame(cbind(y = sqrt(ntable[fcix[i, 3] : L, RV]),
                             ma1 = ma1[1 : Lma],
                             ma2 = ma2[1 : Lma],
                             ma3 = ma3[1 : Lma]))
                       #wd = ntable[fcix[i, 3] : L, wd]))
  # Forecast
  fcast <- predict1(y ~ ma1 + ma2 + ma3, nfr, M)
  
  # Filling the data frame with specification, RMSE and MAE
  feix[nrow(feix) + 1, 'Spec'] <- paste0('(', fcix[i, 1], ', ', fcix[i, 2], ', ', fcix[i, 3], ')')
  feix[nrow(feix), 'RMSE'] <- sqrt(mean((fcast - sqrt(ntable[(M + fcix[i, 3]) : L, RV]))^2))
  feix[nrow(feix), 'MAE'] <- mean(abs(fcast - sqrt(ntable[(M + fcix[i, 3]) : L, RV])))
}

rm(fcix, i, fcast, nfr, ma1, ma2, ma3, Lma, predict1)

# Save data frame
save(feix, file = paste0(path, 'fcix', tail(cols, 1), '_', M, '_', coin, f, '.RData'))
openxlsx::write.xlsx(feix, file = paste0(path, 'fcast_errors_upto_28_1-day_', M, '_', coin, f, '.xlsx'))

rm(feix)



# Function for 7-step ahead forecast
predict7 <- function(spec, data, M){
  P <- nrow(data) - M - 6
  results <- rep(0, P)
  for (i in 1 : P) {
    pred <- data[(M + i), ]
    est <- data[i : (M + i - 1), ]
    results[i] <- sum(residuals(forecast::forecast(lm(spec, data = est), newdata = pred, h = 7)))
  }
  results
}

# Testing 7-step-ahead forecast for (1, 7, 28) and (1, 7, 30) models
fcix <- data.frame()
fcix[nrow(fcix) + 2,] <- NA
fcix[, 1] <- c(1, 1)
fcix[, 2] <- c(7, 7)
fcix[, 3] <- c(28, 30)
# Data frame to input RMSE and MAE
feix <- data.frame()

#   Loop for model estimation, forecasting, computation of RMSE and MAE, 
# and filling the data frame 
for (i in 1 : nrow(fcix)){
  # First lag
  ma1 <- sqrt(unlist(lapply(lapply(fcix[i, 3] : L, 
                                   function(t){return(ntable[(t - (fcix[i, 1] - 1)) : t, RV])}), mean)))
  # Second lag
  #ma2 <- sqrt(unlist(lapply(lapply(fcix[i, 2] : L, 
  #                          function(t){return(ma1[(t - (fcix[i, 2] - 1)) : t])}), mean)))
  ma2 <- sqrt(unlist(lapply(lapply(fcix[i, 3] : L, 
                                   function(t){return(ntable[(t - (fcix[i, 2] - 1)) : t, RV])}), mean)))
  # Third lag
  ma3 <- sqrt(unlist(lapply(lapply(fcix[i, 3] : L, 
                                   function(t){return(ntable[(t - (fcix[i, 3] - 1)) : t, RV])}), mean)))
  # Length of each lag vector
  Lma <- length(ma1)
  #  L2 <- length(ma2)
  # L3 <- length(ma3)
  # New data frame with only the data that is needed for the model estimation
  nfr <- as.data.frame(cbind(y = sqrt(ntable[fcix[i, 3] : L, RV]),
                             ma1 = ma1[1 : Lma],
                             ma2 = ma2[1 : Lma],
                             ma3 = ma3[1 : Lma]))
  #wd = ntable[fcix[i, 3] : L, wd]))
  # Forecast
  fcast <- predict7(y ~ ma1 + ma2 + ma3, nfr, M)
  
  # Filling the data frame with specification, RMSE and MAE
  feix[nrow(feix) + 1, 'Spec'] <- paste0('(', fcix[i, 1], ', ', fcix[i, 2], ', ', fcix[i, 3], ')')
  feix[nrow(feix), 'RMSE'] <- sqrt(mean((fcast - sqrt(ntable[(M + fcix[i, 3]) : (L - 6), RV]))^2))
  feix[nrow(feix), 'MAE'] <- mean(abs(fcast - sqrt(ntable[(M + fcix[i, 3]) : (L - 6), RV])))
}

rm(fcix, i, fcast, nfr, ma1, ma2, ma3, Lma, predict7)

# Save data frame
openxlsx::write.xlsx(feix, file = paste0(path, 'fcast_errors_7-day_', M, '_', coin, f, '.xlsx'))

rm(feix)




# Function for 28-step ahead forecast
predict28 <- function(spec, data, M){
  P <- nrow(data) - M - 27
  results <- rep(0, P)
  for (i in 1 : P) {
    pred <- data[(M + i), ]
    est <- data[i : (M + i - 1), ]
    results[i] <- sum(residuals(forecast::forecast(lm(spec, data = est), newdata = pred, h = 28)))
  }
  results
}

# Testing 28-step-ahead forecast for (1, 7, 28) and (1, 7, 30) models
fcix <- data.frame()
fcix[nrow(fcix) + 2,] <- NA
fcix[, 1] <- c(1, 1)
fcix[, 2] <- c(7, 7)
fcix[, 3] <- c(28, 30)
# Data frame to input RMSE and MAE
feix <- data.frame()

#   Loop for model estimation, forecasting, computation of RMSE and MAE, 
# and filling the data frame 
for (i in 1 : nrow(fcix)){
  # First lag
  ma1 <- sqrt(unlist(lapply(lapply(fcix[i, 3] : L, 
                                   function(t){return(ntable[(t - (fcix[i, 1] - 1)) : t, RV])}), mean)))
  # Second lag
  #ma2 <- sqrt(unlist(lapply(lapply(fcix[i, 2] : L, 
  #                          function(t){return(ma1[(t - (fcix[i, 2] - 1)) : t])}), mean)))
  ma2 <- sqrt(unlist(lapply(lapply(fcix[i, 3] : L, 
                                   function(t){return(ntable[(t - (fcix[i, 2] - 1)) : t, RV])}), mean)))
  # Third lag
  ma3 <- sqrt(unlist(lapply(lapply(fcix[i, 3] : L, 
                                   function(t){return(ntable[(t - (fcix[i, 3] - 1)) : t, RV])}), mean)))
  # Length of each lag vector
  Lma <- length(ma1)
  #  L2 <- length(ma2)
  # L3 <- length(ma3)
  # New data frame with only the data that is needed for the model estimation
  nfr <- as.data.frame(cbind(y = sqrt(ntable[fcix[i, 3] : L, RV]),
                             ma1 = ma1[1 : Lma],
                             ma2 = ma2[1 : Lma],
                             ma3 = ma3[1 : Lma]))
  #wd = ntable[fcix[i, 3] : L, wd]))
  # Forecast
  fcast <- predict28(y ~ ma1 + ma2 + ma3, nfr, M)
  
  # Filling the data frame with specification, RMSE and MAE
  feix[nrow(feix) + 1, 'Spec'] <- paste0('(', fcix[i, 1], ', ', fcix[i, 2], ', ', fcix[i, 3], ')')
  feix[nrow(feix), 'RMSE'] <- sqrt(mean((fcast - sqrt(ntable[(M + fcix[i, 3]) : (L - 27), RV]))^2))
  feix[nrow(feix), 'MAE'] <- mean(abs(fcast - sqrt(ntable[(M + fcix[i, 3]) : (L - 27), RV])))
}

rm(fcix, i, fcast, nfr, ma1, ma2, ma3, Lma, predict28)

# Save data frame
openxlsx::write.xlsx(feix, file = paste0(path, 'fcast_errors_28-day_', M, '_', coin, f, '.xlsx'))

rm(feix)



# Function for 28-step ahead forecast
predict30 <- function(spec, data, M){
  P <- nrow(data) - M - 29
  results <- rep(0, P)
  for (i in 1 : P) {
    pred <- data[(M + i), ]
    est <- data[i : (M + i - 1), ]
    results[i] <- sum(residuals(forecast::forecast(lm(spec, data = est), newdata = pred, h = 30)))
  }
  results
}

# Testing 28-step-ahead forecast for (1, 7, 28) and (1, 7, 30) models
fcix <- data.frame()
fcix[nrow(fcix) + 2,] <- NA
fcix[, 1] <- c(1, 1)
fcix[, 2] <- c(7, 7)
fcix[, 3] <- c(28, 30)
# Data frame to input RMSE and MAE
feix <- data.frame()

#   Loop for model estimation, forecasting, computation of RMSE and MAE, 
# and filling the data frame 
for (i in 1 : nrow(fcix)){
  # First lag
  ma1 <- sqrt(unlist(lapply(lapply(fcix[i, 3] : L, 
                                   function(t){return(ntable[(t - (fcix[i, 1] - 1)) : t, RV])}), mean)))
  # Second lag
  #ma2 <- sqrt(unlist(lapply(lapply(fcix[i, 2] : L, 
  #                          function(t){return(ma1[(t - (fcix[i, 2] - 1)) : t])}), mean)))
  ma2 <- sqrt(unlist(lapply(lapply(fcix[i, 3] : L, 
                                   function(t){return(ntable[(t - (fcix[i, 2] - 1)) : t, RV])}), mean)))
  # Third lag
  ma3 <- sqrt(unlist(lapply(lapply(fcix[i, 3] : L, 
                                   function(t){return(ntable[(t - (fcix[i, 3] - 1)) : t, RV])}), mean)))
  # Length of each lag vector
  Lma <- length(ma1)
  #  L2 <- length(ma2)
  # L3 <- length(ma3)
  # New data frame with only the data that is needed for the model estimation
  nfr <- as.data.frame(cbind(y = sqrt(ntable[fcix[i, 3] : L, RV]),
                             ma1 = ma1[1 : Lma],
                             ma2 = ma2[1 : Lma],
                             ma3 = ma3[1 : Lma]))
  #wd = ntable[fcix[i, 3] : L, wd]))
  # Forecast
  fcast <- predict30(y ~ ma1 + ma2 + ma3, nfr, M)
  
  # Filling the data frame with specification, RMSE and MAE
  feix[nrow(feix) + 1, 'Spec'] <- paste0('(', fcix[i, 1], ', ', fcix[i, 2], ', ', fcix[i, 3], ')')
  feix[nrow(feix), 'RMSE'] <- sqrt(mean((fcast - sqrt(ntable[(M + fcix[i, 3]) : (L - 29), RV]))^2))
  feix[nrow(feix), 'MAE'] <- mean(abs(fcast - sqrt(ntable[(M + fcix[i, 3]) : (L - 29), RV])))
}

rm(fcix, i, fcast, nfr, ma1, ma2, ma3, Lma, predict30)

# Save data frame
openxlsx::write.xlsx(feix, file = paste0(path, 'fcast_errors_30-day_', M, '_', coin, f, '.xlsx'))

rm(feix, M)




### Specific HAR Construction ###
# Time windows do be used
short <- 7
long <- 30

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
rvt <- sqrt(ntable[31 : L, RV])
rvt_1 <- sqrt(ntable[30 : (L - 1), RV])
nrvols <- rvols[24 : (Ls - 1)]
nrvoll <- rvoll[1 : (Ll - 1)]

df_train <- data.frame(rvt[1:730], rvt_1[1:730], nrvols[1:730], nrvoll[1:730])
df_for <- data.frame(rvt[731:1278], rvt_1[731:1278], nrvols[731:1278], nrvoll[731:1278])
colnames(df_train) <- c('rvt', 'rvt_1', 'nrvols', 'nrvoll')
colnames(df_for) <- c('rvt', 'rvt_1', 'nrvols', 'nrvoll')

nn <- neuralnet(rvt ~ rvt_1 + nrvols + nrvoll, data = df, 
                hidden = 5, act.fct = "logistic", linear.output = FALSE,
                rep = 1) # Set df to training period

Pred <- compute(nn, df_for)$net.result # Set df to forecast period


plot(nn)
