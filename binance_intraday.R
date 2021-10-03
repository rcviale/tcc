#### Import raw data matrix and unify all series #####
library(tidyverse)

ini_crypto <- readxl::read_excel(paste0('series_initial_dates.xlsx'))
ini_crypto <- readxl::read_excel(paste0('new_initial_dates.xlsx'))

# Deactivate scientific notation
options(scipen = 10000)

# Looping to change file names and save them as RDS
for (z in 1 : nrow(ini_crypto)){
  # Load data matrix
  load(paste0('Data/binix_', ini_crypto[z, 2], '.RData'))
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
all_data <- unify(as.matrix(ini_crypto[, 2]), 'Data/', 'open_time')
colnames(all_data) <- c("open_time", as.matrix(ini_crypto[, 2]))
all_data <- all_data %>% arrange(open_time) #FIXME
all_data %>% slice_tail(n = 10)

# Save RDS
readr::write_rds(all_data, file = 'Data/new_all_data.rds')



# Data summary
gazer1 <- ini_crypto[, c(2, 4, 5, 6)]
gazer1 <- gazer1 %>% 
  select(name, coin, date, nobs)

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

rm(cdata)

gazer1 <- gazer1 %>% 
  mutate(nas = as.vector(nas),
         nobs = 1430 * 1440 - as.vector(nas),
         perc_nas = round(as.vector(nas) / nobs * 100, digits = 2)) %>%
  # select(name, coin, date, nobs, nas, perc_nas) %>% 
  rename(Name = name, Acronym = coin, `Initial Date` = date,
         N = nobs, NAs = nas, `% NAs` = perc_nas)

readr::write_rds(gazer1, file = "Print/table3.1.rds")

stargazer::stargazer(gazer1, summary = F)



#### Computation of Returns and Realized Volatility Matrices ####
# Functions to collapse series in chosen frequency, take log returns, take RV, and collapse in day
source('R/collapse_time.R')
source('R/lrets.R')
source('R/rv.R')
source('R/collapse_date.R')

path <- "Data/new_all_data.rds"

# Load raw data set
all_data <- readr::read_rds(path)

# Convert series to different time frequency
all_data <- all_data %>%
  collapse_time(open_time, 5, tail, 1) # 5min, closing

npath <- "Data/new_all_data5.rds"

# Save RDS for the future
readr::write_rds(all_data, file = npath)

rm(collapse_time)



# Load specific data set
all_data <- readr::read_rds(npath)



# Tibble with daily returns
rets <- all_data %>% 
  lrets() %>% 
  collapse_date(open_time, "day", sum, na.rm = TRUE) %>% 
  slice_head(n = nrow(.) - 1) %>% 
  rename(date = open_time)

rets[rets == 0] = NA

# Save returns RDS
readr::write_rds(rets, file = 'Data/rets.rds')

# Read RDS
rets <- readr::read_rds(file = 'Data/rets.rds')

tablea13 <- rets %>%
  select(-date) %>% 
  summarise(across(everything(), stats, .names = "{.fn}_{.col}")) %>% 
  pivot_longer(cols = everything(), values_to = "value") %>% 
  separate(col = name, into = c("m", "cur"), sep = "_") %>% 
  pivot_wider(names_from = m, values_from = value) %>% 
  select(cur, min, med, mean, max, var)

readr::write_rds(tablea13, file = "Print/tablea13.rds")



# Tibble with Realized Variances
rvs <- all_data %>%
  rv() %>%
  collapse_date(open_time, 'day', sum, na.rm = TRUE) %>% 
  slice_head(n = nrow(.) -1) %>% # Tira a última linha
  rename(date = open_time)

# Replace 0 for NA
rvs[rvs == 0] = NA

# Save RVs RDS
readr::write_rds(rvs, file = 'Data/rvs.rds')

# Read RDS
rvs <- readr::read_rds(file = 'Data/rvs.rds')

stats <- list(
  min  = ~min(.x, na.rm = T),
  med  = ~median(.x, na.rm = T),
  mean = ~mean(.x, na.rm = T),
  max  = ~max(.x, na.rm = T),
  var  = ~var(.x, na.rm = T)
)

tablea11 <- rvs %>%
  select(-date) %>% 
  summarise(across(everything(), stats, .names = "{.fn}_{.col}")) %>% 
  pivot_longer(cols = everything(), values_to = "value") %>% 
  separate(col = name, into = c("m", "cur"), sep = "_") %>% 
  pivot_wider(names_from = m, values_from = value) %>% 
  select(cur, min, med, mean, max, var)

readr::write_rds(tablea11, file = "Print/tablea11.rds")

rm(tablea11, rvs)



# Tibble with Realized Volatilities
rvols <- all_data %>%
  rv() %>%
  collapse_date(open_time, 'day', sum, na.rm = TRUE) %>% 
  slice_head(n = nrow(.) -1) %>% # Tira a última linha
  # Take square root (volatility)
  modify_if(is.numeric, .f = ~sqrt(.x)) %>% 
  rename(date = open_time)

rvols[rvols == 0] = NA

# Save Rvols RDS
readr::write_rds(rvols, file = 'Data/rvols.rds')

# Read RDS
rvols <- readr::read_rds(file = 'Data/rvols.rds')

tablea12 <- rvols %>%
  select(-date) %>% 
  summarise(across(everything(), stats, .names = "{.fn}_{.col}")) %>% 
  pivot_longer(cols = everything(), values_to = "value") %>% 
  separate(col = name, into = c("m", "cur"), sep = "_") %>% 
  pivot_wider(names_from = m, values_from = value) %>% 
  select(cur, min, med, mean, max, var)

readr::write_rds(tablea12, file = "Print/tablea12.rds")



#### Covariance Matrix and PCA Market Estimates #####
# Load RDS with specific time frequency
npath <- "Data/new_all_data5.rds"
all_data <- readr::read_rds(npath)

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

# Daily covariance matrix
cov_daily <- all_data %>%
  lrets() %>% 
  collapse_date(open_time, "day", sum, na.rm = TRUE) %>% 
  # nest(data = -open_time) %>% # Nest according to day
  slice_head(n = nrow(.) -1) %>% 
  na_if(0) %>% 
  # group_by(open_time) %>%
  select(-open_time) %>% 
  cov(use = "complete.obs")

readr::write_rds(cov_daily, "Data/cov_daily.rds")  

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
source("R/fmb_t.R")

### Fama-MacBeth with Realized Market Cap based estimates ###
# Compute first step regressions
regs <- daily_data %>% fmb1()

b1 <- regs %>% filter(term == "mkt_ret") %>% select(estimate) %>% unlist()
b2 <- regs %>% filter(term == "mkt_rvol") %>% select(estimate) %>% unlist()

# Compute second step regressions
regs2 <- daily_data %>% fmb2(beta1 = b1, beta2 = b2, K = 10)

# t test for the coefficient of interest
cap_t <- regs2 %>% fmb_t("beta1", "beta2")



### Fama-MacBeth with Realized PCA based estimates ###
# Compute first step regressions
regs <- daily_data_pca %>% fmb1()

b1 <- regs %>% filter(term == "mkt_ret") %>% select(estimate) %>% unlist()
b2 <- regs %>% filter(term == "mkt_rvol") %>% select(estimate) %>% unlist()

# Compute second step regressions
regs2 <- daily_data_pca %>% fmb2(beta1 = b1, beta2 = b2, K = 10)

# t test for the coefficient of interest
pca_t <- regs2 %>% fmb_t("beta1", "beta2")



### Fama-MacBeth with GARCH Market Cap based estimates ###
# Compute first step regressions
regs <- daily_data_garch %>% fmb1()

b1 <- regs %>% filter(term == "mkt_ret") %>% select(estimate) %>% unlist()
b2 <- regs %>% filter(term == "mkt_rvol") %>% select(estimate) %>% unlist()

# Compute second step regressions
regs2 <- daily_data_garch %>% fmb2(beta1 = b1, beta2 = b2, K = 10)

# t test for the coefficient of interest
cap_garch_t <- regs2 %>% fmb_t("beta1", "beta2")



### Fama-MacBeth with GARCH PCA based estimates ###
# Compute first step regressions
regs <- daily_data_pca_garch %>% fmb1()

b1 <- regs %>% filter(term == "mkt_ret") %>% select(estimate) %>% unlist()
b2 <- regs %>% filter(term == "mkt_rvol") %>% select(estimate) %>% unlist()

# Compute second step regressions
regs2 <- daily_data_pca_garch %>% fmb2(beta1 = b1, beta2 = b2, K = 10)

# t test for the coefficient of interest
pca_garch_t <- regs2 %>% fmb_t("beta1", "beta2")

rm(regs, regs2, fmb1, fmb2, fmb_t, daily_data, daily_data_garch, daily_data_pca, daily_data_pca_garch,
   b1, b2)

cap_t
pca_t
cap_garch_t
pca_garch_t

rm(list = ls())



# mu_g2 <- mean(gammas2)
# sd_g2 <- sd(gammas2)
# test_stat <- sqrt(nrow(regs2) / 3) * mu_g2 / sd_g2
# p_value <- 2 * pt(-abs(test_stat), df = nrow(regs2) - 1)










