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
    mutate(open_time = lubridate::as_datetime(open_time/1000) %>%
             lubridate::floor_date(unit = 'minutes')
    )
  # Save as RDS
  readr::write_rds(tmp, file = paste0('Data/', ini_crypto[z, 5], '.rds'))
}

# Function to unify all the series' closing prices in one tibble sorted by time
source('R/unify.R')
all_data <- unify(files = as.matrix(ini_crypto[, 2]), directory = 'Data/', col = 'open_time')
colnames(all_data) <- c("open_time", as.matrix(ini_crypto[, 2]))
all_data <- all_data %>% arrange(open_time) #FIXME
all_data %>% slice_tail(n = 10)

# Save RDS
readr::write_rds(all_data, file = 'Data/new_all_data.rds')



#### Raw data analysis and summary statistics ####
all_data <- readr::read_rds("Data/new_all_data.rds")

# Summary Timestamps Statistics
stats <- list(
  obs  = ~length(.x),
  min  = ~min(.x, na.rm = T),
  med  = ~median(.x, na.rm = T),
  mean = ~round(mean(.x, na.rm = T), digits = 2),
  max  = ~max(.x, na.rm = T),
  var  = ~round(var(.x, na.rm = T), digits = 2),
  sd   = ~round(sd(.x, na.rm = T), digits = 2),
  q1   = ~quantile(t(.x), na.rm = T)[2],
  q3   = ~quantile(t(.x), na.rm = T)[4]
)

difs <- all_data$open_time %>% 
  diff() - 1 #%>% e
difs <- difs %>% 
  as_tibble() %>% 
  filter(value != 0) %>% 
  summarise(across(everything(), stats, .names = "{.fn}")) %>% 
  t()

hole_summ_table <- tibble(Metric = c("Ocurrences", "Minimum", "Median", "Mean", "Maximum", "Variance", 
                  "Standard Deviation", "1st Quantile", "3rd Quantile"),
       Value  = as.vector(difs))

readr::write_rds(hole_summ_table, "Print/hole_summ_table.rds")

rm(stats, difs, hole_summ_table)



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
  arrange(datetime) %>%
  dplyr::filter(datetime >= as.Date('2017-09-01'), datetime < as.Date('2021-08-01')) %>% 
  arrange(datetime, BTC) %>% 
  distinct(datetime, .keep_all = TRUE)

rm(all_data)

# Save RDS
readr::write_rds(cdata, file = "Data/cdata.rds")

# Fill data and save
cdata %>% fill(2:11, .direction = "down") %>% 
  readr::write_rds("Data/fdata.rds")

# Take last observation of filled data before working sample starts
readr::read_rds("Data/fdata.rds") %>% 
  filter(datetime >= as.Date("2019-07-31"), datetime < as.Date("2019-08-01")) %>% 
  slice_tail(n = 5) -> last

# Restrict data, fill and save
readr::read_rds("Data/cdata.rds") %>%
  filter(datetime >= as.Date("2019-08-01"), datetime < as.Date("2021-08-01")) %>% 
  fill(2:11, .direction = "down") %>%
  rbind(last) %>% 
  arrange(datetime) %>% 
  readr::write_rds("Data/rest_fdata.rds")

rm(list = ls())



# Load complete data
cdata <- readr::read_rds("Data/cdata.rds")
ini_crypto <- readxl::read_excel(paste0('new_initial_dates.xlsx'))

# Summary table of original sample
full_summ_table <- ini_crypto %>% 
  mutate(nas      = as.vector(t(cdata %>% summarise_if(is.numeric, ~sum(is.na(.x)))) - 
                                (t(cdata %>% summarise_if(is.numeric, ~which(is.na(.x) == FALSE)[1])) - 1)),
         nobs     = cdata %>% summarise_if(is.numeric, ~sum(is.na(.x) == FALSE)) %>% t() %>% as.vector(),
         perc_nas = round(nas / nobs * 100, digits = 2),
         Name     = c("Bitcoin", "Ethereum", "Binance Coin", "Litecoin", "Cardano",
                  "Ripple", "Cosmos", "Polygon", "Algorand", "Dogecoin"),
         `start date` = lubridate::as_date(`start date`)) %>%
  rename(Acronym = coin, N = nobs, NAs = nas, `% NAs` = perc_nas,
         `Initial Date` = `start date`) %>% 
  select(-market) %>% 
  select(Name, everything())

readr::write_rds(full_summ_table, file = "Print/full_summ_table.rds")

rm(full_summ_table)



# Summary table of restricted completed sample
cdata <- cdata %>% 
  filter(datetime >= as.Date("2019-08-01"), datetime < as.Date("2021-08-01"))

rest_summ_table <- ini_crypto %>% 
  mutate(nas      = as.vector(t(cdata %>% summarise_if(is.numeric, ~sum(is.na(.x)))) - 
                                (t(cdata %>% summarise_if(is.numeric, ~which(is.na(.x) == FALSE)[1])) - 1)),
         nobs     = cdata %>% summarise_if(is.numeric, ~sum(is.na(.x) == FALSE)) %>% t() %>% as.vector(),
         perc_nas = round(nas / nobs * 100, digits = 2),
         Name     = c("Bitcoin", "Ethereum", "Binance Coin", "Litecoin", "Cardano",
                      "Ripple", "Cosmos", "Polygon", "Algorand", "Dogecoin")) %>%
  rename(Acronym = coin, `Initial Date` = `start date`,
         N = nobs, NAs = nas, `% NAs` = perc_nas) %>% 
  select(-c(market, `Initial Date`)) %>% 
  select(Name, everything())

readr::write_rds(rest_summ_table, file = "Print/rest_summ_table.rds")

rm(list = ls())



#### Computation of Returns and Realized Volatility Matrices ####
# Functions to collapse series in chosen frequency, take log returns, take RV, and collapse in day
source('R/collapse_time.R')
source('R/lrets.R')
source('R/rv.R')
source('R/collapse_date.R')

# Load restricted data set
all_data <- readr::read_rds("Data/fdata.rds")

# Convert series to different time frequency
all_data <- all_data %>%
  collapse_time(datetime, 5, tail, 1) %>% # 5min, tail = closing
  slice_head(n = nrow(.) - 1)

# Save RDS for the future
readr::write_rds(all_data, file = "Data/fdata5.rds")

rm(collapse_time)



# Load specific data set
all_data <- readr::read_rds("Data/fdata5.rds")

# Cumulative log returns
cum_rets <- all_data %>% 
  lrets() %>% 
  slice_tail(n = nrow(.) - 1) %>%
  collapse_date(datetime, "day", sum, na.rm = TRUE) %>% 
  modify_if(is.numeric, .f = ~cumsum(.x) * 100)

cum_rets[cum_rets == 0] <- NA

# Full sample cumulative log returns
cum_rets %>% 
  reshape2::melt(id = "datetime") %>% 
  rename(Asset = variable) %>% 
  ggplot(aes(x = datetime, y = value, colour = Asset, group = Asset)) + 
  geom_line() + 
  labs(x = "Days",
       y = "Cumulative Log-Return") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

# Actual sample cumulative log returns
cum_rets %>% 
  filter(datetime >= as.Date("2019-08-01")) %>% 
  reshape2::melt(id = "datetime") %>% 
  rename(Asset = variable) %>% 
  ggplot(aes(x = datetime, y = value, colour = Asset, group = Asset)) + 
  geom_line() + 
  labs(x = "Days",
       y = "Cumulative Log-Return") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))



# Tibble with daily returns
rets <- all_data %>% 
  lrets() %>% 
  slice_tail(n = nrow(.) - 1) %>%
  collapse_date(datetime, "day", sum, na.rm = TRUE) %>% 
  rename(date = datetime)

rets[rets == 0] <- NA

# Save returns RDS
readr::write_rds(rets, file = 'Data/rets.rds')

# Read RDS
rets <- readr::read_rds(file = 'Data/rets.rds')

stats <- list(
  min  = ~min(.x, na.rm = T),
  med  = ~median(.x, na.rm = T),
  mean = ~mean(.x, na.rm = T),
  max  = ~max(.x, na.rm = T),
  var  = ~var(.x, na.rm = T),
  sd   = ~sd(.x, na.rm = T),
  q1   = ~quantile(t(.x), na.rm = T)[2],
  q3   = ~quantile(t(.x), na.rm = T)[4]
)

# Summary stats for returns
rets %>%
  select(-date) %>% 
  summarise(across(everything(), stats, .names = "{.fn}_{.col}")) %>% 
  pivot_longer(cols = everything(), values_to = "value") %>% 
  separate(col = name, into = c("m", "cur"), sep = "_") %>% 
  pivot_wider(names_from = m, values_from = value) %>% 
  select(cur, min, med, mean, max, var, sd, q1, q3) %>% 
  readr::write_rds(file = "Print/tablea13.rds")

rm(rets, lrets)



# Tibble with Realized Variances
rvs <- all_data %>%
  rv() %>%
  slice_tail(n = nrow(.) - 1) %>% 
  collapse_date(datetime, 'day', sum, na.rm = TRUE) %>% 
  rename(date = datetime)

rvs[rvs == 0] <- NA

# Save RVs RDS
readr::write_rds(rvs, file = 'Data/rvs.rds')

# Read RDS
rvs <- readr::read_rds(file = 'Data/rvs.rds')

rvs %>%
  select(-date) %>% 
  summarise(across(everything(), stats, .names = "{.fn}_{.col}")) %>% 
  pivot_longer(cols = everything(), values_to = "value") %>% 
  separate(col = name, into = c("m", "cur"), sep = "_") %>% 
  pivot_wider(names_from = m, values_from = value) %>% 
  select(cur, min, med, mean, max, var, sd, q1, q3) %>% 
  readr::write_rds(file = "Print/tablea11.rds")



# Tibble with Realized Volatilities
rvols <- rvs %>% 
  modify_if(is.numeric, ~sqrt(.x))

# Save Rvols RDS
readr::write_rds(rvols, file = 'Data/rvols.rds')

# Read RDS
rvols <- readr::read_rds(file = 'Data/rvols.rds')

rvols %>%
  select(-date) %>% 
  summarise(across(everything(), stats, .names = "{.fn}_{.col}")) %>% 
  pivot_longer(cols = everything(), values_to = "value") %>% 
  separate(col = name, into = c("m", "cur"), sep = "_") %>% 
  pivot_wider(names_from = m, values_from = value) %>% 
  select(cur, min, med, mean, max, var, sd, q1, q3) %>% 
  readr::write_rds(file = "Print/tablea12.rds")

rm(list = ls())



#### Covariance Matrix and PCA Market Estimates #####
# Load RDS with specific time frequency
all_data <- readr::read_rds("Data/fdata5.rds")
source('R/lrets.R')

# Take log returns, compute PCA weights and 1st component series
covs_pca <- all_data %>%
  lrets() %>% # Take log rets
  slice_tail(n = nrow(.) - 1) %>% # Take out first row
  mutate(date = lubridate::as_date(datetime)) %>% # Only day column
  select(-datetime) %>%
  select(date, everything()) %>% 
  nest(data = -date) %>% # Nest according to day
  mutate(covs    = map(.x = data, .f = ~ cov(.x, use = "pairwise.complete.obs")),
         layout  = map_dbl(.x = covs, .f = ~ sqrt(nrow(.x)^2 - sum(is.na(.x)))),
         pca     = map2(.x = data, .y = layout, .f = ~princomp(na.omit(.x[, 1 : .y]))),
         mkt_ret = map_dbl(.x = pca, .f = ~ mean(.x$scores[1:288])), # First day of full sample is wrong (should be 287)
         weights = map2(.x = pca, .y = layout, 
                        .f = ~ tibble(asset = colnames(all_data)[2 : (.y + 1)],
                                      weight = .x$loadings[1 : .y]^2))) %>%
  select(-data) %>% 
  unnest(weights) %>% 
  pivot_wider(names_from = asset, values_from = weight) %>% 
  nest(data = -c(date, covs, mkt_ret, layout, pca)) %>% 
  mutate(act_cov  = map2(.x = covs, .y = layout, .f = ~ as.matrix(.x[1:.y, 1:.y])),
         act_wts  = map2(.x = data, .y = layout, .f = ~ as.matrix(.x[1:.y])),
         mkt_rv   = map2_dbl(.x = act_cov, .y = act_wts, .f = ~ .y %*% .x %*% t(.y)),
         mkt_rvol = sqrt(mkt_rv)) %>% 
  select(-c(layout, data, act_cov))

# Select covariance matrices only and save RDS
covs_pca %>% 
  select(date, covs) %>% 
  readr::write_rds(file = "Data/covs.rds")

# Select the PCA market computations and save RDS
covs_pca %>% 
  select(-c(covs, pca)) %>% 
  readr::write_rds(file = "Data/pca_mkt.rds")

# Select the PCA weights and save RDS
all_data %>%
  lrets() %>% # Take log rets
  slice_tail(n = nrow(.) - 1) %>% # Take out first row
  mutate(date = lubridate::as_date(datetime)) %>% # Only day column
  select(-datetime) %>%
  select(date, everything()) %>% 
  nest(data = -date) %>% # Nest according to day
  mutate(covs    = map(.x = data, .f = ~ cov(.x, use = "pairwise.complete.obs")),
         layout  = map_dbl(.x = covs, .f = ~ sqrt(nrow(.x)^2 - sum(is.na(.x)))),
         pca     = map2(.x = data, .y = layout, .f = ~princomp(na.omit(.x[, 1 : .y]))),
         weights = map2(.x = pca, .y = layout, 
                        .f = ~ tibble(asset = colnames(all_data)[2 : (.y + 1)],
                                      weight = .x$loadings[1 : .y]^2))) %>%
  select(-data) %>% 
  unnest(weights) %>% 
  pivot_wider(names_from = asset, values_from = weight) %>% 
  select(-c(covs, layout, pca)) %>% 
  readr::write_rds(file = "Data/pca_wts.rds")



# Select PCA market Rvol
# covs_pca %>% 
#   select(date, mkt_rvol) %>% 
#   readr::write_rds("Data/pca_rvol.rds")

# pca_rvol <- readr::read_rds("Data/pca_rvol.rds")
rets <- readr::read_rds("Data/rets.rds")

# Daily PCA
dpca <- rets %>%
  na.omit() %>% 
  select(-date) %>% 
  princomp()

summary(dpca)

tibble(date = rets$date,
       mkt_ret = c(rep(NA, 699), as.matrix(dpca$scores)[, 1]),
       mkt_rvol = pca_rvol$mkt_rvol) %>% 
  readr::write_rds("Data/dpca.rds")

# Difference in scalings
w1 <- dpca$loadings[1:10]^2
w2 <- rets %>%
  na.omit() %>% 
  select(-date) %>% 
  prcomp(scale. = TRUE)
w2 <- w2$rotation[1:10]^2

cbind(w1, w2) %>% 
  as_tibble %>% 
  mutate(dd = w1 - w2) %>% 
  select(dd) %>% 
  t.test


# Summary statistics for PC1
# covs_pca %>% 
#   select(pca) %>% 
#   slice_tail(n = 731) %>% 
#   mutate(pc1 = map_dbl(.x = pca, .f = ~factoextra::get_eig(.x)$variance.percent[1])) %>% 
#   summarise(across(pc1, stats))

# Full period covariance matrix
full_cor <- all_data %>%
  slice_tail(n = 210528) %>% 
  lrets() %>% 
  select(-datetime) %>% 
  cor(use = "pairwise.complete.obs")

readr::write_rds(full_cor, "Print/full_cor.rds")  

rm(list = ls())



#### Create Market Cap based Market Measures ####
# Load returns, realized volatilities and market cap based weights
rets <- readr::read_rds("Data/rets.rds")
rvols <- readr::read_rds("Data/rvols.rds")
weights <- readxl::read_excel(paste0('Data/weights.xlsx'))
covs <- readr::read_rds("Data/covs.rds")
pca_wts <- readr::read_rds("Data/pca_wts.rds")

# Pivot all longer
rets_long <- rets %>% 
  pivot_longer(-date, values_to = "ret")
weights_long <- weights %>% 
  pivot_longer(-date, values_to = "weights")
pcawts_long <- pca_wts %>% 
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

# PCA RVol
covs %>%
  left_join(pca_wts, by = "date") %>% 
  slice_tail(n = 731) %>% 
  nest(weights= -c(date, covs))  %>% 
  mutate(mkt_rvol = map2_dbl(.x = covs, .y = weights, .f = ~unlist(.y) %*% .x %*% unlist(.y)))
  mutate(layout = map_dbl(.x = covs, .f = ~ sqrt(nrow(.x)^2 - sum(is.na(.x))))) %>%
  nest(weights = -c(date, covs, layout)) %>% 
  mutate(act_covs = map2(.x = covs, .y = layout, .f = ~ .x[1:.y, 1:.y]),
         act_wts  = map2(.x = weights, .y = layout, .f = ~ .x[1:.y]),
         mkt_rvol = map2_dbl(.x = act_covs, .y = act_wts,
                             .f = ~ as.matrix(.y) %*% .x %*% t(.y))) %>% 
  select(-c(layout, covs, weights, act_covs, act_wts)) %>% 
  readr::write_rds("Data/pca_rvol.rds")



rm(list = ls())


#### Create daily data tibbles ####
# Load Assets' Returns and Market estimates
rets <- readr::read_rds("Data/rets.rds")
mkt_rvol <- readr::read_rds("Data/mkt_rvol.rds")
mkt_ret <- readr::read_rds("Data/mkt_ret.rds")
# pca_mkt <- readr::read_rds("Data/pca_mkt.rds")
dpca <- readr::read_rds("Data/dpca.rds")
pca_rvol <- readr::read_rds("Data/pca_rvol.rds")

# GARCH fit for Market estimates
gspec <- rugarch::ugarchspec(distribution.model = "std", mean.model = list(armaOrder = c(0, 0)),
                             variance.model = list(model = "sGARCH", garchOrder = c(1, 1)))
cap_garch <- rugarch::ugarchfit(gspec, mkt_ret$mkt_ret)@fit$sigma
# pca_garch <- rugarch::ugarchfit(gspec, pca_mkt$mkt_ret, solver = "hybrid")@fit$sigma
pca_garch <- rugarch::ugarchfit(gspec, na.omit(dpca$mkt_ret))@fit$sigma

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
# daily_data_pca <- left_join(rets, pca_mkt, by = "date") %>% select(-mkt_rv)
daily_data_pca <- dpca %>% 
  select(-mkt_rvol) %>% 
  left_join(pca_rvol, by = "date") %>% 
  left_join(rets, by = "date")

# Left join individual returns, PCA based Market Returns and GARCH volatility
# daily_data_pca_garch <- left_join(rets, pca_mkt, by = "date") %>%
#   select(-mkt_rv) %>%
#   mutate(mkt_rvol = pca_garch)
daily_data_pca_garch <- left_join(rets, dpca, by = "date") %>% 
  mutate(mkt_rvol = c(rep(NA, 699), pca_garch))
  

# Save RDS with unified PCA Cap based estimates daily data
readr::write_rds(daily_data_pca, file = "Data/daily_data_pca.rds")
readr::write_rds(daily_data_pca_garch, file = "Data/daily_data_pca_garch.rds")

rm(list = ls())



#### Fama-MacBeth Regressions (FINALLY!) ####
# Load Market Cap and PCA based daily data tibbles
library(tidyverse)
inidate <- as.Date("2019-08-01")
daily_data <- readr::read_rds("Data/daily_data.rds") %>% filter(date >= inidate)
daily_data_garch <- readr::read_rds("Data/daily_data_garch.rds") %>% filter(date >= inidate)
daily_data_pca <- readr::read_rds("Data/daily_data_pca.rds") %>% filter(date >= inidate)
daily_data_pca_garch <- readr::read_rds("Data/daily_data_pca_garch.rds") %>% filter(date >= inidate)

# Load pre Fama-MacBeth regression functions
# source("R/pre_fmb.R")

# Check for any existing zeroes after each series start dates
# daily_data %>% pre_fmb
# If returns FALSE, proceed with Fama-MacBeth regression

# rm(pre_fmb)

# Load Fama-MacBeth regression functions
source("R/fmb1.R")
source("R/fmb2.R")
source("R/fmb_t.R")

### Fama-MacBeth with Realized Market Cap based estimates ###
# Compute first step regressions
regs <- daily_data %>% fmb1()

b1 <- regs %>% filter(term == "mkt_ret") %>% select(estimate) %>% unlist()
b2 <- regs %>% filter(term == "mkt_rvol") %>% select(estimate) %>% unlist()
b1_m1 <- b1
b2_m1 <- b2
estat1 <- tibble(Model = "Model 1",
                 `Ljung-Box 7` = regs %>% filter(lbox7 < 0.05) %>% nrow(.)/30,
                 `Ljung-Box 28` = regs %>% filter(lbox28 < 0.05) %>% nrow(.)/30,
                 `Breusch-Pagan` = regs %>% filter(bp < 0.05) %>% nrow(.)/30)

# Compute second step regressions
regs2 <- daily_data %>% fmb2(beta1 = b1, beta2 = b2)
ustat1 <- tibble(Model = "Model 1",
                 `Ljung-Box 7` = regs2 %>% filter(lbox7 < 0.05) %>% nrow(.)/2193,
                 `Ljung-Box 28` = regs2 %>% filter(lbox28 < 0.05) %>% nrow(.)/2193,
                 `Breusch-Pagan` = regs2 %>% filter(bp < 0.05) %>% nrow(.)/2193)

# t test for the coefficient of interest
cap_t <- regs2 %>% fmb_t("beta1", "beta2")



### Fama-MacBeth with Realized PCA based estimates ###
# Compute first step regressions
regs <- daily_data_pca %>% fmb1()

b1 <- regs %>% filter(term == "mkt_ret") %>% select(estimate) %>% unlist()
b2 <- regs %>% filter(term == "mkt_rvol") %>% select(estimate) %>% unlist()
b1_m2 <- b1
b2_m2 <- b2
estat2 <- tibble(Model = "Model 2",
                 `Ljung-Box 7` = regs %>% filter(lbox7 < 0.05) %>% nrow(.)/30,
                 `Ljung-Box 28` = regs %>% filter(lbox28 < 0.05) %>% nrow(.)/30,
                 `Breusch-Pagan` = regs %>% filter(bp < 0.05) %>% nrow(.)/30)

# Compute second step regressions
regs2 <- daily_data %>% fmb2(beta1 = b1, beta2 = b2)
ustat2 <- tibble(Model = "Model 2",
                 `Ljung-Box 7` = regs2 %>% filter(lbox7 < 0.05) %>% nrow(.)/2193,
                 `Ljung-Box 28` = regs2 %>% filter(lbox28 < 0.05) %>% nrow(.)/2193,
                 `Breusch-Pagan` = regs2 %>% filter(bp < 0.05) %>% nrow(.)/2193)

# t test for the coefficient of interest
pca_t <- regs2 %>% fmb_t("beta1", "beta2")



### Fama-MacBeth with GARCH Market Cap based estimates ###
# Compute first step regressions
regs <- daily_data_garch %>% fmb1()

b1 <- regs %>% filter(term == "mkt_ret") %>% select(estimate) %>% unlist()
b2 <- regs %>% filter(term == "mkt_rvol") %>% select(estimate) %>% unlist()
b1_m3 <- b1
b2_m3 <- b2
estat3 <- tibble(Model = "Model 3",
                 `Ljung-Box 7` = regs %>% filter(lbox7 < 0.05) %>% nrow(.)/30,
                 `Ljung-Box 28` = regs %>% filter(lbox28 < 0.05) %>% nrow(.)/30,
                 `Breusch-Pagan` = regs %>% filter(bp < 0.05) %>% nrow(.)/30)

# Compute second step regressions
regs2 <- daily_data %>% fmb2(beta1 = b1, beta2 = b2)
ustat3 <- tibble(Model = "Model 3",
                 `Ljung-Box 7` = regs2 %>% filter(lbox7 < 0.05) %>% nrow(.)/2193,
                 `Ljung-Box 28` = regs2 %>% filter(lbox28 < 0.05) %>% nrow(.)/2193,
                 `Breusch-Pagan` = regs2 %>% filter(bp < 0.05) %>% nrow(.)/2193)

# t test for the coefficient of interest
cap_garch_t <- regs2 %>% fmb_t("beta1", "beta2")



### Fama-MacBeth with GARCH PCA based estimates ###
# Compute first step regressions
regs <- daily_data_pca_garch %>% fmb1()

b1 <- regs %>% filter(term == "mkt_ret") %>% select(estimate) %>% unlist()
b2 <- regs %>% filter(term == "mkt_rvol") %>% select(estimate) %>% unlist()
b1_m4 <- b1
b2_m4 <- b2
estat4 <- tibble(Model = "Model 4",
                 `Ljung-Box 7` = regs %>% filter(lbox7 < 0.05) %>% nrow(.)/30,
                 `Ljung-Box 28` = regs %>% filter(lbox28 < 0.05) %>% nrow(.)/30,
                 `Breusch-Pagan` = regs %>% filter(bp < 0.05) %>% nrow(.)/30)

# Compute second step regressions
regs2 <- daily_data %>% fmb2(beta1 = b1, beta2 = b2)
ustat4 <- tibble(Model = "Model 4",
                 `Ljung-Box 7` = regs2 %>% filter(lbox7 < 0.05) %>% nrow(.)/2193,
                 `Ljung-Box 28` = regs2 %>% filter(lbox28 < 0.05) %>% nrow(.)/2193,
                 `Breusch-Pagan` = regs2 %>% filter(bp < 0.05) %>% nrow(.)/2193)

# t test for the coefficient of interest
pca_garch_t <- regs2 %>% fmb_t("beta1", "beta2")

rm(regs, regs2, fmb1, fmb2, fmb_t, daily_data, daily_data_garch, daily_data_pca, daily_data_pca_garch,
   b1, b2)

# Save t-tests' results table
cap_t %>% 
  rbind(pca_t, cap_garch_t, pca_garch_t) %>% 
  mutate(model = c(rep(1, 3), rep(2, 3), rep(3, 3), rep(4,3))) %>%
  select(coef, model, everything()) %>% 
  arrange(coef, model) %>% 
  readr::write_rds("Print/fmbt_res.rds")

tibble("Asset" = readxl::read_excel("new_initial_dates.xlsx")[, 2] %>% unlist(),
       "Model 1" = b2_m1,
       "Model 2" = b2_m2,
       "Model 3" = b2_m3,
       "Model 4" = b2_m4) %>% 
  readr::write_rds("Print/betas2_comp.rds")

tibble("Asset" = readxl::read_excel("new_initial_dates.xlsx")[, 2] %>% unlist(),
       "Model 1" = b1_m1,
       "Model 2" = b1_m2,
       "Model 3" = b1_m3,
       "Model 4" = b1_m4) %>% 
  readr::write_rds("Print/betas1_comp.rds")


# pivot_longer(cols = c(x_mean, t_stat, p_val)) %>% 
#   mutate(id = paste0(coef, "_", model)) %>% 
#   select(-c(coef, model)) %>%
#   pivot_wider(names_from = id, values_from = value)

# rm(list = ls())



# mu_g2 <- mean(gammas2)
# sd_g2 <- sd(gammas2)
# test_stat <- sqrt(nrow(regs2) / 3) * mu_g2 / sd_g2
# p_value <- 2 * pt(-abs(test_stat), df = nrow(regs2) - 1)
#### More Summary Statistics

#### More Summary Statistics
stats <- list(
  obs  = ~length(.x),
  min  = ~min(.x, na.rm = T),
  med  = ~median(.x, na.rm = T),
  mean = ~mean(.x, na.rm = T),
  max  = ~max(.x, na.rm = T),
  var  = ~var(.x, na.rm = T),
  sd   = ~sd(.x, na.rm = T),
  q1   = ~quantile(t(.x), na.rm = T)[2],
  q3   = ~quantile(t(.x), na.rm = T)[4]
)

inidate <- as.Date("2019-08-01")

readr::read_rds("Data/daily_data_pca_garch.rds") %>%
  filter(date >= inidate) %>% 
  select(mkt_ret, mkt_rvol) %>% 
  summarise(across(everything(), stats)) %>% 
  pivot_longer(cols = everything()) %>% 
  separate(name, c("mkt", "variable", "measure"), sep = "_") %>%
  select(-c(mkt, variable)) %>% 
  mutate(`Variable - Model` = c(paste0(rep("Returns", 9), " - M2 and M4"), paste0(rep("Volatility", 9), " - M4"))) %>% 
  select(`Variable - Model`, everything()) %>% 
  pivot_wider(names_from = measure, values_from = value) -> m4_mkt

readr::read_rds("Data/daily_data_pca.rds") %>%
  filter(date >= inidate) %>% 
  select(mkt_rvol) %>% 
  summarise(across(everything(), stats)) %>% 
  pivot_longer(cols = everything()) %>% 
  separate(name, c("mkt", "variable", "measure"), sep = "_") %>%
  select(-c(mkt, variable)) %>% 
  mutate(`Variable - Model` = paste0(rep("Volatility", 9), " - M2")) %>% 
  select(`Variable - Model`, everything()) %>% 
  pivot_wider(names_from = measure, values_from = value) -> m2_mkt

readr::read_rds("Data/daily_data_garch.rds") %>%
  filter(date >= inidate) %>% 
  select(mkt_ret, mkt_rvol) %>% 
  summarise(across(everything(), stats)) %>% 
  pivot_longer(cols = everything()) %>% 
  separate(name, c("mkt", "variable", "measure"), sep = "_") %>%
  select(-c(mkt, variable)) %>% 
  mutate(`Variable - Model` = c(paste0(rep("Returns", 9), " - M1 and M3"), paste0(rep("Volatility", 9), " - M3"))) %>% 
  select(`Variable - Model`, everything()) %>% 
  pivot_wider(names_from = measure, values_from = value) -> m3_mkt

readr::read_rds("Data/daily_data.rds") %>% 
  filter(date >= inidate) %>% 
  select(mkt_rvol) %>% 
  summarise(across(everything(), stats)) %>% 
  pivot_longer(cols = everything()) %>% 
  separate(name, c("mkt", "variable", "measure"), sep = "_") %>%
  select(-c(mkt, variable)) %>% 
  mutate(`Variable - Model` = paste0(rep("Volatility", 9), " - M1")) %>% 
  select(`Variable - Model`, everything()) %>% 
  pivot_wider(names_from = measure, values_from = value) %>% 
  rbind(m2_mkt, m3_mkt, m4_mkt) %>% 
  arrange(`Variable - Model`) %>% 
  rename(N = obs, Min. = min, Median = med, Mean = mean, Max. = max, Var. = var, 
         `Std. Dev.` = sd, `25%` = q1, `75%` = q3) %>% 
  readr::write_rds("Print/mktest_summ.rds")

readr::read_rds("Data/pca_wts.rds") %>% 
  select(-date) %>% 
  summarise(across(everything(), stats)) %>% 
  pivot_longer(everything(), names_to = c("Acronym", "Statistic"), 
               names_sep = "_", values_to = "Value") %>% 
  pivot_wider(names_from = Statistic, values_from = Value) %>% 
  rename(N = obs, Min. = min, Median = med, Mean = mean, Max. = max, Var. = var, 
         `Std. Dev.` = sd, `25%` = q1, `75%` = q3) %>% 
  readr::write_rds("Print/pcawts_summ.rds")

estat1 %>% 
  rbind(estat2, estat3, estat4) %>% 
  readr::write_rds("Print/res1.rds")

ustat1 %>% 
  rbind(ustat2, ustat3, ustat4) %>% 
  readr::write_rds("Print/res2.rds")

rm(list = ls())
