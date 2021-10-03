# Function to compute 2nd Fama-MacBeth regression
fmb2 <- function(data, beta1, beta2, K = 10){
  data %>% 
    # Take market return and volatility out of the data set
    dplyr::select(-c(mkt_ret, mkt_rvol)) %>% 
    # Pivot longer
    tidyr::pivot_longer(cols = -date, names_to = "assets", values_to = "rets") %>%
    # Nest data
    tidyr::nest(data = c(assets, rets)) %>% 
    # Nunmber of series in that day
    mutate(n        = map_dbl(.x = data, .f = ~10 - sum(is.na(.x)))) %>%
    # Filter only days with K series
    filter(n == K) %>% 
    # Run regression
    mutate(reg      = map(.x = data, .f = ~lm(.x$rets ~ beta1 + beta2)),
           reg_tidy = map(.x = reg, .f = broom::tidy),
           rsq      = map_dbl(.x = reg, .f = ~summary(.x)$r.squared)) %>% 
    unnest(reg_tidy)
}
