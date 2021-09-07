# Function to compute 2nd Fama-MacBeth regression
fmb2 <- function(data, col_date = date, mkt_data = c(mkt_ret, mkt_rvol), beta1, beta2){
  data %>% 
    # Take market return and volatility out of the data set
    dplyr::select(-{{mkt_data}}) %>% 
    tidyr::pivot_longer(cols = -{{col_date}}, names_to = "assets", values_to = "rets") %>% 
    tidyr::nest(data = c(assets, rets)) %>% 
    dplyr::mutate(reg      = map(.x = data, .f = ~ lm(formula = rets ~ beta1 + beta2, data = .x)), 
           reg_tidy = map(.x = reg, .f = broom::tidy)) %>% 
    unnest(reg_tidy)
}