# Function to compute 1st Fama-MacBeth regression
fmb1 <- function(data, date = date, mkt_ret = mkt_ret, mkt_rvol = mkt_rvol){
  data %>% 
    # Pivot longer
    tidyr::pivot_longer(cols = -c(date, mkt_ret, mkt_rvol), names_to = "assets", values_to = "rets") %>% 
    # Nest
    tidyr::nest(data = c(date, mkt_ret, mkt_rvol, rets)) %>% 
    # Column with first observation that should be considered in the regression, then the linear models' columns and
    # lastly, Ljung-Box test for the residuals, with lag = 7 (DEACTIVATED)
    dplyr::mutate(t0        = map_dbl(.x = data, .f = ~length(which(.x$rets == 0)) + 1),
                  reg       = map2(.x = data, .y = t0, .f = ~ lm(formula = rets ~ mkt_ret + mkt_rvol, 
                                                                 data = .x[.y : nrow(.x), ])),
                  reg_tidy  = map(.x = reg, .f = broom::tidy)) %>% #,
    #              lbox      = map_dbl(.x = reg, .f = ~Box.test(residuals(.x), lag = 7, type = "Ljung-Box")$p.value)) %>% 
    tidyr::unnest(reg_tidy) %>% 
    # Delete first observation column
    dplyr::select(-t0)
}
