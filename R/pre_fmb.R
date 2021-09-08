# Check for any existing zeroes in the middle of the data (after each series start), 
# which would compromise the 1st Fama-MacBeth regression
pre_fmb <- function(data, date = date, mkt_ret = mkt_ret, mkt_rvol = mkt_rvol){
  data %>% 
    # Pivot longer
    tidyr::pivot_longer(cols = -c(date, mkt_ret, mkt_rvol), names_to = "assets", values_to = "rets") %>% 
      # Nest
      tidyr::nest(data = c(date, mkt_ret, mkt_rvol, rets)) %>% 
      # Column with first observation that should be considered in the regression, 
      # then verifying if there are any other zeroes besides the ones before each series start
      dplyr::mutate(t0        = map_dbl(.x = data, .f = ~length(which(.x$rets == 0)) + 1),
                    other_0   = map2_lgl(.x = data, .y = t0, .f = ~any(.x$rets[.y : 1430] == 0))) %>% 
      # Check if any value in the column t0 == 0
      summarise(any(other_0 == TRUE)) %>% 
      # Return a single logical
      as.logical()
}
  