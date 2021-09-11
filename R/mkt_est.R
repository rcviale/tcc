# Weight the provided measure according to the provided weights to compute a "Market" estimate
mkt_est <- function(measure_long, weights_long, col_measure, col_weights = weights){
  left_join(measure_long, weights_long, by = c("date", "name")) %>% 
    mutate(weighted_measure = {{col_measure}} * {{col_weights}}) %>% 
    group_by(date) %>% 
    summarise(mkt_est = sum(weighted_measure, na.rm = TRUE))
}
