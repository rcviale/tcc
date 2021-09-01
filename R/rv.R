# Take Realized Variance
rv <- function(data, h = 1){
  data %>% 
    modify_if(is.numeric, ~log(.x / dplyr::lag(.x, n = h))^2)
}
