# Take log returns
lrets <- function(data, h = 1){
  data %>% 
    modify_if(is.numeric, ~log(.x / dplyr::lag(.x, n = h)))
}
# ~ é um atalho para function(x) e . ou .x seria o x dessa function