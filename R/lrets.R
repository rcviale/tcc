# Take log returns
lrets <- function(data){
  data %>% 
    modify_if(is.numeric, ~log(.x / dplyr::lag(.x)))
}
# ~ é um atalho para function(x) e . ou .x seria o x dessa function