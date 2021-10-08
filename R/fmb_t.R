fmb_t <- function(data, lambda1, lambda2){
  data %>% 
    filter(term == lambda1) %>% 
    select(estimate) %>% 
    t.test() -> cf1
  data %>% 
    filter(term == lambda2) %>% 
    select(estimate) %>% 
    t.test() -> cf2
  tibble(coef   = c("lambda1", "lambda2"),
         x_mean = c(cf1$estimate, cf2$estimate),
         t_stat = c(cf1$statistic, cf2$statistic),
         p_val  = c(cf1$p.value, cf2$p.value))
}
