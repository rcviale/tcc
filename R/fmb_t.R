fmb_t <- function(data, lambda1, lambda2){
  data %>% 
    filter(term == "(Intercept)") %>% 
    select(estimate) %>% 
    t.test() -> cf0
  data %>% 
    filter(term == "(Intercept)") %>% 
    select(estimate) %>% 
    unlist() %>% 
    sd() -> sd0
  data %>% 
    filter(term == lambda1) %>% 
    select(estimate) %>% 
    t.test() -> cf1
  data %>% 
    filter(term == lambda1) %>% 
    select(estimate) %>% 
    unlist() %>% 
    sd() -> sd1
  data %>% 
    filter(term == lambda2) %>% 
    select(estimate) %>% 
    t.test() -> cf2
  data %>% 
    filter(term == lambda2) %>% 
    select(estimate) %>% 
    unlist() %>% 
    sd() -> sd2
  tibble(coef   = c("lambda0", "lambda1", "lambda2"),
         x_mean = c(cf0$estimate, cf1$estimate, cf2$estimate),
         sd_er  = c(sd0, sd1, sd2),
         t_stat = c(cf0$statistic, cf1$statistic, cf2$statistic),
         p_val  = c(cf0$p.value, cf1$p.value, cf2$p.value))
}
