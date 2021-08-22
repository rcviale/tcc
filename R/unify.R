unify <- function(files, directory, col){
  paste0(directory, files, '.rds') %>% 
    purrr::map(.f = readr::read_rds) %>%
    purrr::reduce(full_join, by = {{col}}) %>% 
    dplyr::arrange({{col}})
}