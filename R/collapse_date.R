# Function to construct different minute-based series
collapse_date <- function(data, col, periodicity = 'day', func, ...) {
  data %>% 
    dplyr::mutate({{col}} := lubridate::floor_date({{col}}, unit = periodicity)) %>% 
    dplyr::group_by({{col}}) %>% 
    dplyr::summarise(across(where(is.numeric), func, ...))
}