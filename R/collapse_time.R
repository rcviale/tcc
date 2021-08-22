# Function to construct different minute-based series
collapse_time <- function(data, col, periodicity, func, ...) {
  assertthat::assert_that(assertthat::is.number(periodicity))
  assertthat::assert_that(periodicity <= 60, msg = "periodicity must be between 1 and 60.")
  
  data %>% 
    dplyr::mutate({{col}} := lubridate::ceiling_date({{col}}, unit = paste(periodicity, "min"))) %>% 
    dplyr::group_by({{col}}) %>% 
    dplyr::summarise(across(where(is.numeric), func, ...))
}