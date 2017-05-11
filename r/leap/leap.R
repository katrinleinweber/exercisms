leap <- function(year) {
  
  dplyr::case_when(
    year %% 400 == 0 ~ TRUE,
    year %% 100 == 0 ~ FALSE, 
    year %% 4 == 0 ~ TRUE,
    TRUE ~ FALSE
  )
}
