library(magrittr)

scrabble_score <- function(input){
  
  # match of letter to score
  score <- function(letter) {
    dplyr::case_when(
      letter %in% c("A", "E", "I", "O", "U", "L", "N", "R", "S", "T") ~ 1,
      letter %in% c("F", "H", "V", "W", "Y") ~ 4,
      letter %in% c("B", "C", "M", "P") ~ 3,
      letter %in% c("D", "G") ~ 2,
      letter %in% c("J", "X") ~ 8,
      letter %in% c("Q", "Z") ~ 10,
      letter %in% c("K") ~ 5
    )}
  
  # split input into letters
  # sum scores up
  input %>% 
    toupper() %>% 
    strsplit("") %>% 
    purrr::map(.f = score) %>% 
    unlist() %>% 
    sum()
  
}
