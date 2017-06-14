library(magrittr)

is_isogram <- function(word) {
  
    gsub(pattern = "\\W", replacement = "", x = word) %>%
    strsplit("") %>% 
    unlist %>% 
    tolower ->
    letters
  
  # compare letter vector before & after unique()
  identical(letters, unique(letters))
  
}
