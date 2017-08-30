library(magrittr)

is_pangram <- function(input) {

  # reduce char space, sort and compare against letter list
  input %>% 
    tolower() %>% 
    strsplit("") %>% 
    unlist() %>% 
    unique() %>% 
    grep(pattern = "[a-z]", value = TRUE) %>% 
    sort() %>% 
    identical(letters)
}
