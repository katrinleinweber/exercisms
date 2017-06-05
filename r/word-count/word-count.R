library(stringi)

word_count <- function(input) {
  
  string <- tolower(input)
  
  # extract unique words, so that counting works without duplicates
  string %>% 
    stri_extract_all_words() %>% 
    unlist() %>% 
    unique() -> 
    words
  
  # count each word in string by utilising word boundary reg-ex
  count <- function(w) {
    string %>% 
      stri_count_regex(paste0(w, "\\b"))
  }
  
  # construct results list
  counts <- purrr::map_int(words, count)
  names(counts) <- words
  as.list(counts)
  
}
