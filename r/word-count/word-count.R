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
  counts <- c()
  for (w in words) {
    string %>% 
      stri_count_regex(paste0(w, "\\b")) ->
      count
    
    counts <- append(counts, count)
  }
  # [ ] purrr::map?
  
  # construct results list
  names(counts) <- words
  as.list(counts)
  
}
