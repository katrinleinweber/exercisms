library(magrittr)
library(stringi)

anagram <- function(subject, candidates) {

  options(encoding = "UTF-8")
  
  # buffer
  anagrams <- c()
  
  if (length(candidates) <= 1)
    return(anagrams)
  
  # prepare comparison
  candidates_lengths <- stri_length(candidates)
  
  for (x in 1:length(candidates)) {
    
    if (candidates_lengths[x] == stri_length(subject)) {
      
      candidates[x] %>%
        stri_extract_all_regex(pattern = paste0("[", tolower(subject), toupper(subject), "]")) %>%
        unlist() %>% 
        paste(collapse = "") ->
        anagram
      
      # discard anagrams that are not different from subject
      if (identical(anagram, subject) | identical(tolower(anagram), tolower(subject)))
          anagram <- c()
         
      # store true anagrams in buffer 
      if (identical(stri_length(anagram), stri_length(subject)))
        anagrams <- append(anagrams, anagram)
    }
  }
  
  return(anagrams)
}
