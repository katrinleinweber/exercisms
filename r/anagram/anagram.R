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
  length_subject <- stri_length(subject)
  
  for (x in 1:length(candidates)) {
    
    if (candidates_lengths[x] == length_subject) {
      
      candidates[x] %>%
        stri_extract_all_regex(pattern = paste0("[", tolower(subject), toupper(subject), "]")) %>%
        unlist() %>% 
        paste(collapse = "") ->
        anagram
      
      # discard anagrams that are not different from subject
      if (identical(anagram, subject) | identical(tolower(anagram), tolower(subject)))
          anagram <- c()
         
      # store true anagrams in buffer 
      if (identical(stri_length(anagram), length_subject))
        anagrams <- append(anagrams, anagram)
    }
  }
  
  return(anagrams)
}
