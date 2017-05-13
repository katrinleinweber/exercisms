library(stringi)

bob <- function(input) {
  
  # remove white-space
  input <- stri_replace_all_regex(input, "\\s+", "")
  
  dplyr::case_when(
    
    # yelling
    stri_count_regex(input, "[[:upper:]]") > 
      stri_count_regex(input, "[[:lower:]]") ~ 
      paste("Whoa, chill out!"),
    
    # question
    stri_endswith(input, fixed = "?") ~ 
      paste("Sure."),
    
    # not saying anything
    input == "" | 
      stri_count_regex(input, "[[:punct:]]") > 
      stri_count_regex(input, "[[:alnum:]]") ~ 
      paste("Fine. Be that way!"),
    
    # everything else
    TRUE ~ paste("Whatever.")
  )
}
