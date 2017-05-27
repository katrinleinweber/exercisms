library(stringi)

bob <- function(input) {
  # remove all white-space from both sides and multiple in between
  input <- stri_replace_all(input, regex = "^\\s$|\\s+", "")
  
  dplyr::case_when(
    # yelling := more upper- than lowercase OR exclamation
    stri_count_regex(input, "[A-Z]") >
      stri_count_regex(input, "[a-z]") |
      stri_detect_regex(input, "[A-Z]\\!") ~
      paste("Whoa, chill out!"),
    
    # question
    stri_endswith(input, fixed = "?") ~
      paste("Sure."),
    
    # not saying anything
    input == "" | input == " " |
      stri_count_regex(input, "[[:punct:]]") >
      stri_count_regex(input, "[[:alnum:]]") ~
      paste("Fine. Be that way!"),
    
    # everything else
    TRUE ~ paste("Whatever.")
  )
}
