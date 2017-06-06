library(magrittr)
library(stringi)

parse_phone_number <- function(number_string) {
  
  # proceed only, if string contains nothing other than 
  # digits, dots, hyphens, parentheses and/or spaces
  if (!stri_detect_regex(number_string, "[^0-9.\\-() ]")) {
    
    # remove valid non-digit characters
    number_string %>%
      stri_extract_all_regex("[0-9]") %>%
      unlist() %>%
      paste(collapse = "") ->
      phone_number
    
    # return only if exactly 10 digits, or 11 and starting with 1
    if (nchar(phone_number) == 10)
      return(phone_number)
    else if (nchar(phone_number) == 11 &
             stri_detect_regex(phone_number, "^1"))
      return(gsub("^1", "", phone_number))
    # [ ] refactor to dplyr::case_when without "subscript out of bounds" error
  }
}
