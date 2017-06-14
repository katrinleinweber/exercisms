library(magrittr)

is_isogram <- function(word) {
  
  word %>%
    stringi::stri_extract_all_regex("\\w") %>%
    unlist %>%
    tolower ->
    letters
  
  # compare length of letter vectors before & after unique()
  identical(length(letters),
            length(unique(letters)))
  
}
