library(magrittr)

is_isogram <- function(word) {
  
    gsub(pattern = "\\W", replacement = "", x = word) %>%
    tolower ->
    letters
  
  # compare length of letter vectors before & after unique()
  identical(length(letters),
            length(unique(letters)))
  
}
