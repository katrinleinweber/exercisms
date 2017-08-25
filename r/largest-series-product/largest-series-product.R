library(magrittr)

largest_series_product <- function(digits, span) {
  
  # catch edge cases & invalid input
  if (nchar(digits) < span)
    stop("Span must not be longer then the digit string!")
  else if (digits == "" | span == 0)
    return(1)
  else if (stringi::stri_detect_regex(digits, "[^0-9]"))
    stop("There are non-digits in the digit string!")
  
  # turn digit string into integer vector
  list <- digits %>%
    strsplit("") %>%
    unlist %>%
    as.numeric
  
  # calculate number of steps
  steps <- length(list) - span + 1
  
  products <- c()
  
  # slide window along vector
  for (s in 1:steps) {
    products <- c(products,
                  prod(list[s:(s + span - 1)]))
  }
  
  max(products)
  
  # [ ] speed up by ignoring steps with 0
}
