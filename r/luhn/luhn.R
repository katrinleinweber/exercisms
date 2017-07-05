library(magrittr)

is_valid <- function(input) {
  
  # convert to digits
  input %>%
    gsub(" ", "", x = .) %>%
    strsplit("") %>%
    unlist ->
    digits
  
  # reject invalid input (too small or non-digit characters)
  if (length(digits) <= 1
      | TRUE %in% stringi::stri_detect_regex(digits, "\\D"))
    return(FALSE)
  
  # prepare for Luhn's algorithm
  digits %>%
    as.integer %>%
    rev ->
    stigid
  
  # conduct Luhn's algorithm: double every second digits, but stay below 10
  for (d in seq(2, length(stigid), 2)) {
    stigid[d] <- 2 * stigid[d]
    if (stigid[d] > 9)
      stigid[d] <- stigid[d] - 9
  }
  
  # conforms to Luhn, if divisible by 10
  if (stigid %>% sum %% 10 == 0)
    return(TRUE)
  else
    return(FALSE)
}
