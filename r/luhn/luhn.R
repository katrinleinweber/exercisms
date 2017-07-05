library(magrittr)

is_valid <- function(input) {
  
  # convert to digits
  digits <- input %>%
    gsub(" ", "", x = .) %>%
    strsplit("") %>%
    unlist
  
  # reject too small input
  if (length(digits) <= 1)
    return(FALSE)
  
  # reject non-digit characters
  if (TRUE %in% stringi::stri_detect_regex(digits, "\\D"))
    return(FALSE)
  # [ ] compound all checks
  
  # prepare for Luhn's algorithm
  stigid <- digits %>%
    as.integer %>%
    rev
  
  # conduct Luhn's algorithm
  # [ ] more elegant
  for (d in seq(1, length(stigid)-1, 2)) {
    d <- d+1
    stigid[d] <- 2 * stigid[d]
    if (stigid[d] > 9)
      stigid[d] <- stigid[d] - 9
  }
  
  # conforms to Luhn, if divisible by 10
  # [ ] only revert & convert once
  if (stigid %>% rev %>% as.integer %>% sum %% 10 == 0)
    return(TRUE)
  else
    return(FALSE)
}

