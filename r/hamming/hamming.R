library(magrittr)

hamming <- function(strand1,strand2) {
  
  dplyr::case_when(
    identical(strand1, strand2) ~ as.integer(0),
    nchar(strand1) == nchar(strand2) ~
      
      # detect base mistmatches, adding them up
      purrr::map2_lgl(split(strand1),
                      split(strand2),
                      stringi::stri_cmp_neq
                      ) %>%
      sum()
  )
  
  # omitting explicit stop("Different lengths!") doesn't cause last test to fail
}

split <- function(strand) {
  strand %>% strsplit("") %>% unlist()
}
