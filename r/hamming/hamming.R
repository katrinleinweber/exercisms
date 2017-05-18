library(magrittr)

hamming <- function(strand1,strand2) {
  
  if (identical(strand1, strand2))
    return(0)
  
  if (nchar(strand1) != nchar(strand2))
    stop("different lengths!")
  
  # detect base mistmatches, adding them up
  if (nchar(strand1) == nchar(strand2)) {
    
    bases1 <- unlist(strsplit(strand1, ""))
    bases2 <- unlist(strsplit(strand2, ""))
    
    purrr::map2_lgl(bases1, bases2, stringi::stri_cmp_neq) %>%
      sum() %>%
      return()
  }
}
