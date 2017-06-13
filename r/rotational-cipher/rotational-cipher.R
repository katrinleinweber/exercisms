library(dplyr)
library(magrittr)

rotate <- function(text, key) {
  
  shift <- function(byte) {
  
    # define alphabet boundaries for ASCII decimals, to enable shifting
    wrap <- 26
    A <- 65
    Z <- 90
    a <- 97
    z <- 122
    
    if      (between(byte, A, Z)) return((byte - A + key) %% wrap + A)
    else-if (between(byte, a, z)) return((byte - a + key) %% wrap + a)
    else    return(byte)   #   set decimal to 1 ^      reset range ^
  }
  
  # convert text to ASCII decimals
  # convert back to (cipher)text
  text %>% 
    utf8ToInt() %>% 
    purrr::map(shift) %>% 
    intToUtf8  # %>% return()
  
}
