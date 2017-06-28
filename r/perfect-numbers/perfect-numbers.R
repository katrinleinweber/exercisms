library(magrittr)

is_perfect <- function(n){
  
  # catch edge cases
  if (n <= 0)
    stop("Only natural number can be classified here!")
  if (n <= 2)
    return("deficient")
  
  # find n's factors, incl. 1 but not n (aliquots)
  factor <- function(i)
    if (n %% i == 0)
      i
  
  # calculate sum and classify n
  lapply(1:(n/2), factor) %>% 
    unlist %>% 
    sum ->
    sum
    
  dplyr::case_when(
    sum == n ~ "perfect",
    sum < n ~ "deficient",
    sum > n ~ "abundant"
  )
}
