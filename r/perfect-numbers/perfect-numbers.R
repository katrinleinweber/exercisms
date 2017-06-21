library(magrittr)

is_perfect <- function(n){
  
  # catch edge cases
  if (n <= 0)
    stop("Only natural number can be classified here!")
  
  if (n == 1)
    return("deficient")
  
  
  aliquots <- c()
  
  # find n's factors, incl. 1 but not n (aliquots)
  for (i in seq(n - 1)) {
    if (n %% i == 0)
      aliquots <- append(aliquots, i)
  }
    
  # calculate sum and classify n
  sum <- as.numeric(sum(aliquots))
  
  dplyr::case_when(
    sum == n ~ "perfect",
    sum < n ~ "deficient",
    sum > n ~ "abundant"
  )
}
