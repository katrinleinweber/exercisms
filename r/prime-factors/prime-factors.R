prime_factors <- function(number) {

  # set start conditions
  factors <- c()
  f <- 2
  
  # loop through checking factors for even division and append
  # [ ] recursion possible as in collatz.R?
  while (number > 1) {
    if (number %% f == 0) {
      factors <- c(factors, f)
      number <- number / f
    } else {
      f <- f + 1
    }
  }
  
  factors
}
