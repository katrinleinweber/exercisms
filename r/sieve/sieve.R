sieve <- function(limit) {
  
  # catch edge case
  if (limit == 1)
    return()
  
  # list candidates
  candidates <- 2:limit
  
  # walk through list, calculate multiples and eliminate
  for (c in candidates) {
    multiples <- candidates * c
    candidates <- setdiff(candidates, multiples)
  }
  
  candidates
}
