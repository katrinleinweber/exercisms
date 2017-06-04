sum_of_multiples <- function(factors, limit) {
  
  # create vector of candidate integers
  candidates <- seq(limit - 1)
  
  # narrow candidates down to factors & multiples
  multiples <- c()
  for (f in factors)
    for (c in candidates)
      if (c %% f == 0)
        multiples <- append(multiples, c)
  
  # sum those up
  sum(unique(multiples))
  
}
