sum_of_multiples <- function(factors, limit) {
  
  # create vector of candidate integers
  candidates <- seq(limit - 1)
  
  # narrow candidates down to factors & unique multiples
  multiples <- c()
  for (f in factors)
    for (c in candidates)
      if (c %% f == 0 & !(c %in% multiples))
        multiples <- append(multiples, c)
  # [ ] recursive append?
  # [ ] use interaction()?
  # [ ] purrr::map or ::walk on differently long vectors?
  
  # sum those up
  sum(multiples)
  
}
