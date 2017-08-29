library(magrittr)

triangle <- function(x, y, z) {
  
  # exclude invalid cases
  if (x == 0
      | y == 0
      | z == 0
      | x + y < z
      | z + x < y
      | y + z < x
  ) stop("That's not really a triangle!")
  
  # construct S3 class
  c(x, y, z) %>% 
    unique() %>% 
    length() %>% 
    switch(c("equilateral", "isosceles"), "isosceles", "scalene") %>% 
    structure(c(x, y, z), class = .)
}

