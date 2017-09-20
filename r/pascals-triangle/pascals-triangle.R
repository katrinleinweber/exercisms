pascals_triangle <- function(n) {
  
  # catch invalid input & edge cases 
  if (n < 0 | is.null(n))
    stop("Invalid! Please provide n > 0!")
  else if (n == 0)
    list()
  else if (n == 1)
    list(1)
  else if (n == 2)
    list(1, c(1, 1))
  else if (n >= 3) {
    
  # construct triangle
    t <- list(1, c(1, 1))
    
    for (i in seq(3, n)) {
      t <- append(t, list(c(i)))
      t[[i]][1] <- 1
      t[[i]][i] <- 1
      
  # construct row
      for (j in seq(2, i-1))
        t[[i]][j] <- t[[i-1]][j-1] + t[[i-1]][j]
    }
    return(t)
  }
}
