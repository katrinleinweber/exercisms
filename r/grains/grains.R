# chess board min & max
a1 <- 1
g8 <- 64

square <- function(n) {
  if (dplyr::between(n, a1, g8))
    2 ^ (n - 1)
  else
    stop("That's not a proper chess board!")
}

total <- function() {
  sum(2 ^ ((a1:g8) - 1))
}
