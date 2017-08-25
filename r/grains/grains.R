# chess board min & max

square <- function(n) {
  if (1 <= n & n <= 64)
    2 ^ (n - 1)
  else
    stop("That's not a proper chess board!")
}

total <- function() {
  sum(2 ^ ((1:64) - 1))
}
