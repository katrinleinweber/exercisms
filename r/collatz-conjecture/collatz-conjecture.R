collatz_step_counter <- function(num) {
  
  # initialise counter
  steps <- 0
  
  # recurse through cases, after testing for base case
  if (num == 1)
    steps
  else if (num %% 2 == 0)
    1 + collatz_step_counter(num / 2)
  else if (num %% 2 != 0)
    1 + collatz_step_counter(num * 3 + 1)
  else  # if (num <= 0)
    stop("Num must be an integer larger than 0.")
}
