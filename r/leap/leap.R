leap <- function(year) {
  
  if (year %% 4 == 0) {
    
    if (year %% 100 != 0)  
      return(TRUE)
    else if (year %% 400 == 0)  # together with `!=` above means "unless also"
        return(TRUE)
  }
  
  return(FALSE)
}
