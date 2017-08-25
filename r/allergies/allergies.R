allergy <- function(num) {
  
  # convert score to binary
  binary <- rawToChar(intToBits(num), multiple = TRUE)
  
  # construct list
  allergy_list <- c(
    "eggs",          # 2
    "peanuts",       # 4
    "shellfish",     # 8
    "strawberries",  # 16
    "tomatoes",      # 32
    "chocolate",     # 64
    "pollen",        # 128
    "cats"           # 256
  )
  
  allergies <- character()
  
  # match places with 1
  for (i in seq(length(allergy_list))) {
    if (binary[i] != "")
      allergies <- append(allergies, allergy_list[i])
  }
  
  return(allergies)
}

allergic_to <- function(allergy_object, allergy) {
  
  # check element in list
  allergy %in% allergy_object
}

list_allergies <- function(allergy_object) {
  
  # pass list through
  return(allergy_object)
}

