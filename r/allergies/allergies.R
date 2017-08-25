allergy <- function(num) {
  
  # convert score to binary
  binary <- rawToChar(intToBits(num), multiple = TRUE)[1:8]
  
  # match places with 1
  # construct list
  
  allergies <- character()
  
  if (binary[1] != "") allergies <- append(allergies, "eggs")
  if (binary[2] != "") allergies <- append(allergies, "peanuts")
  if (binary[3] != "") allergies <- append(allergies, "shellfish")
  if (binary[4] != "") allergies <- append(allergies, "strawberries")
  if (binary[5] != "") allergies <- append(allergies, "tomatoes")
  if (binary[6] != "") allergies <- append(allergies, "chocolate")
  if (binary[7] != "") allergies <- append(allergies, "pollen")
  if (binary[8] != "") allergies <- append(allergies, "cats")

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

