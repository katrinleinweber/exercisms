space_age <- function(seconds, planet) {
  
  # naive, if-else alternative with planets years multiplied with Earth's 
  dplyr::case_when(
    identical(planet, "mercury") ~ round(seconds / 7600544, 2),
    identical(planet, "venus") ~ round(seconds / 19414149, 2),
    identical(planet, "earth") ~ round(seconds / 31557600, 2),
    identical(planet, "mars") ~ round(seconds / 59354033, 2),
    identical(planet, "jupiter") ~ round(seconds / 374355659, 2),
    identical(planet, "saturn") ~ round(seconds / 929292363, 2),
    identical(planet, "uranus") ~ round(seconds / 2651370019, 2),
    identical(planet, "neptune") ~ round(seconds / 5200418560, 2)
  )
}
