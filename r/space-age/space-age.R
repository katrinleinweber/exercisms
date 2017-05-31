space_age <- function(seconds, planet) {
  Earth_seconds_per_year <- 31557600
  
  # naive, but readable if-else alternative 
  dplyr::case_when(
    identical(planet, "mercury") ~ round(seconds / Earth_seconds_per_year / 0.2408467, 2),
    identical(planet, "venus") ~ round(seconds / Earth_seconds_per_year / 0.61519726, 2),
    identical(planet, "earth") ~ round(seconds / Earth_seconds_per_year / 1, 2),
    identical(planet, "mars") ~ round(seconds / Earth_seconds_per_year / 1.8808158, 2),
    identical(planet, "jupiter") ~ round(seconds / Earth_seconds_per_year / 11.862615, 2),
    identical(planet, "saturn") ~ round(seconds / Earth_seconds_per_year / 29.447498, 2),
    identical(planet, "uranus") ~ round(seconds / Earth_seconds_per_year / 84.016846, 2),
    identical(planet, "neptune") ~ round(seconds / Earth_seconds_per_year / 164.79132, 2)
  )
}
