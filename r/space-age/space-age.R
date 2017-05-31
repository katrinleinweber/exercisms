space_age <- function(seconds, planet) {
  
  Earth_seconds_per_year <- 31557600
  
  # calculate factor on demand
  conversion <- list(
    mercury = Earth_seconds_per_year * 0.2408467,
    venus = Earth_seconds_per_year * 0.61519726,
    earth = Earth_seconds_per_year * 1,
    mars = Earth_seconds_per_year * 1.8808158,
    jupiter = Earth_seconds_per_year * 11.862615,
    saturn = Earth_seconds_per_year * 29.447498,
    uranus = Earth_seconds_per_year * 84.016846,
    neptune = Earth_seconds_per_year * 164.79132
  )
  
  # divide given age by planet-specific factor
  round(seconds / conversion[[planet]], 2)
}
