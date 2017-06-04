space_age <- function(seconds, planet) {
  
  # switch provided planet name with its relative orbital period
  convert <- function(planet) {
    switch(planet,
           "mercury" = 0.2408467,
           "venus"   = 0.61519726,
           "earth"   = 1,
           "mars"    = 1.8808158,
           "jupiter" = 11.862615,
           "saturn"  = 29.447498,
           "uranus"  = 84.016846,
           "neptune" = 164.79132)
  }
  
  # divide given age by product of sec_per_Earth_year and relative orbital period
  round(seconds / (31557600 * convert(planet)), 2)
}
