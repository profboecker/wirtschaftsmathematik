rm(list = ls())

zahl <- function(x) {
  x[4] * 1000 + x[3] * 100 + x[2] * 10 + x[1]
}

ziffern <- function(zahl) {
  arbeitszahl <- zahl
  x <- c()
  for (i in 1:4) {
    x <- c(x, arbeitszahl %% 10)
    arbeitszahl <- arbeitszahl %/% 10
  }
  return(x)
}

berechne <- function(zahl) {
  x_neu <- ziffern(zahl)
  print("Startzahl:")
  while (!(zahl(x_neu) == 6174)) {
    print(zahl(x_neu))
    x_max <- sort(x_neu, decreasing = TRUE)
    x_min <- sort(x_neu, decreasing = FALSE)
    x_neu <- zahl(x_min) - zahl(x_max)
    print(paste("Berechne ", zahl(x_min), "-", zahl(x_max), "=", x_neu))
    x_neu <- ziffern(x_neu)
  }
  return(zahl(x_neu))
}

startzahl <- 1235
print(berechne(startzahl))
