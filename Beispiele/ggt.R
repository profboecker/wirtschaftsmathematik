# Erweiterter euklidischer Algorithmus
egcd <- function(a, b) {
  if (b == 0) {
    return(list(g = a, x = 1, y = 0))
  } else {
    res <- egcd(b, a %% b)
    g <- res$g
    x <- res$y
    y <- res$x - (a %/% b) * res$y
    return(list(g = g, x = x, y = y))
  }
}

# Multiplikatives Inverses von a modulo m
mod_inverse <- function(a, m) {
  a <- a %% m  # Normalisieren
  res <- egcd(a, m)
  
  if (res$g != 1) {
    stop("Kein multiplikatives Inverses vorhanden (a und m sind nicht teilerfremd).")
  }
  
  # Inverses positiv machen
  inv <- res$x %% m
  return(inv)
}

