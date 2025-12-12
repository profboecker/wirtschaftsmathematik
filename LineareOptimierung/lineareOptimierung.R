rm(list = ls())

# R-Skript zur Lösung des Zelt-Problems

best_persons  <- -1
best_solution <- c(x = NA, y = NA, cost = NA, persons = NA)

for (x in 0:5) {          # 10-Personen-Zelte
  for (y in 0:4) {        # 15-Personen-Zelte
    cost <- 200 * x + 400 * y
    if (cost <= 1800) {
      persons <- 10 * x + 15 * y
      if (persons > best_persons) {
        best_persons  <- persons
        best_solution <- c(x = x, y = y, cost = cost, persons = persons)
      }
    }
  }
}

best_solution


rm(list = ls())
# R-Beispiel mit lpSolve für das Zelt-Problem (Simplex)
# install.packages("lpSolve") # falls nötig

library(lpSolve)

# Zielfunktionskoeffizienten: 10-Personen (x), 15-Personen (y)
obj <- c(10, 15)

# Nebenbedingungenmatrix (<= Form):
# 1) x <= 5
# 2) y <= 4
# 3) 200x + 400y <= 1800
A <- matrix(c(1,   0,
              0,   1,
              200, 400),
            nrow = 3, byrow = TRUE)

rhs   <- c(5, 4, 1800)
sense <- c("<=", "<=", "<=")

# LP lösen (Maximierung, ganzzahlige Variablen)
solution <- lp(direction   = "max",
               objective.in = obj,
               const.mat    = A,
               const.dir    = sense,
               const.rhs    = rhs,
               all.int      = TRUE)

solution$solution   # optimale x, y
solution$objval     # maximale Personenanzahl
solution$status     # 0 bedeutet optimale Lösung gefunden
