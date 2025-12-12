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
library(ggplot2)

zeltkapazitaet <- c(10, 15)
zeltpreise <- c(200, 400)
budget <- 1800

# Zielfunktionskoeffizienten: 10-Personen (x), 15-Personen (y)
obj <- zeltkapazitaet

# Nebenbedingungenmatrix (<= Form):
# 1) x <= 5
# 2) y <= 4
# 3) 200x + 400y <= 1800
A <- matrix(c(1,   0,
              0,   1,
              zeltpreise),
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

# LP mit Ganzzahligkeit lösen
sol <- lp(direction    = "max",
          objective.in = obj,
          const.mat    = A,
          const.dir    = sense,
          const.rhs    = rhs,
          all.int      = TRUE)

opt_x <- sol$solution[1]
opt_y <- sol$solution[2]

# Daten für die Nebenbedingungen als Geraden im (x,y)-Diagramm
x_vals <- seq(0, 5, by = 0.1)

# 200x + 400y <= 1800 -> y = (1800 - 200x)/400
line_budget <- data.frame(
  x = x_vals,
  y = (budget - zeltpreise[1] * x_vals) / zeltpreise[2] 
)

# zulässige ganzzahlige Punkte erzeugen
pts <- expand.grid(x = 0:5, y = 0:4)
pts$feasible <- with(pts, zeltpreise[1] * x + zeltpreise[2] * y <= budget)
pts$persons  <- with(pts, 10 * x + 15 * y)

# Plot
p <- ggplot() +
  # zulässige ganzzahlige Punkte
  geom_point(data = subset(pts, feasible),
             aes(x = x, y = y, color = persons), size = 3) +
  # unzulässige ganzzahlige Punkte
  geom_point(data = subset(pts, !feasible),
             aes(x = x, y = y), color = "grey80", size = 2, shape = 4) +
  # Budgetgerade
  geom_line(data = line_budget,
            aes(x = x, y = y), color = "red", linewidth = 1) +
  # Schranke x = 5
  geom_vline(xintercept = 5, linetype = "dashed") +
  # Schranke y = 4
  geom_hline(yintercept = 4, linetype = "dashed") +
  # Optimale Lösung hervorheben
  geom_point(aes(x = opt_x, y = opt_y),
             color = "black", fill = "yellow", size = 4, shape = 21) +
  labs(x = "Anzahl 10-Personen-Zelte (x)",
       y = "Anzahl 15-Personen-Zelte (y)",
       color = "Personen",
       title = "Lineares Programm: Zelt-Problem",
       subtitle = "Zulässige ganzzahlige Lösungen und optimale Lösung") +
  coord_cartesian(xlim = c(0, 5.2), ylim = c(0, 4.2)) +
  theme_minimal()

print(p)

