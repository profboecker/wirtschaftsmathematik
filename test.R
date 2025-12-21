# Speicherinhalt vorsorglich löschen
rm(list = ls())
library(tidyverse)
set.seed(Sys.time())
#set.seed(20251221)

# Anzahl der angezeigten Nachkommastellen
options(digits = 3)

# Anzahl der Datenpunkte für die Simulation
n <- 100

# Steigung der Simulationsgeraden
m <- 1

# Achsenabschnitt der Simulationsgeraden
b <- 0

# Standardabweichung des Fehlers
sd <- 10

# Datenpunkte erzeugen
x <- seq(-(n / 2), (n / 2 - 1), 1)
y <- rnorm(n, mean = (m * x + b), sd = sd)
data <- tibble(x, y)

# Lineare Regression
model <- lm(data$y ~ data$x)
se <- sqrt(deviance(model)/df.residual(model))

# Zeichnen
ggplot(data = data, aes(x = x, y = y)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE, level = 0.99) +
  geom_errorbar(aes(ymin = y - se, ymax = y + se), width = 0.1)

print(summary(model))

rm(list = ls())
