rm(list = ls())
library(readr)
library(tidyverse)
daten <- read_delim("Beispiele/Stromdaten/daten_10240.csv", 
                          delim = ";", escape_double = FALSE, col_types = cols(dayTime = col_datetime(format = "%Y%m%d:%H")), 
                          trim_ws = TRUE)
# View(daten)

scaled_sum <- function(x) {
  sum(x / 1000)
}

monatsdaten <- daten |>
  mutate(month = format(dayTime, "%Y-%m")) |>
  group_by(month) |>
  summarize_at(vars(c(selfUsedEnergyBattery, selfUsedEnergyPV, feedInEnergyGrid), -group_cols()), scaled_sum)

colnames(monatsdaten) <- c("Monat", "#2 Selbstverbrauch Speicher", "#1 Selbstverbrauch PV", "#3 Einspeisung")

monatsdaten_lang <- pivot_longer(monatsdaten, cols = -Monat, names_to = "reihe", values_to = "wert")

ggplot(monatsdaten_lang, aes(x = Monat, y = wert, fill = reihe)) +
  geom_col() +  #Transparenz für bessere Lesbarkeit
  labs(title = "Gestapeltes Zeitreihendiagramm",
       x = "Monat",
       y = "kWh",
       fill = "Herkunft") +
  theme_minimal()



monatsdaten <- daten |>
  mutate(month = format(dayTime, "%Y-%m")) |>
  group_by(month) |>
  summarize_at(vars(c(selfUsedEnergyPV, selfUsedEnergyBattery, gridUsedEnergy), -group_cols()), scaled_sum)

colnames(monatsdaten) <- c("Monat", "PV-Selbstverbrauch", "Batterieverbrauch" ,"Netzverbrauch")

monatsdaten_lang <- pivot_longer(monatsdaten, cols = -Monat, names_to = "reihe", values_to = "wert")

ggplot(monatsdaten_lang, aes(x = Monat, y = wert, fill = reihe)) +
  geom_col() +  #Transparenz für bessere Lesbarkeit
  labs(title = "Gestapeltes Zeitreihendiagramm",
       x = "Monat",
       y = "kWh",
       fill = "Herkunft") +
  theme_minimal()
