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

colnames(monatsdaten) <- c("Monat", "Selbstverbrauch Speicher", "Selbstverbrauch PV", "Einspeisung")

monatsdaten_lang <- pivot_longer(monatsdaten, cols = -Monat, names_to = "reihe", values_to = "wert")

ggplot(monatsdaten_lang, aes(x = Monat, y = wert, fill = reihe)) +
  geom_col() +  #Transparenz für bessere Lesbarkeit
  labs(title = "Verwendung der durch PV erzeugten Energie",
       x = "Monat",
       y = "kWh",
       fill = "Verwendung") +
  theme_minimal()


monatsdaten <- daten |>
  mutate(month = format(dayTime, "%Y-%m")) |>
  group_by(month) |>
  summarize_at(vars(c(selfUsedEnergyPV, selfUsedEnergyBattery, gridUsedEnergy), -group_cols()), scaled_sum)

colnames(monatsdaten) <- c("Monat", "b) PV-Selbstverbrauch", "c) Batterieverbrauch" ,"a) Netzbezug")

monatsdaten_lang <- pivot_longer(monatsdaten, cols = -Monat, names_to = "reihe", values_to = "wert")

ggplot(monatsdaten_lang, aes(x = Monat, y = wert, fill = reihe)) +
  geom_col() +  #Transparenz für bessere Lesbarkeit
  labs(title = "Deckung des Strombedarfs durch",
       x = "Monat",
       y = "kWh",
       fill = "Energiequelle") +
  theme_minimal()

# Eigenverbrauchsquote
sum(daten$selfUsedEnergy)/sum(daten$powerProduction)

# Autarkiegrad
sum(daten$selfUsedEnergy)/sum(daten$energyConsumption)

# Eingespeister Strom kWh/Jahr
sum(daten$feedInEnergyGrid)/1000

# Selbstgenutzter Strom kWh/Jahr
sum(daten$selfUsedEnergy)

# Ersparnis durch Akku
(ersparnis_durch_akku <- sum(daten$selfUsedEnergyBattery)/1000 * (0.32 - 0.0703))

# Ersparnis Anlage pro Jahr
(ersparnis_anlage_pro_jahr <- sum(daten$selfUsedEnergy)/1000 * 0.32 + sum(daten$feedInEnergyGrid)/1000 * 0.0703)

# Amortisationszeit Anlage
29150 / ersparnis_anlage_pro_jahr 

# Amortisationszeit Speicher
10.24 * 500 / ersparnis_durch_akku
