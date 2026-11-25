# SageMath-Skripte zu den Folien

Zu den Beispielen und Grafiken der Foliensätze (getestet mit SageMath 10.9).
Jedes Skript rechnet die Beispiele nach, gibt den Rechenweg aus und prüft die Ergebnisse
per `assert`, wo die Folien Zahlenwerte angeben, gegen diese Werte.

## Vorlesung (`Wirtschaftsmathematik/Folien.Rmd`)

| Skript | Inhalt | Grafiken |
|:--|:--|:--|
| `01_lineare_verzinsung.sage` | Zinsen, Zinstage, Barwert, Äquivalenzprinzip, Laufzeit | 01 |
| `02_jahresersatzrate_skonto_raten.sage` | vor-/nachschüssige Zahlungen, Jahresersatzrate, Skonto, Raten | |
| `03_zinseszins.sage` | Zinseszins, unterjährige/stetige/gemischte Verzinsung, Zinskurve, Laufzeit | |
| `04_renten.sage` | Bar- und Endwerte von Renten (Zahlenwerte frei gewählt) | |
| `05_funktionen.sage` | Nullstellen, Symmetrie, Stetigkeit, Polynomdivision, Ableitungen, Gewinnmaximum, Elastizität | 05a-05g |
| `06_lineare_gleichungssysteme.sage` | Bauernhof, grafische Lösung, Gauß-Schritte, Treppenform | 06a-06d |
| `07_matrizen.sage` | Vektoren, Skalarprodukt, Matrizenprodukt, Produktionsverflechtung | 07 |
| `08_waehlerwanderung.sage` | Übergangsmatrix, Simulation, Grenzverteilung | 08 |
| `09_lineare_optimierung.sage` | Lösungsmengen von Ungleichungen, Zelt-Beispiel | 09a-09g |
| `10_simplex.sage` | Simplex-Tableaus mit exakten Brüchen | |

## Lineare Optimierung (`LineareOptimierung/`)

| Skript | Inhalt | Grafiken |
|:--|:--|:--|
| `11_lo_produktionsprogramm.sage` | Simplex-Tableaus, Ressourcen, Schattenpreis, Geometrie, Konvergenz | 11a, 11b |
| `12_lo_zelte_ganzzahlig.sage` | Zelt-Problem ganzzahlig (zu `lineareOptimierung.R`) | 12 |

## Beispiel-Foliensätze (`Beispiele/`)

| Skript | Inhalt | Grafiken |
|:--|:--|:--|
| `13_bsp_finanzmathematik.sage` | Zinseszins, Sparplan, Annuitätendarlehen, Tilgungsplan | 13a, 13b |
| `14_bsp_lgs.sage` | Gauß-Algorithmus: eine, unendlich viele, keine Lösung | |
| `15_bsp_uebergangsmatrizen.sage` | Wählerwanderung, Marktanteile, stationäre Verteilung | 15a, 15b |

## Ausführen

```bash
cd sage
sage 05_funktionen.sage   # ein Skript
./run_all.sh              # alle Skripte
```

Die Grafiken werden in `sage/plots/` gespeichert.
