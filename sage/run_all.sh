#!/bin/sh
# Führt alle SageMath-Skripte aus; bricht beim ersten Fehler ab.
cd "$(dirname "$0")" || exit 1
for f in [0-9][0-9]_*.sage; do
  echo "================ $f ================"
  sage "$f" || { echo "FEHLER in $f"; exit 1; }
  rm -f "$f.py"
done
echo "Alle Skripte erfolgreich."
