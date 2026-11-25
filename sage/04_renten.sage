# Rentenrechnung
# Folien: "Endwert einer nachschüssigen Rente" bis "Zusammenfassung der zentralen Rentenformeln"
# Die Folien rechnen allgemein; die Zahlenwerte R = 1000, p = 5 %, n = 10 sind frei gewählt.


def de(x, stellen=2):
    """Zahl mit Dezimalkomma und Tausenderpunkt."""
    return f"{float(x):,.{int(stellen)}f}".replace(",", "#").replace(".", ",").replace("#", ".")


var('R q n k')

# Geschlossene Formeln aus der Zusammenfassung
EN = R * (q^n - 1)/(q - 1)
EV = R * q * (q^n - 1)/(q - 1)
BN = R / q^n * (q^n - 1)/(q - 1)
BV = R * q / q^n * (q^n - 1)/(q - 1)

print("--- Symbolischer Nachweis: Summe = geschlossene Formel (für n = 1, ..., 12) ---")
for N in range(1, 13):
    summen = [sum(R*q^(j - 1) for j in range(1, N + 1)),   # Endwert nachschüssig
              sum(R*q^j for j in range(1, N + 1)),         # Endwert vorschüssig
              sum(R/q^j for j in range(1, N + 1)),         # Barwert nachschüssig
              sum(R/q^(j - 1) for j in range(1, N + 1))]   # Barwert vorschüssig
    for summe, formel in zip(summen, [EN, EV, BN, BV]):
        assert (summe - formel.subs(n=N)).simplify_rational() == 0
print("Alle vier Formeln stimmen.")

print("\n--- Beziehungen aus der Zusammenfassung ---")
assert (EV - q*EN).simplify_full() == 0
assert (BV - q*BN).simplify_full() == 0
assert (BN - EN/q^n).simplify_full() == 0
assert (BV - EV/q^n).simplify_full() == 0
print("vorschüssig = q * nachschüssig, Barwert = Endwert / q^n")

werte = {R: 1000, q: 105/100}
N = 10
for titel, vorschuessig in [("nachschüssig", False), ("vorschüssig", True)]:
    print(f"\n--- Tabelle: Rente {titel}, R = 1000, p = 5 %, n = {N} ---")
    print("Jahr | Restzeit |  Endwert | Zeitpunkt |  Barwert")
    summe_E, summe_B = 0, 0
    for jahr in range(1, N + 1):
        rest = N - jahr + (1 if vorschuessig else 0)
        zeit = jahr - (1 if vorschuessig else 0)
        e = 1000 * (105/100)^rest
        b = 1000 / (105/100)^zeit
        summe_E += e
        summe_B += b
        print(f"{jahr:4} | {rest:8} | {de(e):>8} | {zeit:9} | {de(b):>8}")
    E, B = (EV, BV) if vorschuessig else (EN, BN)
    print(f"Summe Endwert {de(summe_E)} EUR, Summe Barwert {de(summe_B)} EUR")
    assert summe_E == E.subs(werte).subs(n=N)
    assert summe_B == B.subs(werte).subs(n=N)
