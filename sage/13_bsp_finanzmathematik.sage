# Finanzmathematik - Beispiele
# Foliensatz: Beispiele/Finanzmathematik/Finanzmathematik_Beispiel.Rmd
import os

ordner = os.path.join(os.path.dirname(os.path.abspath(__file__)) if "__file__" in globals() else ".", "plots")
os.makedirs(ordner, exist_ok=True)


def de(x, stellen=2):
    """Zahl mit Dezimalkomma und Tausenderpunkt."""
    return f"{float(x):,.{int(stellen)}f}".replace(",", "#").replace(".", ",").replace("#", ".")


print("=== Sparbetrag mit Zinseszins (Frau Müller) ===")
K0, q, n = 8000, 1035/1000, 10
K10 = K0 * q^n
print(f"(1) Endwert nach 10 Jahren: {de(K10)} EUR")
assert round(float(K10), 2) == 11284.79

dauer = log(1.5) / log(1.035)
print(f"(2) Laufzeit bis 12.000 EUR: {de(dauer)} Jahre, Ziel im {ceil(dauer)}. Jahr erreicht")
assert K0*q^floor(dauer) < 12000 <= K0*q^ceil(dauer)
assert round(float(dauer), 2) == 11.79

B = 12000 / q^n
print(f"(3) Nötiger Anfangsbetrag: {de(B)} EUR")
assert round(float(B), 2) == 8507.03


def endwert(R, q, n, vorschuessig=False):
    return R * (q if vorschuessig else 1) * (q^n - 1) / (q - 1)


def barwert(R, q, n, vorschuessig=False):
    return endwert(R, q, n, vorschuessig) / q^n


print("\n=== Altersvorsorgesparen (Herr Schmidt) ===")
R, q, n = 2400, 104/100, 25
EN, BN = endwert(R, q, n), barwert(R, q, n)
EV, BV = endwert(R, q, n, True), barwert(R, q, n, True)
print(f"Endwert  nachschüssig: {de(EN)} EUR")
print(f"Barwert  nachschüssig: {de(BN)} EUR")
print(f"Eingezahlt: {de(R*n)} EUR, Zinsen: {de(EN - R*n)} EUR")
print(f"Endwert  vorschüssig : {de(EV)} EUR")
print(f"Barwert  vorschüssig : {de(BV)} EUR")
print(f"Mehrgewinn vorschüssig: {de(EV - EN)} EUR")
assert EV == q*EN
assert [round(float(w), 2) for w in (EN, BN, EV, BV, EV - EN)] == [99950.18, 37492.99, 103948.19, 38992.71, 3998.01]
# Simulation Jahr für Jahr
konto_n = konto_v = 0
verlauf = [(0, 0, 0, 0)]
for jahr in range(1, n + 1):
    konto_n = konto_n*q + R
    konto_v = (konto_v + R)*q
    verlauf.append((jahr, konto_n, konto_v, R*jahr))
assert (konto_n, konto_v) == (EN, EV)
G = sum(line([(v[0], v[k]) for v in verlauf], color=farbe, legend_label=name)
        for k, farbe, name in [(1, "blue", "nachschüssig"), (2, "red", "vorschüssig"), (3, "gray", "eingezahlt")])
G.save(os.path.join(ordner, "13a_sparplan.png"), frame=True, axes_labels=["Jahr", "Kontostand in EUR"])


def annuitaet(B, q, n):
    return B * q^n * (q - 1) / (q^n - 1)


def tilgungsplan(B, q, n):
    """Liste (Jahr, Zinsen, Tilgung, Restschuld)."""
    rate, rest, plan = annuitaet(B, q, n), B, []
    for jahr in range(1, n + 1):
        zinsen = rest*(q - 1)
        rest -= rate - zinsen
        plan.append((jahr, zinsen, rate - zinsen, rest))
    return plan


print("\n=== Annuitätendarlehen: Kreditvergleich (Familie Berger) ===")
darlehen = 280000 - 80000
banken = {"Bank A": (1038/1000, 15), "Bank B": (1032/1000, 20)}
zinskosten = {}
for name, (q, n) in banken.items():
    rate = annuitaet(darlehen, q, n)
    zinskosten[name] = rate*n - darlehen
    plan = tilgungsplan(darlehen, q, n)
    assert plan[-1][3] == 0 and sum(p[1] for p in plan) == zinskosten[name]
    print(f"{name}: Jahresrate {de(rate)} EUR, Gesamtzahlung {de(rate*n)} EUR, Zinsen {de(zinskosten[name])} EUR")
mehr = zinskosten["Bank B"] - zinskosten["Bank A"]
print(f"Bank B kostet {de(mehr)} EUR mehr Zinsen -> Bank A ist insgesamt günstiger.")
assert [round(float(annuitaet(darlehen, *banken[b])), 2) for b in banken] == [17737.57, 13692.94]
assert [round(float(zinskosten[b]), 2) for b in banken] == [66063.51, 73858.89]
assert round(float(mehr), 2) == 7795.37

print("\n--- Tilgungsplan Bank A, erste 5 Jahre ---")
print(f"{'Jahr':>4} {'Zinsen':>10} {'Tilgung':>11} {'Restschuld':>12}")
plan_A = tilgungsplan(darlehen, *banken["Bank A"])
assert [round(float(w), 2) for w in plan_A[0][1:]] == [7600.00, 10137.57, 189862.43]
for jahr, zinsen, tilgung, rest in plan_A[:5]:
    print(f"{jahr:>4} {de(zinsen):>10} {de(tilgung):>11} {de(rest):>12}")
G = (line([(p[0], p[1]) for p in plan_A], color="red", legend_label="Zinsanteil")
     + line([(p[0], p[2]) for p in plan_A], color="blue", legend_label="Tilgungsanteil"))
G.save(os.path.join(ordner, "13b_tilgung_bank_a.png"), frame=True, ymin=0, axes_labels=["Jahr", "EUR"])
