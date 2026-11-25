# Mehrfache konstante Zahlungen, Jahresersatzrate, Skonto, Ratenzahlungen
# Folien: "Mehrfache konstante Zahlungen" bis "Ratenzahlungen"


def de(x, stellen=2):
    """Zahl mit Dezimalkomma und Tausenderpunkt."""
    return f"{float(x):,.{int(stellen)}f}".replace(",", "#").replace(".", ",").replace("#", ".")


var('r i k m x')

print("--- Mehrfache konstante Zahlungen: vorschüssig ---")
R_vor = sum(r * (1 + i*j/12) for j in range(1, 13))
print("R =", R_vor.factor())
assert bool(R_vor.expand() == (r*(12 + 13/2*i)).expand())

print("\n--- Mehrfache konstante Zahlungen: nachschüssig ---")
R_nach = sum(r * (1 + i*j/12) for j in range(0, 12))
print("R =", R_nach.factor())
assert bool(R_nach.expand() == (r*(12 + 11/2*i)).expand())

print("\n--- Jahresersatzrate für beliebige Zahlungsfrequenz m (symbolisch) ---")
R_vor_m = r * sum(1 + i*(m - k)/m, k, 0, m - 1)
R_nach_m = r * sum(1 + i*k/m, k, 0, m - 1)
print("vorschüssig :", R_vor_m.factor())
print("nachschüssig:", R_nach_m.factor())
assert (R_vor_m - r*(m + (m + 1)/2*i)).simplify_full() == 0
assert (R_nach_m - r*(m + (m - 1)/2*i)).simplify_full() == 0

print("\n--- Jahresersatzrate: Sparplan 75 EUR monatlich, 4 %, 1 % Bonus ---")
rate, zins, bonus_satz = 75, 4/100, 1/100
bonus = 12 * rate * bonus_satz
for art, erste, folie in [("vorschüssig", 1, 928.50), ("nachschüssig", 0, 925.50)]:
    print(f"\n{art}:")
    E = 0
    for monat in range(1, 13):
        restmonate = 12 - monat + erste
        wert = rate * (1 + zins * restmonate/12)
        E += wert
        print(f"  Rate {monat:2}: {restmonate:2} Monate verzinst -> {de(wert)} EUR")
    print(f"  Endwert ohne Bonus {de(E)} EUR + Bonus {de(bonus)} EUR = {de(E + bonus)} EUR")
    assert E + bonus == QQ(folie)

print("\n--- Skontoabzug: 2 % bei Zahlung in 10 statt 30 Tagen ---")
s, T = 2/100, 30 - 10
R = var('R')
i_eff = solve((1 - s)*R == R / (1 + x*T/360), x)[0].rhs()
print(f"i_eff = {i_eff} = {de(100*i_eff)} %")
assert i_eff == s/(1 - s) * 360/T and round(float(100*i_eff), 2) == 36.73

print("\n--- Ratenzahlungen: zwei Raten mit 5 % Aufschlag ---")
i_raten = solve(1 == 105/100/2 + 105/100/2 / (1 + x/2), x)[0].rhs()
print(f"i = {i_raten} = {de(100*i_raten, 4)} %")
assert round(float(i_raten), 6) == 0.210526
