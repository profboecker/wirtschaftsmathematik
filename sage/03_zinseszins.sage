# Zinseszinsrechnung
# Folien: "Zinseszinsrechnung" bis "Zinseszinsformel"


def de(x, stellen=2):
    """Zahl mit Dezimalkomma und Tausenderpunkt."""
    return f"{float(x):,.{int(stellen)}f}".replace(",", "#").replace(".", ",").replace("#", ".")


def zinstage(von, bis):
    """Zinstage nach der Methode 30/360; von und bis als (Tag, Monat, Jahr)."""
    t1, m1, j1 = von
    t2, m2, j2 = bis
    return 360*(j2 - j1) + 30*(m2 - m1) + (min(t2, 30) - min(t1, 30))


var('i m n')

print("--- Zinseszins: Sparbrief 4000 EUR, 5 Jahre, 6 % ---")
K = 4000
for jahr in range(1, 6):
    K = K * 106/100
    print(f"Jahr {jahr}: {de(K)} EUR")
assert K == 4000 * (106/100)^5 and round(float(K), 2) == 5352.90
print(f"Barwert der Zahlung {de(K)} EUR: {de(K / (106/100)^5)} EUR")

print("\n--- Unterjährige Verzinsung (Ergänzung: 4000 EUR, 6 %, 5 Jahre) ---")
for name, perioden in [("jährlich", 1), ("halbjährlich", 2), ("quartalsweise", 4),
                       ("monatlich", 12), ("täglich", 360)]:
    Km = 4000 * (1 + 0.06/perioden)^(5*perioden)
    ieff = (1 + 0.06/perioden)^perioden - 1
    print(f"{name:14} m = {perioden:3}: K = {de(Km)} EUR, i_eff = {de(100*ieff, 4)} %")
print(f"{'stetig':14}          K = {de(4000*exp(0.30))} EUR, i_eff = {de(100*(exp(0.06) - 1), 4)} %")

print("\n--- Grenzwert m -> unendlich: exponentielle Verzinsung ---")
assume(i > 0, n > 0)
grenzwert = limit((1 + i/m)^(n*m), m=oo)
print("lim (1 + i/m)^(n*m) =", grenzwert)
assert bool(grenzwert == exp(i*n))

print("\n--- Effektiver Jahreszins ---")
print("i_eff =", (1 + i/m)^m - 1)
forget()

print("\n--- Gemischte Verzinsung: 10000 EUR vom 15.8.2015 bis 16.3.2019, 12 % ---")
T1 = zinstage((15, 8, 2015), (31, 12, 2015))
jahre = 2018 - 2015  # 31.12.2015 bis 31.12.2018
T2 = zinstage((31, 12, 2018), (16, 3, 2019))
K = 10000 * (1 + 12/100*T1/360) * (1 + 12/100)^jahre * (1 + 12/100*T2/360)
print(f"{T1} Zinstage + {jahre} volle Jahre + {T2} Zinstage")
print(f"K = {de(K)} EUR")
assert (T1, T2) == (135, 76) and round(float(K), 2) == 15053.43

print("\n--- Zinskurveneffekte: 2 Jahre 6 %, 5 Jahre 7 %, 3 Jahre 4 % ---")
zinssaetze = [6/100]*2 + [7/100]*5 + [4/100]*3
K = 10000
for jahr, z in enumerate(zinssaetze, 1):
    K *= 1 + z
    print(f"Jahr {jahr:2} ({de(100*z, 0)} %): {de(K)} EUR")
q_eff = (K/10000)^(1/10)
print(f"Endwert {de(K)} EUR, i_eff = {de(100*(q_eff - 1))} %")
assert round(float(K), 2) == 17726.81 and round(float(q_eff - 1), 4) == 0.0589

print("\n--- Zinseszinsformel: Effektivzins als geometrisches Mittel ---")
q_geo = prod(1 + z for z in zinssaetze)^(1/len(zinssaetze))
q_ari = mean([1 + z for z in zinssaetze])
print(f"geometrisches Mittel : q = {de(q_geo, 6)}")
print(f"arithmetisches Mittel: q = {de(q_ari, 6)}  (wäre falsch)")
assert abs(float(q_geo - q_eff)) < 1e-12

print("\n--- Laufzeitberechnung: Verdreifachung bei 5 % ---")
dauer = log(3.0) / log(1.05)
print(f"n = log 3 / log 1,05 = {de(dauer, 9)} Jahre")
assert abs(dauer - 22.517085304) < 1e-8
