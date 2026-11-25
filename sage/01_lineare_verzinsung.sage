# Einführung in die Finanzmathematik: lineare Verzinsung
# Folien: "Beispiele (1/3)" bis "Berechnung von Zinssatz und Laufzeit"


def de(x, stellen=2):
    """Zahl mit Dezimalkomma und Tausenderpunkt."""
    return f"{float(x):,.{int(stellen)}f}".replace(",", "#").replace(".", ",").replace("#", ".")


def zinstage(von, bis):
    """Zinstage nach der Methode 30/360; von und bis als (Tag, Monat, Jahr)."""
    t1, m1, j1 = von
    t2, m2, j2 = bis
    return 360*(j2 - j1) + 30*(m2 - m1) + (min(t2, 30) - min(t1, 30))


print("--- Beispiele (1/3): Zinsen für 3500 EUR vom 3.3. bis 18.8. bei 3,25 % ---")
K = 3500
i = 325/10000
T = zinstage((3, 3, 2026), (18, 8, 2026))
Z = K * i * T/360
print(f"Zinstage T = {T}")
print(f"Zinsen   Z = {de(Z, 6)} EUR, gerundet {de(Z)} EUR")
assert T == 165 and round(float(Z), 2) == 52.14

print("\n--- Beispiele (2/3): Kredithöhe bei 657,44 EUR Zinsen, 8 %, halbes Jahr ---")
var('x')
Z, i, T = 65744/100, 8/100, 180
K = solve(x * i * T/360 == Z, x)[0].rhs()
print(f"Kredit K = {de(K)} EUR")
assert K == 16436

print("\n--- Beispiele (3/3): Zinstage bei 36,46 EUR Stückzinsen, 5000 EUR, 6,25 % ---")
Z, K, i = 3646/100, 5000, 625/10000
T = solve(K * i * x/360 == Z, x)[0].rhs()
print(f"T = {de(T, 4)}, also {round(T)} Zinstage")
assert round(T) == 42


def endwert(K0, i, t):
    return K0 * (1 + i*t)


def barwert(Kt, i, t):
    return Kt / (1 + i*t)


print("\n--- Beispiele: Barwert von 8000 EUR, fällig in einem halben Jahr, 5 % ---")
K0 = barwert(8000, 5/100, 1/2)
print(f"K0 = {de(K0)} EUR")
assert round(float(K0), 2) == 7804.88

print("\n--- Äquivalenzprinzip (1/2): 9000 EUR in 30 Tagen oder 9085 EUR in 90 Tagen ---")
folie = {6: (8955.22, 8950.74), 3: (8977.56, 9017.37)}
for p in [6, 3]:
    B30 = barwert(9000, p/100, 30/360)
    B90 = barwert(9085, p/100, 90/360)
    guenstiger = "zweite" if B90 < B30 else "erste"
    print(f"{p} %: Barwert(30) = {de(B30)}, Barwert(90) = {de(B90)}"
          f" -> das {guenstiger} Angebot ist günstiger")
    assert (round(float(B30), 2), round(float(B90), 2)) == folie[p]

print("\n--- Äquivalenzprinzip (2/2): Bei welchem Zinssatz sind beide gleich? ---")
gleichung = barwert(9000, x, 30/360) == barwert(9085, x, 90/360)
i_gleich = solve(gleichung, x)[0].rhs()
print(f"i = {i_gleich} = {de(100*i_gleich)} %")
assert round(float(100*i_gleich), 2) == 5.69

# Grafische Veranschaulichung (Ergänzung, nicht auf der Folie)
import os
ordner = os.path.join(os.path.dirname(os.path.abspath(__file__)) if "__file__" in globals() else ".", "plots")
os.makedirs(ordner, exist_ok=True)
G = (plot(barwert(9000, x, 30/360), (x, 0, 0.1), color="blue", legend_label="9000 EUR in 30 Tagen")
     + plot(barwert(9085, x, 90/360), (x, 0, 0.1), color="red", legend_label="9085 EUR in 90 Tagen")
     + point([(i_gleich, barwert(9000, i_gleich, 30/360))], size=40, color="black", zorder=10))
G.save(os.path.join(ordner, "01_barwertvergleich.png"), axes_labels=["i", "Barwert"], frame=True, axes=False)

print("\n--- Laufzeit: 1200 EUR wachsen bei 2,8 % auf 1225,20 EUR ---")
t = solve(endwert(1200, 28/1000, x) == 122520/100, x)[0].rhs()
print(f"t = {t} Jahre")
assert t == 3/4
