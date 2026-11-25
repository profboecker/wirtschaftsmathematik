# Übergangsmatrizen - Beispiele
# Foliensatz: Beispiele/Uebergangsmatrizen/Uebergangsmatrizen_Beispiel.Rmd
import os

ordner = os.path.join(os.path.dirname(os.path.abspath(__file__)) if "__file__" in globals() else ".", "plots")
os.makedirs(ordner, exist_ok=True)


def prozent(namen, v):
    return ",  ".join(f"{p}: {float(100*a):.2f} %".replace(".", ",") for p, a in zip(namen, v))


def verlauf(M, start, perioden):
    """Liste der Zustandsvektoren w0, w1, ..., w_perioden."""
    werte = [start]
    for _ in range(perioden):
        werte.append(M * werte[-1])
    return werte


def stationaer(M):
    """Ersetzt die letzte Gleichung von (M - I) w = 0 durch die Normierung Summe = 1."""
    A = M - identity_matrix(QQ, 3)
    A[2] = vector(QQ, [1, 1, 1])
    w = A.solve_right(vector(QQ, [0, 0, 1]))
    assert M*w == w and sum(w) == 1
    return w


def zeichnen(werte, namen, dauerhaft, datei, titel, xname):
    G = Graphics()
    for k, (farbe, stil) in enumerate([("red", "-"), ("darkgreen", "--"), ("blue", "-.")]):
        punkte = [(n, 100*w[k]) for n, w in enumerate(werte)]
        G += line(punkte, color=farbe, linestyle=stil, legend_label=namen[k])
        if len(werte) <= 21:
            G += point(punkte, color=farbe, size=20)
        if dauerhaft is not None:
            y = 100*dauerhaft[k]
            G += line([(0, y), (len(werte) - 1, y)], color=farbe, linestyle=":")
    G.save(os.path.join(ordner, datei), frame=True, title=titel, figsize=[9, 5],
           axes_labels=[xname, "Anteil in %"])


print("=== Beispiel 1: Wählerwanderung ===")
parteien = ["A", "B", "C"]
W = matrix(QQ, [[90, 8, 1], [4, 85, 7], [6, 7, 92]]) / 100
w0 = vector(QQ, [42, 37, 21]) / 100
assert all(sum(s) == 1 for s in W.columns())

w = verlauf(W, w0, 20)
print("(1) Nächste Wahl:", prozent(parteien, w[1]))
assert w[1] == vector(QQ, [4097, 3460, 2443]) / 10000

print("\n(2) Langfristige Entwicklung")
for n in [1, 2, 3, 5, 10, 20]:
    print(f"  Wahl {n:2}: {prozent(parteien, w[n])}")
assert [[round(float(a), 4) for a in w[n]] for n in [1, 2, 3, 5, 10, 20]] == [
    [0.4097, 0.3460, 0.2443], [0.3989, 0.3276, 0.2736], [0.3879, 0.3136, 0.2985],
    [0.3669, 0.2950, 0.3381], [0.3251, 0.2781, 0.3968], [0.2852, 0.2778, 0.4369]]
zeichnen(w, parteien, None, "15a_waehler.png", "Langfristige Wähleranteile", "Wahlperiode")

wahl = next(n for n in range(1, len(w)) if w[n][2] > w[n][1])
print(f"\n(3) C überholt B nach Wahl {wahl}: {prozent(parteien, w[wahl])}")
assert w[wahl - 1][2] <= w[wahl - 1][1]
assert wahl == 4

w_stern = stationaer(W)
print(f"\nStationäre Verteilung: {w_stern}\n  {prozent(parteien, w_stern)}")
assert [round(float(100*a), 2) for a in w_stern] == [27.00, 28.14, 44.87]
# unabhängig vom Startzustand
for start in identity_matrix(RDF, 3).columns():
    assert (verlauf(W.change_ring(RDF), start, 600)[-1] - w_stern.change_ring(RDF)).norm() < 1e-8
print("Gleiche Grenzverteilung, auch wenn anfangs eine Partei alle Stimmen hat.")

print("\n=== Beispiel 2: Marktanteile dreier Mobilfunkanbieter ===")
anbieter = ["Alpha", "Beta", "Gamma"]
M = matrix(QQ, [[88, 6, 5], [8, 82, 10], [4, 12, 85]]) / 100
m0 = vector(QQ, [50, 30, 20]) / 100
print("Spaltensummen:", [sum(s) for s in M.columns()])
assert all(sum(s) == 1 for s in M.columns())

m = verlauf(M, m0, 60)
print("(1) Nach 6 Monaten:", prozent(anbieter, m[6]))
m_stern = stationaer(M)
assert [round(float(100*a), 2) for a in m[6]] == [37.39, 32.36, 30.26]
assert [round(float(100*a), 2) for a in m_stern] == [31.38, 33.47, 35.15]
print(f"(2) Stationär: {m_stern}\n    {prozent(anbieter, m_stern)}")
fuehrend = anbieter[max(range(3), key=lambda k: m_stern[k])]
if fuehrend == "Alpha":
    print("(3) Alpha bleibt langfristig Marktführer.")
else:
    monat = next(n for n in range(len(m)) if m[n][0] < max(m[n][1], m[n][2]))
    print(f"(3) Alpha verliert die Marktführerschaft (ab Monat {monat}); langfristig führt {fuehrend}.")
zeichnen(m, anbieter, m_stern, "15b_marktanteile.png", "Entwicklung der Mobilfunk-Marktanteile", "Monate")
