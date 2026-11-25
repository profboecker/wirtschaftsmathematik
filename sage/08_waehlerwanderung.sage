# Beispiel Wählerwanderung (Übergangsmatrix)
import os

ordner = os.path.join(os.path.dirname(os.path.abspath(__file__)) if "__file__" in globals() else ".", "plots")
os.makedirs(ordner, exist_ok=True)

W = matrix(QQ, [[90, 8, 1], [4, 85, 7], [6, 7, 92]]) / 100
w0 = vector(QQ, [42, 37, 21]) / 100
parteien = ["A", "B", "C"]


def prozent(v):
    return ",  ".join(f"{p}: {float(100*a):.2f} %".replace(".", ",") for p, a in zip(parteien, v))


# Jede Spalte verteilt 100 % der Wähler einer Partei
assert all(sum(spalte) == 1 for spalte in W.columns())

print("--- Nächste Wahl: w1 = W * w0 ---")
w1 = W * w0
print(prozent(w1))
assert w1 == vector(QQ, [4097, 3460, 2443]) / 10000

print("\n--- Weitere Wahlen: wn = W^n * w0 ---")
Wf, w = W.change_ring(RDF), w0.change_ring(RDF)
verlauf = [w]
for wahl in range(1, 1002):
    w = Wf * w
    verlauf.append(w)
    if wahl in [1, 2, 3, 5, 10, 20, 50, 100, 1001]:
        print(f"n = {wahl:4}: {prozent(w)}")

print("\n--- Grenzverteilung exakt: Eigenvektor zum Eigenwert 1 ---")
v = (W - 1).right_kernel().basis()[0]
stationaer = v / sum(v)
print("exakt:", stationaer)
print(prozent(stationaer))
assert W*stationaer == stationaer
assert (w - stationaer.change_ring(RDF)).norm() < 1e-10
# Assoziativgesetz: schrittweise Anwendung und Matrixpotenz liefern dasselbe
assert W^5 * w0 == W*(W*(W*(W*(W*w0))))

farben = ["red", "blue", "darkgreen"]
G = sum(line([(n, 100*verlauf[n][k]) for n in range(61)], color=farben[k], legend_label=f"Partei {parteien[k]}")
        + line([(0, 100*stationaer[k]), (60, 100*stationaer[k])], color=farben[k], linestyle="--")
        for k in range(3))
G.save(os.path.join(ordner, "08_waehlerwanderung.png"), axes_labels=["Wahl n", "Anteil in %"], frame=True, ymin=0, ymax=60)
