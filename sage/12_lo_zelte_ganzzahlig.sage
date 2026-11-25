# Zelt-Problem als ganzzahliges Programm
# Vorlage: LineareOptimierung/lineareOptimierung.R
import os

ordner = os.path.join(os.path.dirname(os.path.abspath(__file__)) if "__file__" in globals() else ".", "plots")
os.makedirs(ordner, exist_ok=True)

print("--- Durchprobieren aller Kombinationen ---")
zulaessig, unzulaessig = [], []
beste = None
for a in range(6):        # 10-Personen-Zelte
    for b in range(5):    # 15-Personen-Zelte
        kosten, personen = 200*a + 400*b, 10*a + 15*b
        if kosten <= 1800:
            zulaessig.append((a, b))
            if beste is None or personen > beste[3]:
                beste = (a, b, kosten, personen)
        else:
            unzulaessig.append((a, b))
print(f"{len(zulaessig)} zulässige, {len(unzulaessig)} unzulässige Kombinationen")
print("Beste Lösung: x = %s, y = %s, Kosten = %s EUR, Personen = %s" % beste)
assert beste == (5, 2, 1800, 80)

print("\n--- Ganzzahliger LP-Solver ---")
lp = MixedIntegerLinearProgram(maximization=True, solver="GLPK")
v = lp.new_variable(integer=True, nonnegative=True)
lp.set_objective(10*v[0] + 15*v[1])
lp.add_constraint(v[0] <= 5)
lp.add_constraint(v[1] <= 4)
lp.add_constraint(200*v[0] + 400*v[1] <= 1800)
z = lp.solve()
werte = lp.get_values(v)
loesung = (int(round(werte[0])), int(round(werte[1])), int(round(z)))
print("x = %s, y = %s, Personen = %s" % loesung)
assert loesung == (5, 2, 80)

x = var('x')
G = plot((1800 - 200*x)/400, (x, 0, 5), color="red", legend_label="200x + 400y = 1800")
G += line([(5, 0), (5, 5)], color="black", linestyle="--") + line([(0, 4), (5.2, 4)], color="black", linestyle="--")
# Farbe der zulässigen Punkte nach Personenzahl
farben = colormaps.viridis
for a, b in zulaessig:
    G += point([(a, b)], color=tuple(farben(float((10*a + 15*b)/80))[:3]), size=70, zorder=5)
    G += text(str(10*a + 15*b), (a + 0.12, b + 0.15), color="black", fontsize=8)
for a, b in unzulaessig:
    G += point([(a, b)], color="gray", marker="x", size=40, zorder=5)
G += point([(5, 2)], color="yellow", markeredgecolor="black", size=130, zorder=10)
G.save(os.path.join(ordner, "12_zelte_ganzzahlig.png"), xmin=-0.1, xmax=5.4, ymin=-0.1, ymax=5, frame=True,
       figsize=[9, 6], title="Zelt-Problem: ganzzahlige Lösungen (Zahl = Personen)",
       axes_labels=["Anzahl 10-Personen-Zelte (x)", "Anzahl 15-Personen-Zelte (y)"])
