# Lineare Optimierung
# Folien: "Einführung in die Problemstellung" und "Beispiel Zelte" (Grafiken und Lösung)
# (Simplex-Tableaus: siehe 10_simplex.sage)
import os

ordner = os.path.join(os.path.dirname(os.path.abspath(__file__)) if "__file__" in globals() else ".", "plots")
os.makedirs(ordner, exist_ok=True)


def speichern(grafik, name, **optionen):
    grafik.save(os.path.join(ordner, name), **optionen)


x, y = var('x y')

print("--- Schnittpunkt: 2x1 + x2 = 5 und x1 - x2 = 1 ---")
lsg = solve([2*x + y == 5, x - y == 1], [x, y], solution_dict=True)[0]
print(lsg)
assert (lsg[x], lsg[y]) == (2, 1)
speichern(plot(5 - 2*x, (x, -7, 7), color="red", legend_label="Gleichung I")
          + plot(x - 1, (x, -7, 7), color="blue", legend_label="Gleichung II")
          + point([(2, 1)], color="black", size=50, zorder=10)
          + text("Schnittpunkt (2, 1)", (2.3, 0.3), color="black", horizontal_alignment="left"),
          "09a_schnittpunkt.png", frame=True, axes_labels=["x", "y"])

print("--- Halbebene y <= -2x + 5 ---")
speichern(region_plot(y <= -2*x + 5, (x, -7, 7), (y, -10, 10), incol="lightblue", plot_points=300)
          + plot(-2*x + 5, (x, -2.5, 7), color="black"),
          "09b_halbebene_I.png", title="y <= -2x + 5")

print("--- Halbebene y >= x - 1 ---")
speichern(region_plot(y >= x - 1, (x, -7, 7), (y, -10, 10), incol="lightblue", plot_points=300)
          + plot(x - 1, (x, -7, 7), color="black"),
          "09c_halbebene_II.png", title="y >= x - 1")

print("\n--- Lösungsmenge des Ungleichungssystems ---")
P = Polyhedron(ieqs=[[5, -2, -1], [1, -1, 1], [0, 1, 0], [0, 0, 1]])  # b + a1*x + a2*y >= 0
ecken = sorted(tuple(v) for v in P.vertices())
print("Eckpunkte:", ecken)
assert ecken == [(0, 0), (0, 5), (1, 0), (2, 1)]
speichern(P.plot(fill="lightblue")
          + plot(-2*x + 5, (x, -0.5, 3.5), color="black")
          + plot(x - 1, (x, -1, 4), color="black"),
          "09d_loesungsmenge.png", xmin=-1, xmax=4, ymin=-2, ymax=6)

print("\n=== Beispiel Zelte ===")
Z = 10*x + 15*y
# x <= 5, y <= 4, 200x + 400y <= 1800, x >= 0, y >= 0
zelte = Polyhedron(ieqs=[[5, -1, 0], [4, 0, -1], [1800, -200, -400], [0, 1, 0], [0, 0, 1]])
ecken = sorted(tuple(v) for v in zelte.vertices())
print("Eckpunkte des zulässigen Bereichs und Personenzahl:")
for ex, ey in ecken:
    print(f"  ({ex}, {ey}): Z = {Z.subs(x=ex, y=ey)}")
assert ecken == [(0, 0), (0, 4), (1, 4), (5, 0), (5, 2)]

print("\n1. Frage: mögliche Zeltkombinationen (ganzzahlig)")
moeglich = [(a, b) for a in range(6) for b in range(5) if 200*a + 400*b <= 1800]
print(f"{len(moeglich)} Kombinationen:", moeglich)

print("\n2. Frage: maximale Personenzahl")
bestes = max(moeglich, key=lambda p: 10*p[0] + 15*p[1])
print(f"Durchprobieren: {bestes} mit Z = {10*bestes[0] + 15*bestes[1]}")
assert bestes == (5, 2)

lp = MixedIntegerLinearProgram(maximization=True, solver="GLPK")
v = lp.new_variable(integer=True, nonnegative=True)
lp.set_objective(10*v[0] + 15*v[1])
lp.add_constraint(v[0] <= 5)
lp.add_constraint(v[1] <= 4)
lp.add_constraint(200*v[0] + 400*v[1] <= 1800)
z_max = lp.solve()
werte = lp.get_values(v)
print(f"LP-Solver     : x = {int(round(werte[0]))}, y = {int(round(werte[1]))}, Z = {int(round(z_max))}")
assert (round(werte[0]), round(werte[1]), round(z_max)) == (5, 2, 80)

basis = (zelte.plot(fill="lightblue", line="lightblue", point=False)
         + plot((1800 - 200*x)/400, (x, 0, 9), color="red")
         + line([(5, -0.3), (5, 5.5)], color="black", linestyle="--")
         + line([(0, 4), (9, 4)], color="black", linestyle="--"))
achsen = dict(xmin=0, xmax=9, ymin=-0.3, ymax=5.5, frame=True, figsize=[9, 6],
              axes_labels=["x (10-Personen-Zelte)", "y (15-Personen-Zelte)"])


def zielgerade(wert, **stil):
    return (plot((wert - 10*x)/15, (x, 0, 7), color="blue", **stil)
            + text(f"Z = {wert}", (3, (wert - 30)/15 + 0.2), color="blue"))


speichern(basis + text("200x + 400y = 1800", (7.4, 1.5), color="red")
          + text("x = 5", (5.5, 3), color="black") + text("y = 4", (2.5, 4.25), color="black"),
          "09e_zelte_bereich.png", title="Zulässiger Bereich", **achsen)
speichern(basis + zielgerade(60), "09f_zelte_z60.png", title="Zielfunktion Z = 60", **achsen)
speichern(basis + zielgerade(60, linestyle="--") + zielgerade(70, linestyle="--") + zielgerade(80, thickness=2)
          + point([(5, 2)], color="black", size=60, zorder=10),
          "09g_zelte_maximum.png", title="Zielfunktion maximieren", **achsen)

print("\nGrafiken gespeichert in", ordner)
