# Lineare Optimierung mit dem Simplex-Verfahren: Produktionsprogrammplanung
# Foliensatz: LineareOptimierung/LineareOptimierung.Rmd
import os

ordner = os.path.join(os.path.dirname(os.path.abspath(__file__)) if "__file__" in globals() else ".", "plots")
os.makedirs(ordner, exist_ok=True)

A = matrix(QQ, [[4, 2], [10, 30], [1, 1]])
b = vector(QQ, [40, 120, 9])
c = vector(QQ, [40, 30])
namen = ["x1", "x2", "s1", "s2", "s3"]
ressourcen = ["Arbeitszeit", "Material", "Lager"]

print("--- Standardform: A x + s = b ---")
A_std = A.augment(identity_matrix(QQ, 3))
c_std = vector(QQ, list(c) + [0, 0, 0])
print(A_std)
print("b =", b, "  c =", c_std)


def ausgeben(T, basis):
    zeilen = [["Basis"] + namen + ["RHS"]]
    zeilen += [[name] + [str(a) for a in T[i]] for i, name in enumerate(basis + ["Z"])]
    for zeile in zeilen:
        print("".join(f"{eintrag:>7}" for eintrag in zeile))
    print()


T = block_matrix(QQ, [[A_std, matrix(QQ, 3, 1, list(b))],
                      [matrix(QQ, 1, 5, list(-c_std)), 0]], subdivide=False)
basis = ["s1", "s2", "s3"]
print("\n--- Initiales Tableau (Iteration 0) ---")
ausgeben(T, basis)
assert T == matrix(QQ, [[4, 2, 1, 0, 0, 40], [10, 30, 0, 1, 0, 120],
                        [1, 1, 0, 0, 1, 9], [-40, -30, 0, 0, 0, 0]])

gewinne = [T[3, 5]]
iteration = 0
while min(list(T[3])[:5]) < 0:
    iteration += 1
    zf = list(T[3])[:5]
    s = zf.index(min(zf))
    print(f"Eingangsvariable: {namen[s]} (kleinster Wert {min(zf)} in der Z-Reihe)")
    quotienten = [(T[i, 5]/T[i, s], i) for i in range(3) if T[i, s] > 0]
    for q, i in quotienten:
        print(f"  Zeile {i+1}: {T[i, 5]}/{T[i, s]} = {q}")
    q, z = min(quotienten)
    print(f"Ausgangsvariable: {basis[z]}, Pivotelement {T[z, s]} (Zeile {z+1})")
    T.rescale_row(z, 1/T[z, s])
    for i in range(4):
        if i != z and T[i, s] != 0:
            faktor = -T[i, s]
            T.add_multiple_of_row(i, z, faktor)
            print(f"  Zeile {i+1} + ({faktor}) * Zeile {z+1}" if i < 3 else f"  Z-Reihe + ({faktor}) * Zeile {z+1}")
    basis[z] = namen[s]
    gewinne.append(T[3, 5])
    print(f"\n--- Tableau nach Iteration {iteration} ---")
    ausgeben(T, basis)

print("Optimalitätstest: Z-Reihe", list(T[3])[:5], ">= 0 -> Optimum erreicht")
assert iteration == 1 and basis == ["s1", "s2", "x1"]
assert T == matrix(QQ, [[0, -2, 1, 0, -4, 4], [0, 20, 0, 1, -10, 30],
                        [1, 1, 0, 0, 1, 9], [0, 10, 0, 0, 40, 360]])

werte = {name: 0 for name in namen}
werte.update({name: T[i, 5] for i, name in enumerate(basis)})
x_opt = vector(QQ, [werte["x1"], werte["x2"]])
print(f"\n--- Optimale Lösung ---\n{werte}\nMaximaler Gewinn Z = {c*x_opt} EUR")
assert x_opt == vector([9, 0]) and c*x_opt == 360

print("\n--- Ressourcen-Auslastung ---")
verbraucht = A*x_opt
print(f"{'Ressource':12} {'verfügbar':>9} {'verbraucht':>10} {'Rest':>5} {'Auslastung':>11}")
for name, v, g in zip(ressourcen, verbraucht, b):
    print(f"{name:12} {str(g):>9} {str(v):>10} {str(g - v):>5} {float(100*v/g):>9.1f} %".replace(".", ","))
assert list(b - verbraucht) == [4, 30, 0]

print("\n--- Schattenpreis der Lagerkapazität: eine Einheit mehr ---")
P1 = Polyhedron(ieqs=[[40, -4, -2], [120, -10, -30], [10, -1, -1], [0, 1, 0], [0, 0, 1]])
z_mehr = max(c*vector(v) for v in P1.vertices())
print(f"Lager 10 statt 9: Z = {z_mehr} EUR, Zuwachs {z_mehr - 360} EUR")
assert z_mehr - 360 == T[3, 4] == 40

print("\n--- Geometrische Interpretation ---")
P = Polyhedron(ieqs=[[b[i], -A[i, 0], -A[i, 1]] for i in range(3)] + [[0, 1, 0], [0, 0, 1]])
ecken = sorted(tuple(v) for v in P.vertices())
for e in ecken:
    print(f"  Ecke ({e[0]}, {e[1]}): Z = {c*vector(e)}")
assert ecken == [(0, 0), (0, 4), (15/2, 3/2), (9, 0)]

x = var('x')
G = P.plot(fill="lightblue", line="lightblue", point=False)
for i, farbe in enumerate(["blue", "green", "red"]):
    G += plot((b[i] - A[i, 0]*x)/A[i, 1], (x, 0, 13), color=farbe, linestyle="--",
              legend_label=ressourcen[i])
G += plot((360 - 40*x)/30, (x, 0, 9), color="purple", thickness=2, legend_label="Iso-Gewinnlinie Z = 360")
for e in ecken:
    optimum = tuple(e) == (9, 0)
    G += point([e], color="red" if optimum else "orange", size=90 if optimum else 45, zorder=10)
    beschriftung = f"({float(e[0]):g} | {float(e[1]):g}), Z={float(c*vector(e)):g}".replace(".", ",")
    G += text(beschriftung, (e[0] + 0.2, e[1] + 0.3), color="black", horizontal_alignment="left")
G.save(os.path.join(ordner, "11a_geometrie.png"), xmin=0, xmax=13, ymin=0, ymax=8, frame=True, figsize=[10, 7],
       axes_labels=["x1 (Produkt A)", "x2 (Produkt B)"])

print("\n--- Konvergenz des Simplex-Algorithmus ---")
print("Gewinn je Iteration:", gewinne)
assert gewinne == [0, 360]
K = line(list(enumerate(gewinne)), color="darkblue") + point(list(enumerate(gewinne)), color="red", size=60)
for k, g in enumerate(gewinne):
    K += text(f"Z = {g} EUR", (k, g + 20), color="black")
K.save(os.path.join(ordner, "11b_konvergenz.png"), xmin=-0.2, xmax=1.2, ymin=-10, ymax=400, frame=True, ticks=[[0, 1], None],
       axes_labels=["Iteration", "Zielfunktionswert [EUR]"])

print("\n--- Kontrolle mit dem LP-Solver (entspricht lpSolve auf den Folien) ---")
lp = MixedIntegerLinearProgram(maximization=True, solver="GLPK")
v = lp.new_variable(real=True, nonnegative=True)
lp.set_objective(40*v[0] + 30*v[1])
for i in range(3):
    lp.add_constraint(A[i, 0]*v[0] + A[i, 1]*v[1] <= b[i])
z = lp.solve()
loesung = lp.get_values(v)
print(f"x1 = {int(round(loesung[0]))}, x2 = {int(round(loesung[1]))}, Z = {int(round(z))} EUR")
assert abs(z - 360) < 1e-8 and abs(loesung[0] - 9) < 1e-8 and abs(loesung[1]) < 1e-8
