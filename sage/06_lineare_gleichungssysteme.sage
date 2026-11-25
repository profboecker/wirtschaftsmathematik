# Lineare Gleichungssysteme und Gauß-Algorithmus
# Folien: "Beispiel zum Einstieg" bis "Gauß-Algorithmus - Stufen- oder Treppenform"
import os

ordner = os.path.join(os.path.dirname(os.path.abspath(__file__)) if "__file__" in globals() else ".", "plots")
os.makedirs(ordner, exist_ok=True)

x, y, t = var('x y t')


def tableau(zeilen):
    """Erweiterte Koeffizientenmatrix (letzte Spalte = rechte Seite)."""
    M = matrix(QQ, zeilen)
    M.subdivide([], [M.ncols() - 1])
    return M


def schritt(M, beschreibung):
    print(f"{beschreibung}:")
    print(M, end="\n\n")


print("--- Bauernhof: 100 Tiere mit 320 Beinen ---")
gl1, gl2 = 4*x + 2*y == 320, x + y == 100

print("Einsetzungsverfahren:")
y_von_x = solve(gl2, y)[0].rhs()
eingesetzt = gl1.subs(y=y_von_x)
x_s = solve(eingesetzt, x)[0].rhs()
y_s = y_von_x.subs(x=x_s)
print(f"  y = {y_von_x};  eingesetzt: {eingesetzt};  x = {x_s}, y = {y_s}")
assert (x_s, y_s) == (60, 40)

print("Additionsverfahren:")
summe = gl1 + (-2)*gl2
print(f"  I + (-2)*II: {summe}  ->  x = {solve(summe, x)[0].rhs()}")
assert solve(summe, x)[0].rhs() == 60

print("Probe:", "Tiere", x_s + y_s, ", Beine", 4*x_s + 2*y_s)


def zwei_geraden(f1, f2, bereich, namen, titel, datei, **optionen):
    """Zeichnet zwei Geraden und, falls vorhanden, ihren Schnittpunkt."""
    G = (plot(f1, (x, *bereich), color="red", legend_label=namen[0])
         + plot(f2, (x, *bereich), color="blue", legend_label=namen[1]))
    loesung = solve(f1 == f2, x)
    if loesung and (f1 - f2).diff(x) != 0:
        xs = loesung[0].rhs()
        ys = f1.subs(x=xs)
        G += point([(xs, ys)], color="black", size=50, zorder=10)
        xt, yt = (f"{float(a):.4g}".replace(".", ",") for a in (xs, ys))
        G += text(f"Schnittpunkt ({xt} | {yt})", (xs, ys),
                  color="black", horizontal_alignment="left", vertical_alignment="top")
    G.save(os.path.join(ordner, datei), title=titel, frame=True, axes_labels=["x", "y"], **optionen)


print("\n--- Grafische Lösung (Bauernhof) ---")
zwei_geraden(160 - 2*x, 100 - x, (0, 100), ["Anzahl Beine", "Anzahl Tiere"],
             "Graph der beiden Geraden mit Schnittpunkt", "06a_bauernhof.png", ymin=0)

print("\n--- Unendlich viele Lösungen: 2x - y = 1 und 4x - 2y = 2 ---")
print(solve([2*x - y == 1, 4*x - 2*y == 2], [x, y]))
A = matrix(QQ, [[2, -1], [4, -2]])
assert A.rank() == A.augment(vector(QQ, [1, 2])).rank() == 1
G = plot(2*x - 1, (x, -4, 4), color="red", legend_label="2x - y = 1 identisch mit 4x - 2y = 2")
G.save(os.path.join(ordner, "06b_unendlich_viele.png"), ymin=-4, ymax=4,
       title="Unendlich viele Lösungen: identische Geraden", frame=True, axes_labels=["x", "y"])

print("\n--- Keine Lösung: 2x - y = 1 und 4x - 2y = 3 ---")
print(solve([2*x - y == 1, 4*x - 2*y == 3], [x, y]))
assert A.rank() == 1 and A.augment(vector(QQ, [1, 3])).rank() == 2
zwei_geraden(2*x - 1, 2*x - 3/2, (-4, 4), ["2x - y = 1", "4x - 2y = 3"],
             "Keine Lösung: parallele Geraden", "06c_keine_loesung.png")

print("\n--- Genau eine Lösung: 2x - y = 1 und 4x + 2y = 3 ---")
lsg = solve([2*x - y == 1, 4*x + 2*y == 3], [x, y], solution_dict=True)[0]
print(lsg)
assert (lsg[x], lsg[y]) == (5/8, 1/4)
zwei_geraden(2*x - 1, 3/2 - 2*x, (-4, 4), ["Gleichung 1", "Gleichung 2"],
             "Genau eine Lösung", "06d_eine_loesung.png")

print("\n=== Gauß-Algorithmus: Beispiel vorab ===")
M = tableau([[1, -2, 1, 5], [-5, -2, -1, -21], [6, 2, -1, 16]])
schritt(M, "Ausgangstableau")
M.add_multiple_of_row(1, 0, 5)
M.add_multiple_of_row(2, 0, -6)
schritt(M, "II + 5 I,  III - 6 I")
assert M == matrix(QQ, [[1, -2, 1, 5], [0, -12, 4, 4], [0, 14, -7, -14]])
M.rescale_row(1, -1/12)
schritt(M, "II : (-12)")
M.add_multiple_of_row(2, 1, -14)
schritt(M, "III - 14 II")
assert list(M[2]) == [0, 0, -7/3, -28/3]
x3 = M[2, 3] / M[2, 2]
x2 = M[1, 3] - M[1, 2]*x3
x1 = M[0, 3] - M[0, 1]*x2 - M[0, 2]*x3
print(f"Rückwärtseinsetzen: x1 = {x1}, x2 = {x2}, x3 = {x3}")
assert (x1, x2, x3) == (3, 1, 4)

print("\n=== Unendlich viele Lösungen ===")
M = tableau([[0, 1, 2, 9], [-2, 2, 1, 3], [2, -1, 1, 6]])
schritt(M, "Ausgangstableau")
M.swap_rows(0, 2)
schritt(M, "I <-> III")
M.add_multiple_of_row(1, 0, 1)
schritt(M, "II + I")
M.add_multiple_of_row(2, 1, -1)
schritt(M, "III - II")
assert M == matrix(QQ, [[2, -1, 1, 6], [0, 1, 2, 9], [0, 0, 0, 0]])
x3 = t
x2 = solve(x + 2*t == 9, x)[0].rhs()
x1 = solve(2*x - x2 + t == 6, x)[0].rhs()
print(f"x1 = {x1},  x2 = {x2},  x3 = {x3}  (t beliebig)")
assert bool(x1 == 15/2 - 3*t/2) and bool(x2 == 9 - 2*t)
for wert in [0, 1, 5]:
    loes = [ausdruck.subs(t=wert) for ausdruck in (x1, x2, x3)]
    print(f"  Probe t = {wert}: {loes}")
    assert [0*loes[0] + loes[1] + 2*loes[2], -2*loes[0] + 2*loes[1] + loes[2],
            2*loes[0] - loes[1] + loes[2]] == [9, 3, 6]

print("\n=== Genau eine Lösung ===")
M = tableau([[1, -1, 2, 9], [2, 1, 0, 3], [1, 2, -1, -3]])
schritt(M, "Ausgangstableau")
M.add_multiple_of_row(1, 0, -2)
M.add_multiple_of_row(2, 0, -1)
schritt(M, "II - 2 I,  III - I")
M.add_multiple_of_row(2, 1, -1)
schritt(M, "III - II")
assert M == matrix(QQ, [[1, -1, 2, 9], [0, 3, -4, -15], [0, 0, 1, 3]])
M.add_multiple_of_row(1, 2, 4)
M.rescale_row(1, 1/3)
schritt(M, "II + 4 III, dann II : 3")
M.add_multiple_of_row(0, 2, -2)
schritt(M, "I - 2 III")
M.add_multiple_of_row(0, 1, 1)
schritt(M, "I + II")
assert list(M.column(3)) == [2, -1, 3] and M[:, :3] == identity_matrix(3)

print("=== Keine Lösung ===")
M = tableau([[2, -1, 1, 6], [-2, 2, 1, 3], [0, 1, 2, 8]])
schritt(M, "Ausgangstableau")
M.add_multiple_of_row(1, 0, 1)
schritt(M, "II + I")
M.add_multiple_of_row(2, 1, -1)
schritt(M, "III - II")
assert M == matrix(QQ, [[2, -1, 1, 6], [0, 1, 2, 9], [0, 0, 0, -1]])
print("Letzte Zeile 0 = -1: Widerspruch, keine Lösung.")

print("\n=== Stufen- oder Treppenform ===")
M = tableau([[1, 0, 1, 0, 0, 0, 1], [0, 1, 2, 3, 0, 0, 2],
             [0, 0, 0, 0, 1, 0, 3], [0, 0, 0, 0, 0, 1, 4]])
schritt(M, "Lösungsmatrix")
assert M == M.rref()
stufen = [s + 1 for s in M.pivots()]
frei = [s for s in range(1, 7) if s not in stufen]
print("Stufen in den Spalten", stufen, "-> frei wählbar:", [f"x{s}" for s in frei])
assert stufen == [1, 2, 5, 6] and frei == [3, 4]
A, b = M[:, :6], M.column(6)
print("Spezielle Lösung:", A.solve_right(b))
print("Basis der homogenen Lösungen:")
print(A.right_kernel_matrix())
