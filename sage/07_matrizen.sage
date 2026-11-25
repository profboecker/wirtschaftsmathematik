# Rechnen mit Matrizen und Vektoren
# Folien: "Matrizen, Spalten- und Zeilenvektoren" bis "Übergangsmatrizen und Matrizenmultiplikation"
# (Wählerwanderung: siehe 08_waehlerwanderung.sage)

print("--- 3x5-Matrix, Spalten- und Zeilenvektor ---")
A = matrix(QQ, [[2, 3, -1, 2/10, -7],
                [8, 9, -3, 5, 31/10],
                [15/10, 31/10, -32/10, 6, -1]])
print(A.change_ring(RDF))
print("Dimension:", A.dimensions())
a3, z2 = A.column(2), A.row(1)
print("dritte Spalte :", a3.change_ring(RDF))
print("zweite Zeile  :", z2.change_ring(RDF))
assert list(a3) == [-1, -3, -32/10] and list(z2) == [8, 9, -3, 5, 31/10]

print("\n--- Summen von Vektoren und Matrizen ---")
var('a1 a2 a3_ b1 b2 b3')
print(vector([a1, a2, a3_]) + vector([b1, b2, b3]))

print("\n--- Vielfache ---")
v = A.column(2)
print("3 * v =", (3*v).change_ring(RDF))
assert list(3*v) == [-3, -9, -96/10]
x2 = var('x2')
w = vector([2, 0, 4]) + x2*vector([3, 1, 0])
print("(2,0,4) + x2*(3,1,0) =", w)
assert w == vector([2 + 3*x2, x2, 4])

print("\n--- Skalarprodukt ---")
s = vector([2, -1, 3]) * vector([1, 2, 1])
print("(2,-1,3) . (1,2,1) =", s)
assert s == 3


def matrizenprodukt(A, B):
    """Produkt nach der Definition c_ij = Summe a_ik * b_kj, mit Rechenweg."""
    assert A.ncols() == B.nrows(), "Spaltenzahl von A muss Zeilenzahl von B sein"
    C = matrix(QQ, A.nrows(), B.ncols())
    for i in range(A.nrows()):
        for j in range(B.ncols()):
            summanden = [f"({A[i, k]})*({B[k, j]})" for k in range(A.ncols())]
            C[i, j] = sum(A[i, k]*B[k, j] for k in range(A.ncols()))
            print(f"c{i+1}{j+1} = {' + '.join(summanden)} = {C[i, j]}")
    return C


print("\n--- Beispiel für das Produkt zweier Matrizen ---")
A = matrix(QQ, [[3, 2, -4], [0, 6, -5]])
B = matrix(QQ, [[7, 0], [4, 1], [-3, 0]])
C = matrizenprodukt(A, B)
print(C)
assert C == A*B == matrix([[41, 2], [39, 6]])

print("\n--- Lineare Gleichungssysteme und Matrizenmultiplikation ---")
A = matrix(QQ, [[-5, -2, -1], [3, 1, 0], [-6, -2, 1]])
b = vector(QQ, [-18, 9, 20])
x1, x2, x3 = var('x1 x2 x3')
print("A * x =", A * vector([x1, x2, x3]))
loesung = A.solve_right(b)
print("Lösung x =", loesung)
print("Probe A * x =", A*loesung)
assert loesung == vector([38, -105, 38]) and A*loesung == b

print("\n--- Verkettung von Produktionsschritten ---")
RZ = matrix(QQ, [[2, 4, 3], [3, 7, 6], [1, 3, 2]])  # Rohstoffe je Zwischenprodukt
ZE = matrix(QQ, [[4, 11], [7, 10], [3, 2]])         # Zwischenprodukte je Endprodukt
RE = RZ * ZE
print("Rohstoffe je Endprodukt:")
print(RE)
assert RE == matrix([[45, 68], [79, 115], [31, 45]])
e = vector([10, 15])
z = ZE * e
r = RZ * z
print(f"Für E = {e}: Zwischenprodukte Z = {z}, Rohstoffe R = {r}")
assert r == RE*e == vector([1470, 2515, 985])

# Mengenfluss als Graph (Ergänzung, nicht auf der Folie)
import os
ordner = os.path.join(os.path.dirname(os.path.abspath(__file__)) if "__file__" in globals() else ".", "plots")
os.makedirs(ordner, exist_ok=True)
kanten = [(f"R{i+1}", f"Z{j+1}", RZ[i, j]) for i in range(3) for j in range(3)]
kanten += [(f"Z{j+1}", f"E{k+1}", ZE[j, k]) for j in range(3) for k in range(2)]
pos = {**{f"R{i+1}": (0, 2 - i) for i in range(3)},
       **{f"Z{j+1}": (2, 2 - j) for j in range(3)},
       **{f"E{k+1}": (4, 1.5 - k) for k in range(2)}}
DiGraph(kanten).plot(pos=pos, edge_labels=True, vertex_size=600).save(
    os.path.join(ordner, "07_produktionsgraph.png"), figsize=[10, 7])
