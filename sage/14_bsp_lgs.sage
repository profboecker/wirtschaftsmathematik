# LGS-Beispiele (Gauß-Schritte in der Reihenfolge des Foliensatzes)
# Foliensatz: Beispiele/LGS_Beispiel/LGS_Beispiel.Rmd


def tableau(zeilen):
    M = matrix(QQ, zeilen)
    M.subdivide([], [3])
    return M


def schritt(M, beschreibung):
    print(f"{beschreibung}:")
    print(M, end="\n\n")


print("=== Eindeutige Lösung ===")
M = tableau([[1, -1, 2, 9], [2, 1, 0, 3], [1, 2, -1, -3]])
original = copy(M)
schritt(M, "Ausgangstableau")
M.add_multiple_of_row(1, 0, -2)
M.add_multiple_of_row(2, 0, -1)
schritt(M, "II - 2 I,  III - I")
assert M == matrix(QQ, [[1, -1, 2, 9], [0, 3, -4, -15], [0, 3, -3, -12]])
M.add_multiple_of_row(2, 1, -1)
schritt(M, "III - II")
assert M == matrix(QQ, [[1, -1, 2, 9], [0, 3, -4, -15], [0, 0, 1, 3]])
M.add_multiple_of_row(1, 2, 4)
schritt(M, "II + 4 III")
assert M == matrix(QQ, [[1, -1, 2, 9], [0, 3, 0, -3], [0, 0, 1, 3]])
M.add_multiple_of_row(0, 2, -2)
M.rescale_row(1, 1/3)
schritt(M, "I - 2 III,  II : 3")
assert M == matrix(QQ, [[1, -1, 0, 3], [0, 1, 0, -1], [0, 0, 1, 3]])
M.add_multiple_of_row(0, 1, 1)
schritt(M, "I + II")
assert M == original.rref() and list(M.column(3)) == [2, -1, 3]
print("Lösung: x1 = 2, x2 = -1, x3 = 3\n")

print("=== Unendlich viele Lösungen ===")
M = tableau([[0, 1, 2, 9], [-2, 2, 1, 3], [2, -1, 1, 6]])
A, b = M[:, :3], M.column(3)
schritt(M, "Ausgangstableau")
M.swap_rows(0, 2)
schritt(M, "I <-> III")
M.add_multiple_of_row(1, 0, 1)
schritt(M, "II + I")
M.add_multiple_of_row(2, 1, -1)
schritt(M, "III - II (Nullzeile)")
assert M == matrix(QQ, [[2, -1, 1, 6], [0, 1, 2, 9], [0, 0, 0, 0]])
t = var('t')
partikulaer = vector(QQ, [15/2, 9, 0])
richtung = vector(QQ, [-3/2, -2, 1])
print(f"x = {partikulaer} + t * {richtung}")
assert A*partikulaer == b and A*richtung == 0 and A.right_kernel().dimension() == 1
for wert in [0, 1]:
    print(f"  t = {wert}: {partikulaer + wert*richtung}")
assert partikulaer + richtung == vector([6, 7, 1])

print("\n=== Keine Lösung ===")
M = tableau([[2, -1, 1, 6], [-2, 2, 1, 3], [0, 1, 2, 8]])
rang_A, rang_Ab = M[:, :3].rank(), M.rank()
schritt(M, "Ausgangstableau")
M.add_multiple_of_row(1, 0, 1)
schritt(M, "II + I")
M.add_multiple_of_row(2, 1, -1)
schritt(M, "III - II")
assert M == matrix(QQ, [[2, -1, 1, 6], [0, 1, 2, 9], [0, 0, 0, -1]])
print(f"0 = -1 ist unerfüllbar. Rang(A) = {rang_A} < Rang(A|b) = {rang_Ab}: keine Lösung.")
assert rang_A < rang_Ab
