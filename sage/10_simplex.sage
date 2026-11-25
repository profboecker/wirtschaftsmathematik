# Simplex-Algorithmus am Zelt-Beispiel
# Folien: "Lineare Optimierung: Simplex-Algorithmus (Zelt-Beispiel)"
# Exaktes Rechnen mit Brüchen; gleiche Pivotwahl wie auf den Folien.

spalten = ["x1", "x2", "y1", "y2", "y3", "Z", "b"]


def ausgeben(T, basis):
    zeilen = [["BV"] + spalten]
    zeilen += [[name] + [str(a) for a in T[i]] for i, name in enumerate(basis + ["ZF"])]
    for z in zeilen:
        print("".join(f"{eintrag:>8}" for eintrag in z))
    print()


def simplex(A, b, c):
    """Standard-Maximum-Problem: max c*x mit A*x <= b, x >= 0 und b >= 0."""
    m, n = A.nrows(), A.ncols()
    T = block_matrix(QQ, [[A, identity_matrix(QQ, m), zero_matrix(QQ, m, 1), matrix(QQ, m, 1, b)],
                          [matrix(QQ, 1, n, [-ci for ci in c]), zero_matrix(QQ, 1, m), 1, 0]],
                     subdivide=False)
    basis = [f"y{i+1}" for i in range(m)]
    print("Ausgangstableau:")
    ausgeben(T, basis)
    verlauf = [copy(T)]
    schritt = 0
    while True:
        zf = list(T[m])[:n + m]
        if min(zf) >= 0:
            break
        # 1. Simplex-Kriterium: negativster Eintrag der Zielfunktionszeile
        s = zf.index(min(zf))
        # 2. Simplex-Kriterium: kleinster Quotient b_i / a_is mit a_is > 0
        quotienten = [(T[i, n + m + 1] / T[i, s], i) for i in range(m) if T[i, s] > 0]
        if not quotienten:
            raise ValueError("Zielfunktion ist unbeschränkt")
        q, z = min(quotienten)
        schritt += 1
        print(f"Schritt {schritt}: Pivot-Spalte {spalten[s]}, Quotienten "
              + ", ".join(f"{basis[i]}: {wert}" for wert, i in quotienten)
              + f" -> Pivot-Zeile {basis[z]}, Pivot-Element {T[z, s]}")
        T.rescale_row(z, 1 / T[z, s])
        for i in range(m + 1):
            if i != z:
                T.add_multiple_of_row(i, z, -T[i, s])
        basis[z] = spalten[s]
        ausgeben(T, basis)
        verlauf.append(copy(T))
    return T, basis, verlauf


A = matrix(QQ, [[1, 0], [0, 1], [200, 400]])
T, basis, verlauf = simplex(A, [5, 4, 1800], [10, 15])

werte = {name: 0 for name in spalten[:5]}
werte.update({name: T[i, 6] for i, name in enumerate(basis)})
print("Lösung:", werte, " Z =", T[3, 6])

# Abgleich mit den Folien
assert verlauf[1] == matrix(QQ, [[1, 0, 1, 0, 0, 0, 5], [0, 1, 0, 1, 0, 0, 4],
                                 [200, 0, 0, -400, 1, 0, 200], [-10, 0, 0, 15, 0, 1, 60]])
assert verlauf[2] == matrix(QQ, [[0, 0, 1, 2, -1/200, 0, 4], [0, 1, 0, 1, 0, 0, 4],
                                 [1, 0, 0, -2, 1/200, 0, 1], [0, 0, 0, -5, 1/20, 1, 70]])
assert verlauf[3] == matrix(QQ, [[0, 0, 1/2, 1, -1/400, 0, 2], [0, 1, -1/2, 0, 1/400, 0, 2],
                                 [1, 0, 1, 0, 0, 0, 5], [0, 0, 5/2, 0, 3/80, 1, 80]])
assert basis == ["y2", "x2", "x1"] and (werte["x1"], werte["x2"], T[3, 6]) == (5, 2, 80)
print("Alle Tableaus stimmen mit den Folien überein.")
