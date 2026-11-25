# Funktionen: Grafiken und Beispiele
# Folien: "Eigenschaften von reellen Funktionen" bis "Elastizität"
import os


def de(x, stellen=4):
    """Zahl mit Dezimalkomma."""
    return f"{float(x):.{int(stellen)}f}".replace(".", ",")


ordner = os.path.join(os.path.dirname(os.path.abspath(__file__)) if "__file__" in globals() else ".", "plots")
os.makedirs(ordner, exist_ok=True)


def speichern(grafik, name, **optionen):
    grafik.save(os.path.join(ordner, name), axes_labels=["x", ""], frame=False, **optionen)


x = var('x')

print("--- Nullstellen: f(x) = x^2 - 4x + 3 ---")
f = x^2 - 4*x + 3
nst = sorted(s.rhs() for s in solve(f == 0, x))
print("Nullstellen:", nst)
assert nst == [1, 3]
speichern(plot(f, (x, -2, 4), legend_label="$f(x) = x^2 - 4x + 3$")
          + point([(a, 0) for a in nst], color="red", size=50, zorder=10),
          "05a_nullstellen.png")

print("\n--- Achsensymmetrie: f(x) = x^4 - 3x^2 - 3 ---")
f = x^4 - 3*x^2 - 3
print("f(x) - f(-x) =", (f - f.subs(x=-x)).expand())
assert (f - f.subs(x=-x)).expand() == 0
speichern(plot(f, (x, -2.5, 2.5), legend_label="$f(x) = x^4 - 3x^2 - 3$ (gerade)"), "05b_achsensymmetrie.png")

print("\n--- Punktsymmetrie: f(x) = x^3 - 3x ---")
f = x^3 - 3*x
print("f(x) + f(-x) =", (f + f.subs(x=-x)).expand())
assert (f + f.subs(x=-x)).expand() == 0
speichern(plot(f, (x, -2.5, 2.5), legend_label="$f(x) = x^3 - 3x$ (ungerade)"), "05c_punktsymmetrie.png")

print("\n--- Unstetigkeit: Sprungfunktion ---")
sprung = (plot(0, (x, -5, 0), color="red", thickness=2)
          + plot(1, (x, 0, 5), color="red", thickness=2))
speichern(sprung, "05d_unstetigkeit.png", ymin=-0.3, ymax=1.3, title="Beispiel einer unstetigen Funktion")
print("linksseitiger Grenzwert 0, rechtsseitiger Grenzwert 1 -> Sprung bei x = 0")

print("\n--- Verhalten im Unendlichen: f(x) = x ---")
print("x -> +oo:", limit(x, x=oo), "   x -> -oo:", limit(x, x=-oo))
speichern(plot(x, (x, -3, 3), legend_label="$f(x) = x$"), "05e_gerade.png")

print("\n--- Verhalten im Unendlichen: f(x) = 1/x ---")
print("x -> +oo:", limit(1/x, x=oo), "   x -> -oo:", limit(1/x, x=-oo))
rechts, links = limit(1/x, x=0, dir="+"), limit(1/x, x=0, dir="-")
print("x -> 0+ :", rechts, "   x -> 0- :", links)
assert rechts == +Infinity and links == -Infinity
speichern(plot(1/x, (x, -3, -0.05), legend_label="$f(x) = 1/x$") + plot(1/x, (x, 0.05, 3)),
          "05f_hyperbel.png", ymin=-10, ymax=10)

print("\n--- Quadratische Funktionen: Diskriminante (Ergänzung mit Beispielwerten) ---")
for a, b, c in [(1, -4, 3), (1, -6, 9), (1, 0, 1)]:
    D = b^2 - 4*a*c
    anzahl = "zwei reelle Lösungen" if D > 0 else "eine doppelte Lösung" if D == 0 else "keine reelle Lösung"
    print(f"{a}x^2 + ({b})x + ({c}): D = {D} -> {anzahl}")

print("\n--- Linearfaktorzerlegung: f(x) = x^3 - 9x^2 + 27x - 27 ---")
P = PolynomialRing(QQ, 'x')
z = P.gen()  # Polynomvariable, Anzeige als x
f = z^3 - 9*z^2 + 27*z - 27
Q, rest = f.quo_rem(z - 3)
print("Polynomdivision durch (x - 3):", Q, " Rest", rest)
print("Zerlegung:", f.factor())
assert (Q, rest) == (z^2 - 6*z + 9, 0) and f == (z - 3)^3

print("\n--- Echt und unecht gebrochen-rationale Funktionen ---")
for zaehler, nenner, erwartet in [(5*z^2 + 4*z + 3, z^3 - z + 1, "echt"),
                                  (2*z^3 + 2*z - 3, z^3 + 5*z^2, "unecht")]:
    art = "echt" if zaehler.degree() < nenner.degree() else "unecht"
    print(f"({zaehler}) / ({nenner}): Grade {zaehler.degree()} und {nenner.degree()} -> {art} gebrochen")
    assert art == erwartet

print("\n--- Zerlegung durch Polynomdivision: (x^3 - 3x + 5)/(x - 2) ---")
Q, rest = (z^3 - 3*z + 5).quo_rem(z - 2)
print(f"= ({Q}) + ({rest})/(x - 2)")
assert (Q, rest) == (z^2 + 2*z + 1, 7)

print("\n--- Nullstellen gebrochen-rationaler Funktionen (Ergänzung) ---")
Z_, N_ = (z - 1)*(z - 2), (z - 2)*(z + 3)
nullstellen = [a for a in Z_.roots(multiplicities=False) if N_(a) != 0]
print(f"Z(x) = {Z_}, N(x) = {N_}: Nullstellen {nullstellen} (x = 2 liegt nicht in D(f))")

print("\n--- Ableitungen elementarer Funktionen und Ableitungsregeln ---")
r, a, h = var('r a h')
for fkt in [a, x^r, exp(x), log(x)]:
    print(f"f(x) = {fkt}  ->  f'(x) = {diff(fkt, x)}")
g, k_ = function('g')(x), function('k')(x)
print("Produktregel  :", diff(g*k_, x))
print("Quotientenregel:", diff(g/k_, x))
aussen = function('G')
print("Kettenregel   :", diff(aussen(k_), x))
print("Grenzwert des Differenzenquotienten für x^2:", limit(((x + h)^2 - x^2)/h, h=0))

print("\n--- Extremwerte: Gewinnmaximum der Monopolistin ---")
p = 6 - x/2
K = 2*x^2 - 14*x + 25
G = (x*p - K).expand()
print("Umsatz U(x) =", (x*p).expand())
print("Gewinn G(x) =", G)
x_e = solve(diff(G, x) == 0, x)[0].rhs()
G2 = diff(G, x, 2)
print(f"G'(x) = {diff(G, x)} = 0 -> x = {x_e};  G'' = {G2} < 0 -> Maximum; G({x_e}) = {G.subs(x=x_e)}")
assert G == -5/2*x^2 + 20*x - 25 and x_e == 4 and G2 < 0 and G.subs(x=x_e) == 15
speichern(plot(G, (x, 0, 8), legend_label="$G(x) = -5/2 x^2 + 20x - 25$")
          + point([(x_e, 15)], color="red", size=50, zorder=10)
          + text("Maximum (4, 15)", (4.15, 15.5), color="red", horizontal_alignment="left"),
          "05g_gewinn.png")

print("\n--- Elastizität (Ergänzung: Gewinnfunktion an der Stelle x0 = 3) ---")
eps = diff(G, x) / G * x
x0 = 3
print(f"Elastizität bei x0 = {x0}: {eps.subs(x=x0)} = {de(eps.subs(x=x0))}")
print(f"tatsächliche Änderung bei +1 %: {de(100*(G.subs(x=x0*101/100)/G.subs(x=x0) - 1))} %")

print("\nGrafiken gespeichert in", ordner)
