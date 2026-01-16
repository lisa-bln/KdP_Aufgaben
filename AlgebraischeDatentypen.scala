//1a)
enum Form:
  case Rechteck(laenge:Double,hoehe:Double)
  case Kreis(radius:Double)
  case Quadrat(breite: Double)
  case Parallelogram(hoeheA: Double, laengeA: Double, laengeB: Double)
  case Dreieck(laengeA: Double, laengeB: Double, laengeC: Double)

//1b)
//Signatur: Form => Double
//Spezifikation:
//Voraussetzung: Längen echt größer 0, also keine leeren Formen zulässig.
//Effekt: keiner
//Ergebnis: Gibt Fläche der Form aus.    
def flaeche(f:Form):Double=
  import Form.*
  f match
    case Rechteck(a, b) => a*b
    case Kreis(r) => math.Pi *r*r 
    case Quadrat(a) => a * a
    case Parallelogram(h, a, b) =>  h * a
    case Dreieck(a, b, c) => 
      val s = (a + b + c)/2
      math.sqrt(s * (s - a) * (s - b) * (s - c))

//1c)
//Signatur: Form => Double
//Spezifikation:
//Voraussetzung: Längen echt größer 0, also keine leeren Formen.
//Effekt: keiner
//Ergebnis: Gibt Umfang der Form aus.
def umfang(f: Form):Double= 
  import Form.*
  f match
    case Rechteck(a, b) => 2 * (a + b)
    case Kreis(r) => 2 * math.Pi * r
    case Quadrat(a) => 4 * a
    case Parallelogram(h, a, b) => 2 * (a + b)
    case Dreieck(a, b, c) => a + b + c

//1d)
object Form:
  given Ordering[Form] with
    def compare(f1: Form, f2: Form):Int = 
      (f1, f2) match 
        case (x, y) if flaeche(x) == flaeche(y) => 0
        case (x, y) if flaeche(x) < flaeche(y) => -1
        case _ => 1

@main def run(): Unit =
  import Form.*
  import Form.given_Ordering_Form.mkOrderingOps
  println(flaeche(Quadrat(2)))
  println(flaeche(Dreieck(1, 1, 1)))
  println(flaeche(Parallelogram(3, 4, 3)))
  println(umfang(Quadrat(2)))
  println(umfang(Dreieck(1, 1, 1)))
  println(umfang(Parallelogram(3, 4, 3)))  
  println(Dreieck(1, 1, 1) <= Parallelogram(3, 4, 3))
  println(Dreieck(1, 1, 1) >= Parallelogram(3, 4, 3))
  println(Quadrat(3) <= Quadrat(3))
  println(Quadrat(3) >= Quadrat(3))
  println(Rechteck(2, 4) < Parallelogram(2, 4, 5))
  println(Kreis(1) > Kreis(3))
  println(Dreieck(2, 2, 3) < Quadrat(3))
  import BBaum.*
  println(size(Knoten(10,Knoten(5,Knoten(4,Nix,Nix),Knoten(7,Knoten(6,Nix,Nix),Knoten(8,Nix,Nix))),Knoten(30,Knoten(20,Nix,Knoten(22,Nix,Nix)),Nix))))
  println(size(Knoten(12,Knoten(42,Knoten(6,Knoten(2,Nix, Nix),Nix),Knoten(99,Knoten(23, Nix,Nix), Knoten(36, Nix,Nix))),Knoten(14,Knoten(3,Nix,Knoten(7, Nix, Nix)),Nix))))
  println(inorder(Knoten(10,Knoten(5,Knoten(4,Nix,Nix),Knoten(7,Knoten(6,Nix,Nix),Knoten(8,Nix,Nix))),Knoten(30,Knoten(20,Nix,Knoten(22,Nix,Nix)),Nix))))
  println(inorder(Knoten(12,Knoten(42,Knoten(6,Knoten(2,Nix, Nix),Nix),Knoten(99,Knoten(23, Nix,Nix), Knoten(36, Nix,Nix))),Knoten(14,Knoten(3,Nix,Knoten(7, Nix, Nix)),Nix))))



/* Aufgabe 2
b)
*/
enum BBaum[+T]:
    case Nix
    case Knoten(value:T,lKind:BBaum[T],rKind:BBaum[T])
/*
Signatur: BBaum[T] => Int
Spezifikation:
Voraussetzung:
Effekt: keiner
Ergebnis: Anzahl der Knoten
*/
def size [T](baum:BBaum[T]):Int =
    import BBaum.*
    baum match
    case Nix => 0
    case Knoten(value,lKind,rKind) => 1 + size(lKind) + size (rKind)

/*
c)
Signatur: BBaum[T] => List[T]
Spezifikation:
Voraussetzung:
Effekt: keiner
Ergebnis: Liste mit Elementen des BBaumes in Reihenfolge der Inorder-Traversierung
*/
def inorder [T](baum:BBaum[T]):List[T] =
    import BBaum.*
    baum match
    case Nix => Nil
    case Knoten(value, lKind, rKind) => inorder(lKind):::(value::inorder(rKind))
