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
  println(flaeche(Dreieck(1, 1, 1)))
  println(umfang(Parallelogram(3, 4, 3)))
  println(Dreieck(1, 1, 1) == Parallelogram(3, 4, 3))