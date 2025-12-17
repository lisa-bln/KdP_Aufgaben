//a) 
/*
Vor: Eingabeliste besteht aus Integern
Eff: -
Erg: Liste in der die Einträge der Eingabe verdoppelt wurden. 
*/

def doubleList(list: List[Int]): List[Int] = 
  list.map(_*2)


//b)
/*
Vor: Die Liste aus Charaktern besteht ausschließlich aus Buchstaben auf welche die Funktion _.isUpper angewendet werden kann.
Eff: - 
Erg: Eine Liste, die alle Kleinbuchstaben der Eingabe enthält.
*/

def withoutUpper(list: List[Char]): List[Char] =
  list match
  case Nil => Nil
  case x :: xs if (! x.isUpper) => x :: withoutUpper(xs)
  case x :: xs => withoutUpper(xs)

//c)
/*
Vor: l1 und l2 sind vom Typ List[Int] und f eine Funktion von (Int, Int) => Int, die auf den Eingabewerten definiert ist (z.B. / nur wenn der zweite Eingabeparameter Int\{0})
Eff: - 
Erg: Liste mit Wert f(l1(i), l2(i)) an Position i. Ist eine Liste länger wird der Rest der längeren Liste verworfen.
*/

def zipWith(f: (Int, Int) => Int, l1: List[Int], l2: List[Int]): List[Int] =
  (l1, l2) match
  case (x :: xs, y :: ys) => f(x, y) :: zipWith(f, xs, ys)
  case _ => Nil

@main def run(): Unit =
  println(doubleList(List(1, 2, 3)))
  println(doubleList(List(0)))
  println(doubleList(List()))
  println(doubleList(List(1, 2, 3, 12, 41, 43, 80, 3, 2, 5, 6, 7, 8)))

  println(withoutUpper(List('a', 'A', 'b', 'B')))
  println(withoutUpper(List()))
  println(withoutUpper(List('A', 'A', 'B', 'B')))
  println(withoutUpper(List('a', 'b', 'c', 'd')))

  println(zipWith((_+_), List(1, 2, 3), List(3, 2, 1, 4)))
  println(zipWith((_-_), List(1, 2, 3), List(3, 2, 1, 4)))
  println(zipWith((_*_), List(1, 2, 3), List(3, 2, 1, 4)))
  println(zipWith((_%_), List(1, 2, 3), List(3, 2, 1, 4)))
  println(zipWith((_/_), List(1, 2, 3), List(3, 2, 1, 4)))
  println(zipWith((_+_), List(), List()))
  println(zipWith((_-_), List(1, 2, 3, 4, 5, 6, 7, 8), List(3, 2, 1, 4)))
  println(zipWith((_*_), List(1, 2, 3), List()))
  println(zipWith((_%_), List(), List(3, 2, 1, 4)))
  println(zipWith((_/_), List(0, 0, 0), List(8, 2, 1, 4)))