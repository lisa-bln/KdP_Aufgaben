// a) rekursive Funktion

/* 
Voraussetzung: Liste von Listen von Integern als Eingabe. 
Effekt: - 
Ergebnis: Eine Liste, in welcher die Sublisten der Eingabe zusammengefügt wurden.
*/

def flattenRek(nestedList: List[List[Int]]): List[Int] =
  nestedList match
    case Nil => Nil
    case liste :: listen => liste ::: flattenRek(listen)

//b endrekusiv
/* 
Voraussetzung: Liste von Listen von Integern als Eingabe. 
Effekt: - 
Ergebnis: Eine Liste, in welcher die Sublisten der Eingabe in eine Liste zusammengefügt wurden.
*/

def flattenERek(nestedList: List[List[Int]]): List[Int] =
  def helper(nestedList: List[List[Int]], flatlist: List[Int]): List[Int] =
    nestedList match
      case Nil => flatlist
      case liste :: listen => helper(listen, flatlist ::: liste)
  helper(nestedList, Nil)

@main def tests(): Unit =
  println(flattenRek(List(List(0,1), List(2,3), List(4,5), List(6,5))))
  println(flattenRek(List(List(), List(), List(), List())))
  println(flattenRek(List(List(0,1,3,4,5,6,7,8,9,10), List(2,3,30,31,32,33,34,35), List(1,2,3,4,5,6,7,8,9,10,11,12))))
  println(flattenERek(List(List(0,1), List(2,3), List(4,5), List(6,5))))
  println(flattenERek(List(List(), List(), List(), List())))
  println(flattenERek(List(List(0,1,3,4,5,6,7,8,9,10), List(2,3,30,31,32,33,34,35), List(1,2,3,4,5,6,7,8,9,10,11,12))))