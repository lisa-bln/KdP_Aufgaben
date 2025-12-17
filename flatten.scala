// a) rekursive Funktion

/* 
Voraussetzung: Liste von Listen von Integern als Eingabe. 
Effekt: - 
Ergebnis: Eine Liste, in welcher die Sublisten der Eingabe zusammengefügt wurden
*/

def flattenRek(nestedList: List[List[Int]]): List[Int] =
  nestedList match
    case Nil => Nil
    case liste :: listen => liste ::: flattenRek(listen)

def flattenERek(nestedList: List[List[Int]]): List[Int] =
  def helper(nestedList: List[List[Int]], flatlist: List[Int]): List[Int] =
    nestedList match
      case Nil => flatlist
      case liste :: listen => helper(listen, flatlist ::: liste)
  helper(nestedList, Nil)

@main def tests(): Unit =
  println(flattenRek(List(List(0,1), List(2,3), List(4,5), List(6,5))))
  println(flattenERek(List(List(0,1), List(2,3), List(4,5), List(6,5))))
