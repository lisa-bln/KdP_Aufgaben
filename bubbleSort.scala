/* 
pre:
eff:
erg:

*/

def bubbleSort(list: List[Int]): List[Int] = {
    def bubble(xs: List[Int]): List[Int]= xs match{
        case x :: y :: rest if x > y => y :: bubble(x :: rest)
        case x :: rest => x :: bubble(rest)
        case Nil => Nil
    } 
    val next = bubble(list)
    if(next == list) list else bubbleSort(next)
}

@main def tests(): Unit = {
    println(bubbleSort(List(1,2,3)))
    println(bubbleSort((Nil)))
    println(bubbleSort(List(1)))
    println(bubbleSort(List(3,2,1)))
    println(bubbleSort(List(1,2,2,3)))
}

/* 
Laufzeitanalyse:
    1) Wir betrachten zunächst die hilfsfunktion:
        sie nutzt Pattern Matching, bei jedem rekursiven aufruf wird die Liste um einen schritt abgearbeitet
        -> wenn die Liste n Elemente hat, braucht sie n Schritte -> O(n)
    2) Äußere Funktion:
        zwei Listen mit n Elementen werden verglichen, da jedes Paar verglichen werden muss braucnt man n Durchläufe -> O(n)
    
    3)Ergebnis:
        Anzahl der Durchläufe x Anzahl der Kosten pro Durchlauf
        = n x n
        = n^2
    Dieser BubbleSort hat eine quadratische Laufzeit 
 */