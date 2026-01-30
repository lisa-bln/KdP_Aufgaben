/*
Blatt 12
Matthis, Lisa, Thu An
*/

//Aufgabe 1

/* 
Vor: versuche ist eine positive ganze Zahl 
Eff: es werden Variablen und Values erzeugt
Erg: Tupel welches die Anzahl der Versuche, die Approximation duch Floats, die Approximation durch Doubles und den Fehler der Approximationen angibt
*/

import scala.math._
import java.util.Random
val r: Random = new Random()

def piApprox(versuche: Int): Tuple = 
  var erfolgreichF: Int = 0
  var erfolgreichD: Int = 0
  for i <- 0 to versuche-1 do 
    val xF: Float = r.nextFloat
    val yF: Float = r.nextFloat
    val xD: Double = r.nextDouble
    val yD: Double = r.nextDouble
    if ( xF*xF + yF*yF ) <= 1 then 
      erfolgreichF += 1    
    if ( xD*xD + yD*yD ) <= 1 then 
      erfolgreichD += 1
  (s"$versuche Versuche, Approx. Floats: ", erfolgreichF.toFloat * 4 / versuche, " Approx. Doubles: ", erfolgreichD.toDouble* 4 / versuche, 
  "   Fehler: ", ( (erfolgreichF.toFloat * 4 / versuche - scala.math.Pi).abs, (erfolgreichD.toDouble * 4 / versuche - scala.math.Pi).abs))


//Aufgabe 2

object Obi_Wan_Kenobi:
    private val master:String = "Qui-Gon Jinn"
    private val padawan:String = "Anakin Skywalker"
    private val Spezies:String = "Mensch"
    private val Geburtsdatum:Int = 57 //BBY
    private var Lieblingsraumschiff:String = "T-6-Shuttle"
    private var aktuelleWaffe:String = "Lichtschwert"
    private val großesGeheimnis:String = "findet Yoda blöd"

    //getter
    // Spezifikation:
    // Voraussetzung: keine
    // Effekt: keiner
    // Ergebnis: Rückgabe des Wertes des spezifizierten Objektattributs
    def getmaster:String = master
    def getpadawan:String = padawan
    def getSpezies:String = Spezies
    def getGeburtsdatum:Int = Geburtsdatum
    def getLieblingsraumschiff:String = Lieblingsraumschiff
    def getaktuelleWaffe:String = aktuelleWaffe

    //setter
    def setLieblingsraumschiff (newLieblingsraumschiff:String):Unit =
    Lieblingsraumschiff = newLieblingsraumschiff
    def setaktuelleWaffe (newaktuelleWaffe:String):Unit =
    aktuelleWaffe = newaktuelleWaffe
/*
c)
Beim Geheimnisprinzip werden Daten und Infos vom Zugriff auf außen mit dem Stichwort private verborgen. Der direkte Zugriff ist nicht möglich, nur an klar definierten Schnittstellen. So kann man genau festlegen, auf welche Infos man von außen zugreifen kann und welche man ändern kann. Hier können die Infos master, padawan, Spezies, Geburtsdatum, Lieblingsraumschiff und aktuelleWaffe ausgegeben werden. Verändert werden kann das Lieblingsraumschiff und die aktuelleWaffe. Das großesGeheimnis kann nicht ausgegeben werden und es kann auch nicht verändert werden.
Das ist sinnvoll um die Datenintegrität zu erhalten und Implementierungsdetails können geheim gehalten werden.
*/
    //Voraussetzung: keine
    //Effekt: keiner
    //Ergebnis: Obi Wan Kenobi passt seine Verteidigung an die Angreifer an: bei einem sith lord guckt er böse und rettet die Welt, bei einem Kopfgeldjäger lächelt er lässig und versaut ihnen den Job, bei general grieves sagt er hello there und bei allen anderen nutzt er chillig die Macht
    def verteidigung(angreifer:String):String =
        angreifer.toLowerCase match
        case "sith lord" => "Obi Wan Kenobi guckt böse und rettet die Welt"
        case "kopfgeldjäger" => "Obi Wan Kenobi lächelt lässig und versaut den Job"
        case "general grieves" => "hello there :)"
        case _ => "Obi Wan Kenobi nutzt chillig die Macht"


@main def run(): Unit = 
//Zu Aufgabe 1
    println(piApprox(100))
    var i: Int = 1
    while i < 10000000 do 
      var fehlerF: Double = 0
      var fehlerD: Double = 0
      for j <- 1 to 5 do 
        println(piApprox(i))
      i = i*10
      println("-----")
//Zu Aufgabe 2
    println(Obi_Wan_Kenobi.getmaster)
    println(Obi_Wan_Kenobi.getpadawan)
    println(Obi_Wan_Kenobi.getSpezies)
    println(Obi_Wan_Kenobi.getGeburtsdatum)
    println(Obi_Wan_Kenobi.getLieblingsraumschiff)
    println(Obi_Wan_Kenobi.getaktuelleWaffe)
    Obi_Wan_Kenobi.setLieblingsraumschiff("Delta-7-Sternjäger")
    println(Obi_Wan_Kenobi.getLieblingsraumschiff)
    Obi_Wan_Kenobi.setaktuelleWaffe("pink Blaster")
    println(Obi_Wan_Kenobi.getaktuelleWaffe)
    println(Obi_Wan_Kenobi.verteidigung("Sith Lord"))
    println(Obi_Wan_Kenobi.verteidigung("Kopfgeldjäger"))
    println(Obi_Wan_Kenobi.verteidigung("general grieves"))
    println(Obi_Wan_Kenobi.verteidigung("böser Kampfdruide"))


