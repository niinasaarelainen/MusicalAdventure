package o1.adventure

import scala.util.Random


/** The class `Adventure` represents text adventure games. An adventure consists of a player and 
  * a number of areas that make up the game world. It provides methods for playing the game one
  * turn at a time and for checking the state of the game.
  *
  *  */
class MusicAdventure {

  /** The title of the adventure game. */                 
  val title = "Seikkailu Musiikkitalossa"
    
  private val aula      = new Area("Musiikkitalon aula", "\nKultturellia ihmisvilinää.")
  private val anniskelu = new Area("Anniskelualue", "Komennolla 'ota' voit saada ilmaiseksi ihan mitä haluat, esim 'ota tynnyri olutta'.")
  private val sali1 = new Area("Konserttisali 1", "Konsertti alkaa klo 19:00")
  private val wc    = new Area("WC", "Lienet käynyt tällaisessa paikassa ennenkin.")
  private val naulakko      = new Area("Naulakko", "Eteispalvelumaksu 3 euroa.")
  private val sali2        = new Area("Konserttisali 2", "Täällä ei ole tänään konserttia. Nyt on meneillään sound-check huomista konserttia varten.")
  private val sali3        = new Area("Konserttisali 3", "Konsertti meneillään. Taisit mennä pummilla väärään saliin...")
  private var alert = false 
  private var alertFail =  false
         
//      aula.setNeighbors(Vector("w" -> anniskelu, "s" -> naulakko, "z" -> sali2,  "a" -> wc       ))
// anniskelu.setNeighbors(Vector( "w" ->     sali1,"s" -> sali2,    "z" -> aula                       ))
//  sali1.setNeighbors(Vector(                                             "z" -> anniskelu                  ))
//     wc.setNeighbors(Vector(                         "s" -> aula                                       ))
//  naulakko.setNeighbors(Vector("w" -> sali2 ,                                           "a" -> aula     ))
//     sali2.setNeighbors(Vector(                                          "z"-> naulakko,"a"-> anniskelu ))
//     sali3.setNeighbors(Vector("w" -> aula                                                                 ))
     
      aula.setNeighbors(Vector("w" -> anniskelu, "s" -> naulakko, "z" -> sali3,  "a" -> wc       ))
 anniskelu.setNeighbors(Vector( "w" ->     sali1,"s" -> sali2,    "z" -> aula                       ))
  sali1.setNeighbors(Vector(                                             "z" -> anniskelu                  ))
     wc.setNeighbors(Vector(                         "s" -> aula                                       ))
  naulakko.setNeighbors(Vector("w" -> sali2 ,                                           "a" -> aula     ))
     sali2.setNeighbors(Vector(                                          "z"-> naulakko,"a"-> anniskelu ))
     sali3.setNeighbors(Vector("w" -> aula                                                                 ))

  
  private val aanitysLaite = new Item("äänityslaite")   
  private val kuulokkeet = new Item("kuulokkeet")
  private var viisiMinCountDown = 5
  
  val rand = new Random
  var randomAlue = new Area("rand", "rand")
  
 def arvonta(montakoHuonetta:Int) = {
    var randomLuku = rand.nextInt(montakoHuonetta)
    if(randomLuku==0)
      randomAlue = wc
    else if(randomLuku==1)
      randomAlue = anniskelu
    else if(randomLuku==2)
      randomAlue = sali1
    else if(randomLuku==3)
      randomAlue = naulakko
    else if(randomLuku==4)
      randomAlue = sali2
    else randomAlue = sali3
    randomAlue  
  }  
  arvonta(6).addItem(aanitysLaite)
 // println("äänityslaite on randomisti: " + randomAlue.name)
  arvonta(6).addItem(kuulokkeet)
//  println("kuulokkeet on randomisti: " + randomAlue.name)
  println("\n*************** SEIKKAILU MUSIIKKITALOSSA ********************************\n")
  
  val player = new Player(aula)
  val eiSamassaHuoneessaKahtaVuoroa = new Player(wc)
  
  
  var turnCount = 0
  /** The maximum number of turns that this adventure game allows before time runs out. */
  val timeLimit = 30 


  /** Determines if the adventure is complete, that is, if the player has won. */
  // ongelma: painaa rec ennenkuin käy vessassa eikä palaa konserttisaliin enää
  def isComplete = {
    this.player.has("äänityslaite") && this.player.has("kuulokkeet") && this.player.lipukkeita == 0  && this.player.onnistuikoWcKaynti && this.player.aanitys 
    }

  /** Determines whether the player has won, lost, or quit, thereby ending the game. */ 
  def isOver = this.isComplete || this.player.hasQuit || this.turnCount == this.timeLimit || alertFail

  /** Returns a message that is to be displayed to the player at the beginning of the game. */
  def welcomeMessage = "Olet Musiikkitalon aulassa. Tohelo vahtimestari on jättänyt äänityslaitteen ja kuulokkeet\n"+
  "ties minne. Etsi ne ja mene ENNEN klo 19:00 saliin tekemään äänitys. Äänitys alkaa kun kirjoitat rec\n\nÄänityspalkkiosi on 2 ilmaista anniskelualueen tuotetta. Käytä lipukkeet pian\nja muista jättää aikaa myös pakolliseen vessassakäyntiin ennen kello seitsemää !" +
  "\n\nEikä tässä vielä kaikki. Samassa talossa vaeltelee henkilö, jolle olet velkaa. \nEt voi olla kuin yhden vuoron samassa huoneessa kuin hän, muutoin hän huomaisi sinut." +
  "\n\nKirjoita help (ja ENTER) jos kaipaat lisäohjeita"

    
  /** Returns a message that is to be displayed to the player at the end of the game. The message 
    * will be different depending on whether or not the player has completed their quest. */
  def goodbyeMessage = {
    if (this.isComplete)
      "Mahtavuutta!!!  Äänitys lähti käyntiin ajoissa."
    else if (this.turnCount == this.timeLimit)
      "Et saanut äänitettyä ajoissa. Peli loppui tähän."
  }

  
  def playTurn(command: String) = {          
    
    val action = new Action(command)
    if(this.player.lipukkeita == 0)
      viisiMinCountDown -= 1
    val outcomeReport = action.execute(this.player, viisiMinCountDown)   // Option[String]
    if (outcomeReport.isDefined) { 
      this.turnCount += 1 
    }
    if(this.player.location == eiSamassaHuoneessaKahtaVuoroa.location){
   //   println("player: " + this.player.location + eiSamassaHuoneessaKahtaVuoroa.location)
      if(alert == false){
        println("ALERT !!!!!!!! Tyyppi, jolle olet velkaa on samassa huoneessa. Pakene heti tai peli loppuu.")
        alert = true
      }
        else {
          println("Olit liian kauan samassa tilassa kuin velkoja.")
          alertFail= true
        }
    } 
    else   alert = false
    
    if (alert == false){    // tyyppi jää paikoilleen, jos se on samassa huoneessa kuin pelaaja
       val satunnainenSuunta = rand.nextInt(2)   // laitetaan karttotyyppi liikkumaan joko last tai head -suuntaan naapureita
       if (satunnainenSuunta == 0)
          eiSamassaHuoneessaKahtaVuoroa.go(eiSamassaHuoneessaKahtaVuoroa.location.neighbors.keys.last)     
       else if (satunnainenSuunta == 1)
         eiSamassaHuoneessaKahtaVuoroa.go(eiSamassaHuoneessaKahtaVuoroa.location.neighbors.keys.head)
    }   
    outcomeReport.getOrElse("Väärä komento. Kirjoittamalla help saat apua.")
  }
  
  
}

