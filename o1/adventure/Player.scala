package o1.adventure

import scala.collection.mutable.Map
import o1.sound._
//import o1.util._
import o1.util.tryForURL
import util.control.Breaks._
import sun.util.logging.PlatformLogger._
import javax.sound.sampled._

  
/** A `Player` object represents a player character controlled by the real-life user of the program. 
  *
  * A player object's state is mutable: the player's location and possessions can change, for instance.
  *
  * @param startingArea  the initial location of the player */
class Player(startingArea: Area) {

  private var currentLocation = startingArea        // gatherer: changes in relation to the previous location
  private var quitCommandGiven = false              // one-way flag
  private var allItems    = Map[String, Item]() 
  var lipukkeita = 2
  var onnistuikoWcKaynti = false
  var aanitys = false

  val audioInput2 = AudioSystem.getAudioInputStream(tryForURL("ambient_loop.wav").get)   // Konserttisali 2:n musa
  val soundToPlay2 = AudioSystem.getClip()
  soundToPlay2.open(audioInput2)
  val audioInput3 = AudioSystem.getAudioInputStream(tryForURL("flamingo_loop.wav").get)   // Konserttisali 3:n musa
  val soundToPlay3 = AudioSystem.getClip()
  soundToPlay3.open(audioInput3)

   //   AudioSystem.getMixer(x$1)                        // mixer !!!!!!!!!!
  
  def ota(itemName: String): String  = {              
     if (location.name != "Anniskelualue")
        "etsi Anniskelualue."
     else if( lipukkeita > 0 && location.name == "Anniskelualue"){
        lipukkeita -= 1
        playRecording("jungle.wav")    
        itemName + " tuli nautittua. Sinulla on vielä " + lipukkeita + " lipuke(tta)."
     }       
     else "Olet jo käyttänyt äänityspalkkiosi. Ja rahaahan ei äänimiehillä ikinä ole."    
  }
  
  def rec = {
     if(allItems.size == 2 && location.name == "Konserttisali 1" && onnistuikoWcKaynti ){
       aanitys = true
       "Aloitat äänityksen."  
     }  
     else if( allItems.size == 1)
        "Sinulta puuttuu kuulokkeet soundcheckin tekoon tai äänityslaite."
     else if( allItems.size == 0)
        "Sinulta puuttuu sekä kuulokkeet että äänityslaite"     
     else if(lipukkeita>0)
         "Ennen äänitystä sinun piti käyttää lipukkeet Anniskelualueella."  
     else if(!onnistuikoWcKaynti)
          "Et voi mennä kahden tunnin konserttiin käymättä ensin vessassa."    
     else "Äänitys tulee tehdä Konserttisali 1:ssä."  
  }
  
  def nouki(itemName: String): String  = {
     if(location.contains(itemName)){
       allItems += itemName -> location.removeItem(itemName).get
       playRecording("jungle.wav")     // soundToPlay.loop ei toimi, tai ehkä pitäisi stopata mutta voisi olla hankala kun ei tiedä missä
       "Noukkimasi esine: " + itemName + "."   
      }  
      else itemName + " ei löydy täältä" 
  }
  
  def has(itemName: String): Boolean  = allItems.contains(itemName)
  
  def ohjeet = {
  "Olet Musiikkitalon aulassa. Tohelo vahtimestari on jättänyt äänityslaitteen ja kuulokkeet\n"+
  "ties minne. Etsi ne ja mene ENNEN klo 19:00 oikeaan saliin tekemään äänitys. Saleja on kolme, \nsinun tulee päätellä mikä on oikea sali. Äänitys alkaa kun kirjoitat rec\n\nÄänityspalkkiosi on 2 ilmaista anniskelualueen tuotetta. Käytä lipukkeet pian\nja muista jättää aikaa myös pakolliseen vessassakäyntiin ennen kello seitsemää !" +
  "\n\nEikä tässä vielä kaikki. Samassa talossa vaeltelee henkilö, jolle olet velkaa. \nEt voi olla kuin yhden vuoron samassa huoneessa kuin hän, muutoin hän huomaisi sinut.\nRuudulla näkyy varoitusteksti, kun hän on samassa tilassa kuin sinä." +
  "\n\n"+
  "Eli sinulla on kolme ensisijaista tehtävää:\n"+
  " 1) etsi kuulokkeet\n"+
  " 2) etsi äänityslaite\n"+
  " 3) käytä molemmat lipukkeet\n\n"+  
   "\nota = otetaan baarista ilmaiseksi jokin tuote, esim. ota sherry (saat tilata mitä vain)\n" +
   "nouki = tällä käskyllä kerätään tavarat, jotka tarvitset: äänityslaite ja kuulokkeet, esim. nouki kuulokkeet\n" +
   "rec = äänitys alkaa, jos olet ajoissa oikeassa konserttisalissa ja olet noukkinut molemmat äänitykseen tarvittavat tavarat\n" +
   "inventaario = näet mitä esineitä sinulla on\n"+
   "help= sait esille juuri nämä ohjeet"+
   "\n\n             LIIKKUMINEN:\n"+
   "\t\tw = ^\n"+
   "\t    a = <     s = >\n"+
   "\t\tz = `´"  
   
  }
  
  def inventaario: String  = {
      if(allItems.size>0 )
        "Sinulla on:\n" + this.allItems.keys.mkString("\n")
      else "Et ole vielä noukkinut mitään tavaroita"
  }
  
  /** Determines if the player has indicated a desire to quit the game. */
  def hasQuit = this.quitCommandGiven

  
  /** Returns the current location of the player. */
  def location = this.currentLocation
  
def go(direction: String) = {    // minuuttiparametriton go on vain toisen henkilön käytössä
    val destination = this.location.neighbor(direction)
    this.currentLocation = destination.getOrElse(this.currentLocation) 
  }
  
  /** Attempts to move the player in the given direction. This is successful if there 
    * is an exit from the player's current location towards the direction name. 
    * Returns a description of the results of the attempt. */
  def go(direction: String, viisiMinCountDown: Int) = {
    val destination = this.location.neighbor(direction)
    this.currentLocation = destination.getOrElse(this.currentLocation) 
    breakable {
      if(location.name == "WC" && viisiMinCountDown< 5){
         println("Ehdit ajoissa vessaan!")
         onnistuikoWcKaynti = true
         break
      }  
       if(lipukkeita == 0){
          if (viisiMinCountDown > 0 && !onnistuikoWcKaynti){
            println("Nyt sinun pitää päästä " + viisiMinCountDown + " minuutin sisällä vessaan.")
          }    
          else if(!onnistuikoWcKaynti) {
            println("Mission fail !!!" )  
            val action = new Action("quit")
            val outcomeReport = action.execute(this, viisiMinCountDown)
          }
      }   
    }  // end breakable
    
 //   if (location.name == "Konserttisali 1")        
     //  
    if (location.name == "Konserttisali 2")         
       soundToPlay2.loop(10)
    else if (location.name == "Konserttisali 3")        
       soundToPlay3.loop(10)
    else {
      soundToPlay2.stop()
      soundToPlay3.stop()
  //    soundToPlay.close()              pitäisikö sulkea exitisää ?!?
    }
    if (destination.isDefined) "" else  direction + " ei ole nyt mahdollinen."
  }

    
  def quit() = {
    this.quitCommandGiven = true
    ""
  }

  
  /** Returns a brief description of the player's state, for debugging purposes. */
  override def toString = "Now at: " + this.location.name   


}


