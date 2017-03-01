package o1.adventure


/** The class `Action` represents actions that a player may take in a text adventure game.
  * `Action` objects are constructed on the basis of textual commands and are, in effect, 
  * parsers for such commands. An action object is immutable after creation.
  * @param input  a textual in-game command such as "go east" or "rest" */
class Action(input: String) {

  private val commandText = input.trim.toLowerCase
  private val verb        = commandText.takeWhile( _ != ' ' )
  private val modifiers   = commandText.drop(verb.length).trim
   
  
  /** Causes the given player to take the action represented by this object, assuming 
    * that the command was understood. Returns a description of what happened as a result 
    * of the action (such as "You go west."). The description is returned in an `Option` 
    * wrapper; if the command was not recognized, `None` is returned. */
  def execute(actor: Player, viisiMinCountDown: Int): Option[String] = {  
   
    
    if (this.verb == "w" || this.verb == "s" || this.verb == "z" || this.verb == "a") {
       Some(actor.go(this.verb, viisiMinCountDown))
    } 
    else if ( this.verb == "inventaario") {
       Some(actor.inventaario)
    }
    else if ( this.verb == "nouki") {
        Some(actor.nouki(this.modifiers))
    }
    else if ( this.verb == "ota") {
      Some(actor.ota(this.modifiers))
    }
     else if ( this.verb == "rec") {
       Some(actor.rec)                                        
    } 
     else if ( this.verb == "help") {
       Some(actor.ohjeet)                                        
    } 
    else if (this.verb == "quit") {
       Some(actor.quit())
    } else {
       None
    }
    
  }


  /** Returns a textual description of the action object, for debugging purposes. */
  override def toString = this.verb + " (modifiers: " + this.modifiers + ")"  

  
}

