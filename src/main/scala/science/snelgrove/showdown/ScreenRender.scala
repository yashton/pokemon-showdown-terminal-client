package science.snelgrove.showdown

import akka.actor.{ Actor, ActorRef, ActorSystem, Cancellable }
import akka.event.Logging
import com.googlecode.lanterna.TerminalPosition
import com.googlecode.lanterna.screen.Screen

/**
  * Manages rendering of game state
  */
class ScreenRender(val screen: Screen) extends Actor {
  val log = Logging(context.system, this)
  //TODO on shutdown stop screen
  screen.startScreen()

  def receive = {
    case ChatTypingUpdate(text) =>
      screen.clear
      val graphics = screen.newTextGraphics()
      graphics.putString(0, 0, text)
      screen.setCursorPosition(new TerminalPosition(text.size, 0))
      screen.refresh()
    case msg => log.info(msg.toString())
  }
}
