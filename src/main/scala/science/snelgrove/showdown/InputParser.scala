package science.snelgrove.showdown

import akka.actor.{ Actor, ActorRef, ActorSystem, Cancellable }
import com.googlecode.lanterna.input.KeyType
import science.snelgrove.showdown.protocol.TextCommand
import akka.event.Logging

case class ChatTypingUpdate(text: String)
class InputParser(val tempoutput: ActorRef, val screen: ActorRef) extends Actor {
  val log = Logging(context.system, this)

  var buffer = new StringBuilder()

  def receive = {
    case x @ KeyCharacter(Left(c), _, _, _) =>
      buffer += c
      screen ! ChatTypingUpdate(buffer.toString())
    case x @ KeyCharacter(Right(KeyType.Backspace), _, _, _) =>
      buffer = buffer.dropRight(1)
      screen ! ChatTypingUpdate(buffer.toString())
    case x @ KeyCharacter(Right(KeyType.Enter), _, _, _) =>
      tempoutput ! TextCommand(buffer.toString())
      buffer.clear()
      screen ! ChatTypingUpdate(buffer.toString())
    case x =>
      log.warning(x.toString)
  }
}
