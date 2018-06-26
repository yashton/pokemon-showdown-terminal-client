package science.snelgrove.showdown

import akka.actor.{ Actor, ActorRef, ActorSystem, Cancellable }
import com.googlecode.lanterna.input.KeyType
import science.snelgrove.showdown.protocol.TextCommand
import akka.event.Logging

case class RoomSwitch(i: Int)
case class ChatTypingUpdate(text: String)

class InputParser(val screen: ActorRef) extends Actor {
  val log = Logging(context.system, this)

  var buffer = new StringBuilder()

  def receive = {
    case x @ KeyCharacter(Left(c), false, true, false)
        if ('0' to '9').contains(c) =>
      screen ! RoomSwitch(c - '0')
    case x @ KeyCharacter(Left(c), false, false, _) =>
      buffer += c
      screen ! ChatTypingUpdate(buffer.toString())
    case x @ KeyCharacter(Right(KeyType.Backspace), _, _, _) =>
      buffer = buffer.dropRight(1)
      screen ! ChatTypingUpdate(buffer.toString())
    case x @ KeyCharacter(Right(KeyType.Enter), _, _, _) =>
      screen ! TextCommand(buffer.toString())
      buffer.clear()
      screen ! ChatTypingUpdate("")
    case x =>
      log.warning(x.toString)
  }
}
