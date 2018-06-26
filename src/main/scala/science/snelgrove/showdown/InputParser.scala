package science.snelgrove.showdown

import akka.actor.{ Actor, ActorRef, ActorSystem, Cancellable }
import com.googlecode.lanterna.input.{ KeyStroke, KeyType }
import science.snelgrove.showdown.protocol.TextCommand
import akka.event.Logging

case class RoomSwitch(i: Int)
case class ChatTypingUpdate(text: String)

case class KeyCharacter(key: Either[Char, KeyType], ctrl: Boolean, alt: Boolean, shift: Boolean)
object Keyboard {
  implicit def keystrokeConvert(key: KeyStroke): KeyCharacter = {
    val char = key.getKeyType match {
      case KeyType.Character => Left(key.getCharacter: Char)
      case _ => Right(key.getKeyType)
    }
    KeyCharacter(char, key.isCtrlDown(), key.isAltDown(), key.isShiftDown())
  }
}


class InputParser() extends Actor {
  val log = Logging(context.system, this)

  var buffer = new StringBuilder()

  def receive = {
    case x @ KeyCharacter(Left(c), false, true, false)
        if ('0' to '9').contains(c) =>
      context.parent ! RoomSwitch(c - '0')
    case x @ KeyCharacter(Left(c), false, false, _) =>
      buffer += c
      context.parent ! ChatTypingUpdate(buffer.toString())
    case x @ KeyCharacter(Right(KeyType.Backspace), _, _, _) =>
      buffer = buffer.dropRight(1)
      context.parent ! ChatTypingUpdate(buffer.toString())
    case x @ KeyCharacter(Right(KeyType.Enter), _, _, _) =>
      context.parent ! TextCommand(buffer.toString())
      buffer.clear()
      context.parent ! ChatTypingUpdate("")
    case x =>
      log.warning(x.toString)
  }
}
