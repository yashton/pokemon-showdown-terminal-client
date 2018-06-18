package science.snelgrove.showdown

import akka.actor.Actor
import akka.event.Logging
import scala.collection.mutable.{ Buffer, ListBuffer }
import science.snelgrove.showdown.protocol._

case class ChatUpdate(msgs: Seq[ChatMessage])

/**
  * Handles the primary chat behavior of the room.
  */
class ChatBuffer extends Actor {
  val log = Logging(context.system, this)

  val messageBuffer: Buffer[ChatMessage] = new ListBuffer()

  def receive = {
    case x: ChatMessage =>
      messageBuffer += x
      context.parent ! ChatUpdate(messageBuffer.toIndexedSeq)
  }
}
