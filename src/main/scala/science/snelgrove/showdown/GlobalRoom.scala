package science.snelgrove.showdown

import akka.actor.{ Actor, Props }
import akka.event.Logging
import scala.collection.mutable.{ Buffer, SortedSet, TreeSet }
import science.snelgrove.showdown.protocol._
import scala.collection.mutable.ListBuffer
/**
  * Handles the primary chat behavior of the room.
  */
class GlobalRoom extends Actor {
  val log = Logging(context.system, this)
  val chat = context.actorOf(Props[ChatBuffer], s"${self.path.name}-chat")
  val users = context.actorOf(Props[UserList], s"${self.path.name}-user")


  val messageBuffer: Buffer[GlobalMessage] = new ListBuffer()
  var current = GlobalState(Seq(), Seq(), Seq())
  def receive = {
    case c: ChatMessage => chat ! c
    case m: GlobalMessage =>
      messageBuffer += m
      current = current.copy(global = messageBuffer.toIndexedSeq)
      context.parent ! current
    case UserListUpdate(u) =>
      current = current.copy(users = u)
      context.parent ! current
    case ChatUpdate(u) =>
      current = current.copy(chat = u)
      context.parent ! current
  }
}
