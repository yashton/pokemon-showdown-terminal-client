package science.snelgrove.showdown

import akka.actor.{ Actor, Props }
import akka.event.Logging
import scala.collection.mutable.{ Buffer, SortedSet, TreeSet }
import science.snelgrove.showdown.protocol._
import scala.collection.mutable.ListBuffer
/**
  * Handles the primary chat behavior of the room.
  */
class BattleRoom extends Actor {
  val log = Logging(context.system, this)
  val chat = context.actorOf(Props[ChatBuffer], s"${self.path.name}-chat")
  val battle = context.actorOf(Props[BattleProcessor], s"${self.path.name}-battle")
  val users = context.actorOf(Props[UserList], s"${self.path.name}-user")

  var current = BattleState(Seq(), Seq(), Seq())
  def receive = {
    case c: ChatMessage => chat ! c
    case u: UsersMessage => users ! u
    case UserListUpdate(u) =>
      current = current.copy(users = u)
      context.parent ! current
    case ChatUpdate(u) =>
      current = current.copy(chat = u)
      context.parent ! current
    case BattleUpdate(u) =>
      current = current.copy(battle = u)
      context.parent ! current
  }
}
