package science.snelgrove.showdown

import akka.actor.Actor
import akka.event.Logging
import scala.collection.mutable.{ Buffer, SortedSet, TreeSet }
import science.snelgrove.showdown.protocol._
import scala.collection.mutable.ListBuffer
/**
  * Handles the primary chat behavior of the room.
  * Mixed into RoomActor.
  */
trait ChatRoom extends Actor {
  val log = Logging(context.system, this)

  val messageBuffer: Buffer[RoomMessage] = new ListBuffer()
  def chat: PartialFunction[Any, Unit] = {
    case x: RoomMessage => messageBuffer += x
    case RoomDeinit => context.stop(self)
    case x => log.info(x.toString)
  }
}

trait RoomUserList extends Actor {
  val userList: SortedSet[String] = new TreeSet()
  def users: PartialFunction[Any, Unit] = {
    case Join(user) =>
      userList += user.name
    case Leave(user) => userList -= user.name
    case RoomUsers(current) =>
      userList.clear()
      userList ++= (current.map(_.name))
    case Name(user, old) =>
      userList -= old
      userList += user.name
  }
}
