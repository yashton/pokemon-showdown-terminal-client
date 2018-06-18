package science.snelgrove.showdown

import akka.actor.Actor
import akka.event.Logging
import scala.collection.mutable.{ Buffer, SortedSet, TreeSet }
import science.snelgrove.showdown.protocol._
import scala.collection.mutable.ListBuffer

case class UserListUpdate(users: Seq[String])

/**
  * Handles the user list of a room.
  */
class UserList extends Actor {
  val log = Logging(context.system, this)

  val userList: SortedSet[String] = new TreeSet()
  def receive = {
    case Join(user) =>
      userList += user.name
      context.parent ! UserListUpdate(userList.toIndexedSeq)
    case Leave(user) =>
      userList -= user.name
      context.parent ! UserListUpdate(userList.toIndexedSeq)
    case RoomUsers(current) =>
      userList.clear()
      userList ++= (current.map(_.name))
      context.parent ! UserListUpdate(userList.toIndexedSeq)
    case Name(user, old) =>
      userList -= old
      userList += user.name
      context.parent ! UserListUpdate(userList.toIndexedSeq)
  }
}
