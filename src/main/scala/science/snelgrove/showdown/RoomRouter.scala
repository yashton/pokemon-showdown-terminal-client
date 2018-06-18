package science.snelgrove.showdown

import akka.actor.{ Actor, ActorRef, ActorSystem, Cancellable, Props }
import scala.collection.mutable
import science.snelgrove.showdown.protocol._
import akka.event.Logging

sealed trait RoomId
case class RoomName(name: String) extends RoomId
case object GlobalName extends RoomId

class RoomRouter(val output: ActorRef) extends Actor {
  val log = Logging(context.system, this)

  val rooms: mutable.Map[RoomId, ActorRef] =
    mutable.Map(GlobalName -> context.system.actorOf(
      Props(classOf[RoomActor], "global", output), "room-global"))

  val global = rooms(GlobalName)
  global ! RoomInit(GlobalType)

  def receive = {
    // TODO making the distinction from global messages and the lobby
    case x @ Global(msgs) =>
      log.debug(s"global ${msgs.size}")
      for (msg <- msgs) global ! msg
    case x @ Room("global", msgs) =>
      log.debug(s"global ${msgs.size}")
      for (msg <- msgs) global ! msg
    case x @ Room(name, msgs) =>
      log.debug(s"$name ${msgs.size}")
      val id = RoomName(name)
      if (!(rooms contains id)) {
        rooms += (id -> context.actorOf(
          Props(classOf[RoomActor], name, output), s"room-$name"))
        log.info(s"Creating room $name")
      }
      val room = rooms(id)
      for (msg <- msgs) room ! msg
    case x =>
      log.warning(x.toString)
  }
}
