package science.snelgrove.showdown

import akka.actor.{ Actor, ActorRef, ActorSystem, Cancellable, Props }
import scala.collection.mutable
import science.snelgrove.showdown.protocol._
import akka.event.Logging

sealed trait RoomId
case class RoomName(name: String) extends RoomId
case object GlobalName extends RoomId

class RoomRouter() extends Actor {
  val log = Logging(context.system, this)

  val rooms: mutable.Map[RoomId, ActorRef] =
    mutable.Map(GlobalName -> context.system.actorOf(Props(classOf[RoomActor], "room-global")))

  val global = rooms(GlobalName)
  global ! RoomInit(ChatType)
  // todo handle room supervision

  def receive = {
    case x @ Global(msgs) =>
      log.info(x.toString)
      for (msg <- msgs) global ! msg
    case x @ Room("global", msgs) =>
      log.info(x.toString)
      for (msg <- msgs) global ! msg
    case x @ Room(name, msgs) =>
      log.info(x.toString)
      val id = RoomName(name)
      if (!(rooms contains id))
        rooms + (id -> context.system.actorOf(Props(classOf[RoomActor], s"room-$name")))
      val room = rooms(id)
      for (msg <- msgs) room ! msg
    case x =>
      log.warning(x.toString)
  }
}
