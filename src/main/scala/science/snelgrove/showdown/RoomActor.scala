package science.snelgrove.showdown

import akka.actor.{ Actor, ActorRef, ActorSystem, Cancellable, Stash, Props }
import akka.event.Logging
import scala.collection.mutable.{ Buffer, ListBuffer, SortedSet, TreeSet }
import science.snelgrove.showdown.protocol._

/**
  * Manages the initialization of the room based on type, then
  * routing the state of the room over its lifetime.
  */
class RoomActor(val name: String, val output: ActorRef) extends Actor with Stash {
  val log = Logging(context.system, this)

  def receive = {
    case RoomInit(BattleType) =>
      log.info(s"Initialize battle room $name")
      unstashAll()
      context.become(base(context.actorOf(Props[BattleRoom], s"$name-subproc")))
    case RoomInit(ChatType) =>
      log.info(s"Initialize chat room $name")
      unstashAll()
      context.become(base(context.actorOf(Props[ChatRoom], s"$name-subproc")))
    case RoomInit(GlobalType) =>
      log.info(s"Initialize global room $name")
      unstashAll()
      context.become(base(context.actorOf(Props[GlobalRoom], s"$name-subproc")))
    case RoomDeinit =>
      log.info(s"Leaving room $name")
      output ! LeaveRoom(name)
      context.stop(self)
    case _ => stash()
  }

  var title: Option[String] = None
  var current: Option[State] = None
  def base(sub: ActorRef): PartialFunction[Any, Unit] = {
    case RoomTitle(t) =>
      title = Some(t)
      current.foreach { s =>
        output ! StateUpdate(name, title, s)
      }
    case RoomDeinit =>
      log.info(s"Leaving room $name")
      output ! LeaveRoom(name)
      context.stop(self)
    case s: State =>
      current = Some(s)
      output ! StateUpdate(name, title, s)
    case msg => sub ! msg
  }
}

sealed trait State {
  def chat: Seq[ChatMessage]
  def users: Seq[String]
}

case class ChatState(chat: Seq[ChatMessage], users: Seq[String]) extends State
case class BattleState(chat: Seq[ChatMessage], users: Seq[String], battle: GameState) extends State
case class GlobalState(chat: Seq[ChatMessage], users: Seq[String], global: Seq[GlobalMessage]) extends State

case class StateUpdate(name: String, title: Option[String], state: State)
case class LeaveRoom(name: String)
