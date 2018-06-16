package science.snelgrove.showdown

import akka.actor.{ Actor, ActorRef, ActorSystem, Cancellable, Stash }
import akka.event.Logging
import science.snelgrove.showdown.protocol._

/**
  * Manages the initialization of the room based on type, then
  * the state of the room over its lifetime.
  */
class RoomActor(val name: String) extends Actor with Stash with ChatRoom with BattleRoom {
  def receive = {
    case RoomInit(BattleType) =>
      unstashAll()
      initBattle()
      context.become(battle orElse chat)
    case RoomInit(ChatType) =>
      unstashAll()
      context.become(chat)
    case msg => stash()
  }
}
