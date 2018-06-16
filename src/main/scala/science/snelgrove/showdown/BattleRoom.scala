package science.snelgrove.showdown

import akka.actor.Actor

/**
  * Handles the battle behavior of the room.
  * Mixed into RoomActor.
  */
trait BattleRoom extends Actor {
  def initBattle(): Unit = ???
  def battle: PartialFunction[Any, Unit] = {
    case _ => ???
  }
}
