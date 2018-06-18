package science.snelgrove.showdown

import akka.event.Logging
import akka.actor.Actor
import scala.collection.mutable.{ Buffer, ListBuffer }
import science.snelgrove.showdown.protocol.BattleMessage

case class BattleUpdate(msgs: Seq[BattleMessage])

class BattleProcessor extends Actor {
  val log = Logging(context.system, this)
  val messageBuffer: Buffer[BattleMessage] = new ListBuffer()

  def receive = {
    case x: BattleMessage =>
      messageBuffer += x
      context.parent ! BattleUpdate(messageBuffer.toIndexedSeq)
  }
}
