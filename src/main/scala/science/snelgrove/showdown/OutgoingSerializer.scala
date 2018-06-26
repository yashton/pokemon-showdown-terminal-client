package science.snelgrove.showdown

import play.api.libs.json.Json
import science.snelgrove.showdown.protocol.{ShowdownCommand, TextCommand, TargetedCommand}

object OutgoingSerializer {
  def serialize(cmd: ShowdownCommand): String = cmd match {
    case TargetedCommand(room, TextCommand(text)) =>
      val msg = if (room == "global") s"|$text" else s">$room\n|$text"
      Json.stringify(Json.arr(msg))
  }
}
