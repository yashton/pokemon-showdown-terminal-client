package science.snelgrove.showdown

import play.api.libs.json.Json
import science.snelgrove.showdown.protocol.{ShowdownCommand, TextCommand}

object OutgoingSerializer {
  def serialize(cmd: ShowdownCommand): String = cmd match {
      case TextCommand(text) =>
        Json.stringify(Json.arr(text))
  }
}
