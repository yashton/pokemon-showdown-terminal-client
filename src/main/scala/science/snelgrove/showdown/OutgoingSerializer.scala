package science.snelgrove.showdown

import play.api.libs.json.Json
import science.snelgrove.showdown.protocol._

object OutgoingSerializer {
  def serialize(cmd: ShowdownCommand): String = cmd match {
    case LoginCommand(user, token) =>
      val msg = s"|/trn $user,0,$token"
      Json.stringify(Json.arr(msg))
    case TargetedCommand(room, TextCommand(text)) =>
      val msg = if (room == "global") s"|$text" else s"$room|$text"
      Json.stringify(Json.arr(msg))
  }
}
