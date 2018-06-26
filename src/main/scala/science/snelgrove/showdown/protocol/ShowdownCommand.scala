package science.snelgrove.showdown.protocol

trait ShowdownCommand
case class TextCommand(text: String) extends ShowdownCommand
case class TargetedCommand(room: String, cmd: TextCommand) extends ShowdownCommand
case class LoginCommand(user: String, token: String) extends ShowdownCommand
