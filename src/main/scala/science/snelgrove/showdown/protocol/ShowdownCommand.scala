package science.snelgrove.showdown.protocol

trait ShowdownCommand
case class TextCommand(text: String) extends ShowdownCommand
