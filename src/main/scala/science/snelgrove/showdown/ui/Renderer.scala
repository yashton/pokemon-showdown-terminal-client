package science.snelgrove.showdown.ui

import com.googlecode.lanterna.graphics.TextGraphics
import com.googlecode.lanterna.{ TerminalPosition, TerminalSize, TextColor }
import com.googlecode.lanterna.screen.TerminalScreen
import java.time.ZoneId
import java.time.format.DateTimeFormatter
import scala.collection.mutable.LinkedHashMap
import science.snelgrove.showdown.protocol._
import science.snelgrove.showdown._


class Renderer(val screen: TerminalScreen) {
  type State = LinkedHashMap[String, StateUpdate]
  val userSize = 20

  def renderAll(rooms: State, activeRoom: String, inputText: String): Unit = {
    renderBar(rooms, activeRoom)
    renderInput(inputText)
    rooms.get(activeRoom).foreach { room =>
      renderChat(room)
      renderUsers(room)
    }
  }

  def renderBar(rooms: State, activeRoom: String): Unit = {
    val g = screen.newTextGraphics()
    val r = g.getSize.getRows
    val c = g.getSize.getColumns
    g.setBackgroundColor(TextColor.ANSI.BLUE)
    g.setForegroundColor(TextColor.ANSI.BLACK)
    g.drawLine(0, r - 2, c - 1, r - 2, ' ')
    g.drawLine(c - userSize - 2, 0, c - userSize - 2, r - 3, '|')
    val names = for {
      ((name, room), i) <- rooms.zipWithIndex
      active = if (name == activeRoom) "*" else " "
    } yield s"$i:${name}${active}"

    g.putString(0, r - 2, names.mkString(" ").take(c))
  }

  def renderInput(inputText: String): Unit = {
    val g = screen.newTextGraphics()
    val c = g.getSize.getColumns
    val r = g.getSize.getRows
    g.drawLine(0, r - 1, c - 1, r - 1, ' ')
    g.putString(0, r - 1, inputText)
    screen.setCursorPosition(new TerminalPosition(inputText.size, r - 1))
  }
  type ChatLines = Seq[Either[(Option[String], String, String), String]]

  def chatLines(c: Chat, sx: Int): ChatLines = c match {
    case Chat(User(user, _), msg, time) =>
      val timestr = time.map(formatter.format)
      // This will implicitly truncate too long usernames, since the message will
      // be pushed off the edge of the first line, and the username is not split
      // on line length
      val prefixLen = sx - Math.min(timestr.map(_.size + 1).getOrElse(0) + user.size + 1, sx)

      val first = msg.take(prefixLen - 1)
      val rest = msg.drop(prefixLen - 1).grouped(sx)
      Left((timestr, user, first)) :: (rest.map(Right.apply _)).toList
  }


  val formatter = DateTimeFormatter.ofPattern("HH:mm:ss").withZone(ZoneId.systemDefault)
  def renderChat(room: StateUpdate): Unit = {
    val g = screen.newTextGraphics
    val c = g.getSize.getColumns
    val r = g.getSize.getRows

    val (sx, sy) = (c - userSize - 1, r - 2)
    val (ox, oy) = (0, 0)
    val history = room.state.chat
    val lines = history collect { case c: Chat => chatLines(c, sx) } flatten

    for {
      (u, i) <- lines.takeRight(Math.min(sy, lines.size)).zipWithIndex
    } {
      u match {
        case Left((Some(time), user, msg)) =>
          g.clearModifiers
          g.setForegroundColor(TextColor.ANSI.GREEN)
          g.putString(ox, oy + i, time ++ " ")
          g.setForegroundColor(TextColor.ANSI.MAGENTA)
          val timeOffset = time.size + 1
          g.putString(ox + timeOffset, oy + i, (user ++ " ").take(sx - timeOffset - 1))
          g.setForegroundColor(TextColor.ANSI.WHITE)
          val nameOffset = timeOffset + user.size + 1
          g.putString(ox + nameOffset, oy + i, msg.take(sx - nameOffset - 1).padTo(sx - nameOffset - 1, ' '))
        case Left((None, user, msg)) =>
          g.clearModifiers
          g.setForegroundColor(TextColor.ANSI.MAGENTA)
          g.putString(ox, oy + i, (user ++ " ").take(sx - 1))
          g.setForegroundColor(TextColor.ANSI.WHITE)
          val nameOffset = user.size + 1
          g.putString(ox + nameOffset, oy + i, msg.take(sx - nameOffset - 1).padTo(sx - nameOffset - 1, ' '))
        case Right(msg) =>
          g.clearModifiers
          g.putString(ox, oy + i, msg.padTo(sx - 1, ' '))
      }
    }
  }

  def renderUsers(room: StateUpdate): Unit = {
    val g = screen.newTextGraphics
    val c = g.getSize.getColumns
    val r = g.getSize.getRows

    val (sx, sy) = (userSize, r - 3)
    val (ox, oy) = (c - userSize - 1, 1)
    val users = room.state.users
    for {
      (u, i) <- users.takeRight(Math.min(sy, users.size)).zipWithIndex
    } g.putString(ox, oy + i, u.take(sx  - 1).padTo(sx, ' '))

    g.setBackgroundColor(TextColor.ANSI.BLUE)
    g.setForegroundColor(TextColor.ANSI.BLACK)
    g.putString(c - 1 - userSize, 0, s"Users ${users.size}".padTo(sx, ' '))
  }
}
