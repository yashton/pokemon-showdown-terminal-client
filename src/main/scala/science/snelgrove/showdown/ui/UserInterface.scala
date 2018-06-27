package science.snelgrove.showdown.ui

import akka.Done
import akka.actor.{ Actor, ActorRef, ActorSystem, Cancellable, Stash, Props}
import akka.event.Logging
import akka.stream.scaladsl._
import akka.stream.ActorMaterializer
import com.googlecode.lanterna.TerminalSize
import com.googlecode.lanterna.screen.TerminalScreen
import scala.collection.mutable.LinkedHashMap
import scala.concurrent.duration._
import science.snelgrove.showdown._
import science.snelgrove.showdown.protocol._

/**
  * Manages rendering of game state and user interactions
  */
class UserInterface(val screen: TerminalScreen) extends Actor with Stash {
  val log = Logging(context.system, this)
  implicit val materializer = ActorMaterializer()

  val rooms: LinkedHashMap[String, StateUpdate] = new LinkedHashMap()
  var activeRoom: String = "global"

  var inputText: String = ""

  lazy val inputParser = context.actorOf(Props(classOf[InputParser]), "input-parser")

  val inputPolling =
    Source.tick(0.seconds, 100.milliseconds, 'poll)
      .flatMapConcat(_ =>
        Source.unfold('poll)(_ => Option(screen.pollInput()).map('poll -> _)))
      .map(Keyboard.keystrokeConvert(_))
      .to(Sink.actorRef(inputParser, Done))

  override def preStart(): Unit = {
    screen.startScreen()
    screen.getTerminal().addResizeListener((t, s) => if (s != null) self ! s)
    new Renderer(screen).renderAll(rooms, activeRoom, inputText)
    screen.refresh()

    inputPolling.run()
  }

  override def postStop(): Unit = {
    screen.stopScreen()
  }

  def receive = {
    case ClientConnected(outgoing) =>
      unstashAll()
      context.become(running(outgoing))
    case _ => stash()
  }

  def running(outgoing: ActorRef): PartialFunction[Any, Unit] = {
    case s: TerminalSize =>
      log.info("Resizing screen")
      if (screen.doResizeIfNecessary() != null) {
        screen.clear()
        new Renderer(screen).renderAll(rooms, activeRoom, inputText)
        screen.refresh()
      }
    case s @ StateUpdate(n, _, _) =>
      rooms.update(n, s)
      new Renderer(screen).renderAll(rooms, activeRoom, inputText)
      screen.refresh()
    case ChatTypingUpdate(text) =>
      inputText = text
      new Renderer(screen).renderInput(inputText)
      screen.refresh()
    case t: TextCommand =>
      outgoing ! TargetedCommand(activeRoom, t)
    case RoomSwitch(i) =>
      if (i < rooms.size) {
        activeRoom = rooms.keys.toIndexedSeq.apply(i)
        log.info(s"Switching rooms to ${activeRoom}")
        screen.clear()
        new Renderer(screen).renderAll(rooms, activeRoom, inputText)
        screen.refresh()
      }
    case msg => log.warning(msg.toString())
  }
}
