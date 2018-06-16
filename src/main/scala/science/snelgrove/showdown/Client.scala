package science.snelgrove.showdown

import akka.actor.{ Actor, ActorRef, ActorSystem, Cancellable, Props }
import akka.event.Logging
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.ws._
import akka.http.scaladsl.server.Route
import akka.stream.{ ActorMaterializer, OverflowStrategy }
import akka.stream.impl.ActorRefSink
import akka.stream.scaladsl._
import akka.util.ByteString
import akka.{ Done, NotUsed }
import com.googlecode.lanterna.input.{ KeyStroke, KeyType }
import com.googlecode.lanterna.screen.{ Screen, TerminalScreen }
import com.googlecode.lanterna.terminal.ansi.{ UnixLikeTerminal, UnixTerminal }
import com.googlecode.lanterna.terminal.{ Terminal }
import java.nio.charset.Charset
import play.api.libs.json.{ JsValue, Json }
import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration._
import science.snelgrove.showdown.protocol.{ ShowdownCommand, ShowdownMessage, Target }

case class KeyCharacter(key: Either[Char, KeyType], ctrl: Boolean, alt: Boolean, shift: Boolean)

object Client extends App {

  implicit val system: ActorSystem = ActorSystem("pokemon-showdown")
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  import system.dispatcher
  val log = Logging(system, "client")

  val roomRouter = system.actorOf(Props[RoomRouter], "room-router")

  val term = new UnixTerminal(System.in, System.out, Charset.forName("UTF-8"),
    UnixLikeTerminal.CtrlCBehaviour.CTRL_C_KILLS_APPLICATION)

  val url = "wss://sim2.psim.us/showdown/809/yp5rljjy/websocket"

  val incoming =
    Flow[Message]
      .flatMapConcat {
        case message: TextMessage.Strict =>
//          log.debug(message.text)
          MessageParser.parseRaw(message.text) match {
            case Some(l) => Source.apply[Target](l)
            case None => Source.empty[Target]
          }
      }
      .to(Sink.actorRef(roomRouter, Done))

  val outgoing: Source[Message, ActorRef] =
    Source.actorRef[ShowdownCommand](10000, OverflowStrategy.fail)
      .map(OutgoingSerializer.serialize)
      .log("outgoing")
      .map(m => TextMessage(s"$m\n"): Message)

  val flow: Flow[Message, Message, ActorRef] =
    Flow.fromSinkAndSourceMat(incoming, outgoing)(Keep.right)

  val (upgradeResponse, outputActor) =
    Http().singleWebSocketRequest(WebSocketRequest(url), flow)

  val screenRender = system.actorOf(
    Props(classOf[ScreenRender], new TerminalScreen(term)), "console-renderer")

  val inputParser = system.actorOf(
    Props(classOf[InputParser], outputActor, screenRender), "input-parser")

  Keyboard.graph(term, inputParser).run()

  val connected = upgradeResponse.map { upgrade =>
    if (upgrade.response.status == StatusCodes.SwitchingProtocols) {
      Done
    } else {
      throw new RuntimeException(s"Connection failed: ${upgrade.response.status}")
    }
  }

  connected.onComplete(f => log.info(f.toString()))
}

object Keyboard {
  implicit def keystrokeConvert(key: KeyStroke): KeyCharacter = {
    val char = key.getKeyType match {
      case KeyType.Character => Left(key.getCharacter: Char)
      case _ => Right(key.getKeyType)
    }
    KeyCharacter(char, key.isCtrlDown(), key.isAltDown(), key.isShiftDown())
  }
  def graph(term: Terminal, inputParser: ActorRef) =
    Source.tick(0.seconds, 100.milliseconds, 'poll)
      .flatMapConcat(_ =>
        Source.unfold('poll)(_ => Option(term.pollInput()).map('poll -> _)))
      .map(keystrokeConvert(_))
      .to(Sink.actorRef(inputParser, Done))
}
