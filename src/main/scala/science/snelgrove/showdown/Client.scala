package science.snelgrove.showdown

import akka.actor.{ Actor, ActorRef, ActorSystem, Cancellable }
import akka.event.Logging
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.ws._
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import akka.stream.scaladsl._
import akka.util.ByteString
import akka.{ Done, NotUsed }
import com.googlecode.lanterna.input.{ KeyStroke, KeyType }
import com.googlecode.lanterna.terminal.ansi.{ UnixLikeTerminal, UnixTerminal }
import com.googlecode.lanterna.terminal.{ Terminal }
import java.nio.charset.Charset
import play.api.libs.json.{ JsValue, Json }
import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration._

case class KeyCharacter(key: Either[Char, KeyType], ctrl: Boolean, alt: Boolean, shift: Boolean)

object Client extends App {
  implicit def keystrokeConvert(key: KeyStroke): KeyCharacter = {
    val char = key.getKeyType match {
      case KeyType.Character => Left(key.getCharacter: Char)
      case _ => Right(key.getKeyType)
    }
    KeyCharacter(char, key.isCtrlDown(), key.isAltDown(), key.isShiftDown())
  }

  implicit val system: ActorSystem = ActorSystem("pokemon-showdown")
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  import system.dispatcher
  val log = Logging(system, "client")

  val term = new UnixTerminal(System.in, System.out, Charset.forName("UTF-8"),
    UnixLikeTerminal.CtrlCBehaviour.CTRL_C_KILLS_APPLICATION)
  //term.getTerminal.enterPrivateMode()
  term.clearScreen()

  // val url = "ws://echo.websocket.org"
  val url = "wss://sim2.psim.us/showdown/809/yp5rljjy/websocket"

  // print each incoming strict text message
  val incoming: Sink[Message, Future[Done]] =
    Sink.foreach {
      case message: TextMessage.Strict =>
        log.info(message.text)
        for ( c <- message.text) term.putCharacter(c)
        term.putCharacter('\n')
        term.flush()
      case c => log.warning(s"Unknown message $c")
    }

  val prelude: Source[String, NotUsed] =
    Source(List("|/help", "|/cmd rooms", "|/autojoin help", "|/join lobby"))

  val keyboard: Source[String, Cancellable] =
    Source.tick(0.seconds, 100.milliseconds, 'poll)
      .flatMapConcat(_ =>
        Source.unfold('poll)(_ => Option(term.pollInput()).map('poll -> _)))
      .map(keystrokeConvert(_))
      .log("keystrokes")
      .splitAfter { k: KeyCharacter =>
        k match {
          case KeyCharacter(Right(KeyType.Enter), _, _, _) => true
          case _ => false
        }
      }
      .collect {
        case KeyCharacter(Left(c), _, _, _) => c
      }
      .fold("") {(c, n) =>  c + n}
      .concatSubstreams

  val outgoing: Source[Message, Cancellable] = keyboard
    .map(m => TextMessage(s"""["$m"]\n"""): Message)
    .log("outgoing")

  val flow: Flow[Message, Message, Future[Done]] =
    Flow.fromSinkAndSourceMat(incoming, outgoing)(Keep.left)

  val (upgradeResponse, closed) =
    Http().singleWebSocketRequest(WebSocketRequest(url), flow)

  val connected = upgradeResponse.map { upgrade =>
    if (upgrade.response.status == StatusCodes.SwitchingProtocols) {
      Done
    } else {
      throw new RuntimeException(s"Connection failed: ${upgrade.response.status}")
    }
  }


  // In a real application you would not side effect here
  // and handle errors more carefully
  connected.onComplete(f => log.info(f.toString()))
  closed.foreach( _ => {
    log.info("closed")
    system.terminate()
  })
  //Await.result(system.whenTerminated, Duration.Inf)
}
