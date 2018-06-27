package science.snelgrove.showdown

import akka.actor.{ Actor, ActorRef, ActorSystem, Cancellable, Props }
import akka.event.{ Logging, LoggingAdapter }
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.ws._
import akka.stream.impl.ActorRefSink
import akka.stream.scaladsl._
import akka.stream.{ ActorMaterializer, OverflowStrategy }
import akka.util.ByteString
import akka.{ Done, NotUsed }
import com.googlecode.lanterna.screen.{ Screen, TerminalScreen }
import com.googlecode.lanterna.terminal.ansi.{ UnixLikeTerminal, UnixTerminal }
import com.googlecode.lanterna.terminal.{ Terminal }
import com.typesafe.config._
import java.nio.charset.Charset
import play.api.libs.json.{ JsValue, Json }
import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}
import scala.util.Random
import science.snelgrove.showdown.protocol._
import science.snelgrove.showdown.ui._

object Client extends App {

  implicit val system: ActorSystem = ActorSystem("pokemon-showdown")
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  import system.dispatcher
  val log = Logging(system, "client")

  val config = ConfigFactory.load()
  val host = config.getString("showdown.uri")
  val nonceA = new Random().nextInt(1000)
  val nonceB = new Random().alphanumeric.take(8).foldLeft("")(_ + _)
  val uri = f"$host/$nonceA%03d/$nonceB/websocket"

  lazy val roomRouter = system.actorOf(Props[RoomRouter], "room-router")

  val term = new UnixTerminal(System.in, System.out, Charset.forName("UTF-8"),
    UnixLikeTerminal.CtrlCBehaviour.CTRL_C_KILLS_APPLICATION)
  val screen = new TerminalScreen(term)

  lazy val userInterface = system.actorOf(Props(classOf[UserInterface], screen), "console-renderer")

  lazy val incoming = WebsocketFlows.parseFlow.to(Sink.actorRef(roomRouter, Done))
  lazy val outgoing = WebsocketFlows.serializeFlow

  val flow: Flow[Message, Message, ActorRef] =
    Flow.fromSinkAndSourceMat(incoming, outgoing)(Keep.right)

  val (upgradeResponse, outputActor) =
    Http().singleWebSocketRequest(WebSocketRequest(uri), flow)

  val connected = upgradeResponse.map { upgrade =>
    if (upgrade.response.status == StatusCodes.SwitchingProtocols) {
      Done
    } else {
      throw new RuntimeException(s"Connection failed: ${upgrade.response.status}")
    }
  }

  connected.onComplete { f =>
    roomRouter ! Subscribe(userInterface)
    roomRouter ! ClientConnected(outputActor)
    userInterface ! ClientConnected(outputActor)

    log.info(s"Connected ${f.toString()}")
  }
}

object WebsocketFlows {
  val parseFlow =
      Flow[Message]
      .flatMapConcat {
        case message: TextMessage.Strict =>
          MessageParser.parseRaw(message.text) match {
            case Some(l) => Source.apply[Target](l)
            case None => Source.empty[Target]
          }
        case message: TextMessage.Streamed =>
          message.textStream
            .reduce(_ + _)
            .flatMapConcat { s =>
              MessageParser.parseRaw(s) match {
                case Some(l) => Source.apply[Target](l)
                case None => Source.empty[Target]
              }
            }
      }

  val serializeFlow: Source[Message, ActorRef] =
    Source.actorRef[ShowdownCommand](10000, OverflowStrategy.fail)
      .map(OutgoingSerializer.serialize)
      .log("outgoing")
      .map(m => TextMessage(s"$m\n"): Message)

}
