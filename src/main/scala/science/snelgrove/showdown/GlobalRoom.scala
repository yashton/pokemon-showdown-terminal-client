package science.snelgrove.showdown

import akka.actor.{ Actor, Props, Stash, ActorRef }
import akka.event.Logging
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.Uri.Query
import akka.http.scaladsl.model.{ HttpMethod, HttpRequest, HttpResponse, Uri }
import akka.pattern.pipe
import akka.stream.ActorMaterializer
import akka.util.ByteString
import com.typesafe.config._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{ Buffer, SortedSet, TreeSet }
import science.snelgrove.showdown.protocol._

/**
  * Handles the primary chat behavior of the room.
  */
class GlobalRoom extends Actor with Stash {
  val log = Logging(context.system, this)
  val config = ConfigFactory.load()
  val user = config.getString("showdown.user")

  val chat = context.actorOf(Props[ChatBuffer], s"${self.path.name}-chat")
  val users = context.actorOf(Props[UserList], s"${self.path.name}-user")

  val messageBuffer: Buffer[GlobalMessage] = new ListBuffer()
  var current = GlobalState(Seq(), Seq(), Seq())

  implicit val materializer = ActorMaterializer()
  implicit val execContext = context.system.dispatcher
  def receive = {
    case ClientConnected(ref) =>
      unstashAll()
      context.become(loggingIn(ref))
    case _ => stash()
  }

  def loggingIn(output: ActorRef): PartialFunction[Any, Unit] = {
    case LoginChallenge(token) =>
      val uri = Uri("https://play.pokemonshowdown.com/action.php").withQuery(
        Query("act" -> "getassertion",
        "userid" -> user,
        "challstr" -> token)
      )
      val request = HttpRequest(uri = uri)
      val resp = Http()(context.system).singleRequest(request)
      pipe(resp) to self
    case r: HttpResponse =>
      r.entity.dataBytes.runFold(ByteString(""))(_ ++ _).foreach { body =>
        log.info(s"Logging in with token ${body.utf8String}")
        output ! LoginCommand(user, body.utf8String)
      }
      unstashAll()
      context.become(running)
    case _ => stash()
  }

  def running: PartialFunction[Any, Unit] = {
    case c: ChatMessage => chat ! c
    case m: GlobalMessage =>
      messageBuffer += m
      current = current.copy(global = messageBuffer.toIndexedSeq)
      context.parent ! current
    case UserListUpdate(u) =>
      current = current.copy(users = u)
      context.parent ! current
    case ChatUpdate(u) =>
      current = current.copy(chat = u)
      context.parent ! current
  }
}

case class ClientConnected(output: ActorRef)
