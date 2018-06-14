package science.snelgrove.showdown

import akka.actor.Actor
import play.api.libs.json.Json
import science.snelgrove.showdown.protocol._
import java.time.Instant

class MessageParser {
  import GeneralParser._

  private val obj = "^o$".r
  private val arr = "^a(.*)$".r
  def parseRaw(msg: String) = msg match {
    case obj() => Unit
    case arr(rest) =>
      for {
        line <- Json.parse(rest).as[Seq[String]]
      } yield parse(line)
  }

  def parse(line: String) : Target = {
    val msgs = line.split("\n")

    if (msgs.head.startsWith(">"))
      Room(msgs.head.tail, msgs.tail.map(parseMessage))
    else
      Global(msgs.map(parseMessage))
  }

  private val chat = "^(c|chat)$".r
  private val leave = "^([lL]|leave)$".r
  private val join = "^([jJ]|join)$".r
  private val name = "^([nN]|name)$".r
  private val battle = "^([bB]|battle)$".r

  def parseMessage(message: String): ShowdownMessage = {
    if (!message.startsWith("|")) {
      Log(message)
    } else {
      val tokenized = message.tail.split('|').toSeq
      tokenized match {
        case "" :: msg :: Nil =>
          Log(msg)
        case Nil =>
          Empty

        // Room messages
        case chat() :: user :: msg :: Nil =>
          Chat(parseUser(user), msg, None)
        case "c:" :: timestamp :: user :: msg :: Nil =>
          Chat(parseUser(user), msg, Some(Instant.ofEpochSecond(timestamp.toInt)))
        case join() :: user :: Nil =>
          Join(parseUser(user))
        case leave() :: user :: Nil =>
          Leave(parseUser(user))
        case name() :: user :: old :: Nil =>
          Name(parseUser(user), old)
        case "init" :: roomType :: Nil =>
          RoomInit(parseRoomType(roomType))
        case "users" :: list :: Nil =>
          RoomUsers(list.split(',').map(parseUser))
        case "html" :: body :: Nil =>
          HtmlMessage(body)
        case "uhtml" :: name :: body :: Nil =>
          DynamicHtmlMessage(name, body)
        case "uhtmlchange" :: name :: body :: Nil =>
          DynamicHtmlMessageUpdate(name, body)
        case ":" :: timestamp :: Nil =>
          Timestamp(Instant.ofEpochSecond(timestamp.toInt))
        case "battle" :: room :: playerOne :: playerTwo :: Nil =>
          BattleStart(room, parseUser(playerOne), parseUser(playerTwo))
        case "deinit" :: Nil =>
          RoomDeinit
        case "raw" :: msg :: Nil =>
          RawMessage(msg)

        // Global messages
        case "popup" :: msg :: Nil =>
          Popup(msg)
        case "pm" :: sender :: receiver :: msg :: Nil =>
          PrivateMessage(parseUser(sender), parseUser(receiver), msg)
        case "usercount" :: count :: Nil =>
          UserCount(count.toInt)
        case "nametaken" :: username :: reason :: Nil =>
          NameTaken(username, reason)
        case "challstr" :: token :: Nil =>
          LoginToken(token)
        case "updateuser" :: username :: named :: avatar :: Nil =>

          UpdateUser(username, named == "1", avatar)
        case "formats" :: tail =>
          Formats(FormatsParser.parse(tail))
        case "updatesearch" :: json :: Nil =>
          UpdateSearch(Json.parse(json))
        case "updatechallenges" :: json :: Nil =>
          UpdateChallenges(Json.parse(json))
        case "queryresponse" :: query :: json :: Nil =>
          QueryResponse(query, Json.parse(json))

        // Battle messages
        case "player" :: player :: username :: avatar :: Nil =>
          PlayerConnect(parsePlayer(player), parseUser(username), avatar)
        case "gametype" :: game :: Nil =>
          BattleGameType(parseGameType(game))
        case "gen" :: gen :: Nil =>
          BattleGeneration(gen.toInt)
        case "tier" :: tier :: Nil =>
          Tier(tier)
        case "rated" :: Nil =>
          BattleRated
        case "rule" :: clause :: Nil =>
          Rule(clause)
        case "clearpoke" :: Nil =>
          ClearPreview
        case "poke" :: player :: details :: item :: Nil =>
          PreviewPokemon(parsePlayer(player), DetailsParser.parse(details), item)
        case "teampreview" :: Nil =>
          TeamPreview
        case "start" :: Nil =>
          BattleStart
        case "request" :: json :: Nil =>
          Request(Json.parse(json))
        case "inactive" :: msg :: Nil =>
          BattleTimer(true, msg)
        case "inactiveoff" :: msg :: Nil =>
          BattleTimer(false, msg)
        case "turn" :: count :: Nil =>
          Turn(count.toInt)
        case "win" :: user :: Nil =>
          Win(parseUser(user))
        case "tie" :: Nil =>
          Tie
        case "teamsize" :: player :: size :: Nil =>
          TeamSize(parsePlayer(player), size.toInt)
        case "upkeep" :: Nil =>
          Upkeep

        // Battle Actions
        // TODO get the edge cases
        case "move" :: pokemon :: move :: rest =>
          Move(PokemonParser.parse(pokemon), move, None, true)
        case "switch" :: pokemon :: details :: status :: Nil =>
          Switch(PokemonParser.parse(pokemon), DetailsParser.parse(details), StatusParser.parse(status))
        case "drag" :: pokemon :: details :: status :: Nil =>
          Drag(PokemonParser.parse(pokemon), DetailsParser.parse(details), StatusParser.parse(status))
        case "detailschange" :: pokemon :: details :: status :: Nil =>
          DetailsChange(PokemonParser.parse(pokemon), DetailsParser.parse(details), StatusParser.parse(status))
        case "replace" :: pokemon :: details :: status :: Nil =>
          Replace(PokemonParser.parse(pokemon), DetailsParser.parse(details), StatusParser.parse(status))
        case "swap" :: pokemon :: position :: Nil =>
          Swap(PokemonParser.parse(pokemon), position.toInt)
        case "cant" :: pokemon :: reason :: Nil =>
          Cant(PokemonParser.parse(pokemon), reason, None)
        case "cant" :: pokemon :: reason :: move :: Nil =>
          Cant(PokemonParser.parse(pokemon), reason, Some(move))
        case "faint" :: pokemon :: Nil =>
          Faint(PokemonParser.parse(pokemon))

        // Battle Minor Actions
        case "-failed" :: pokemon :: action :: Nil =>
          Failed(PokemonParser.parse(pokemon), action)
        case "-damage" :: pokemon :: status :: Nil =>
          Damage(PokemonParser.parse(pokemon), StatusParser.parse(status))
        case "-heal" :: pokemon :: status :: Nil =>
          Heal(PokemonParser.parse(pokemon), StatusParser.parse(status))
        case "-status" :: pokemon :: effect :: Nil =>
          Status(PokemonParser.parse(pokemon), StatusParser.parseEffect(effect))
        case "-cure" :: pokemon :: effect :: Nil =>
          Cure(PokemonParser.parse(pokemon), StatusParser.parseEffect(effect))
        case "-cureteam" :: pokemon :: Nil =>
          CureTeam(PokemonParser.parse(pokemon))
        case "-boost" :: pokemon :: stat :: amount :: details :: Nil =>
          Boost(PokemonParser.parse(pokemon), parseStat(stat), amount.toInt, Some(details))
        case "-unboost" :: pokemon :: stat :: amount :: details :: Nil =>
          Unboost(PokemonParser.parse(pokemon), parseStat(stat), amount.toInt, Some(details))
        case "-boost" :: pokemon :: stat :: amount :: Nil =>
          Boost(PokemonParser.parse(pokemon), parseStat(stat), amount.toInt, None)
        case "-unboost" :: pokemon :: stat :: amount :: Nil =>
          Unboost(PokemonParser.parse(pokemon), parseStat(stat), amount.toInt, None)
        case "-weather" :: weather :: Nil =>
          Weather(weather, false)
        case "-weather" :: weather :: "[upkeep]" :: Nil =>
          Weather(weather, true)
        case "-fieldconditionstart" :: condition :: Nil =>
          FieldConditionStart(condition)
        case "-fieldconditionstop" :: condition :: Nil =>
          FieldConditionStop(condition)
        case "-sideconditionstart" :: side :: condition :: Nil =>
          SideConditionStart(parsePlayer(side), condition)
        case "-sideconditionstop" :: side :: condition :: Nil =>
          SideConditionStop(parsePlayer(side), condition)
        case "-crit" :: pokemon :: Nil =>
          Crit(PokemonParser.parse(pokemon))
        case "-supereffective" :: pokemon :: Nil =>
          SuperEffective(PokemonParser.parse(pokemon))
        case "-resisted" :: pokemon :: Nil =>
          Resisted(PokemonParser.parse(pokemon))
        case "-immune" :: pokemon :: Nil =>
          Immune(PokemonParser.parse(pokemon))
        case "-item" :: pokemon :: item :: Nil =>
          Item(PokemonParser.parse(pokemon), item)
        case "-enditem" :: pokemon :: item :: Nil =>
          EndItem(PokemonParser.parse(pokemon), item)
        case "-ability" :: pokemon :: ability :: Nil =>
          Ability(PokemonParser.parse(pokemon), ability)
        case "-endability" :: pokemon :: Nil =>
          EndAbility(PokemonParser.parse(pokemon))
        case "-transform" :: pokemon :: species :: Nil =>
          Transform(PokemonParser.parse(pokemon), species)
        case "-megaevolve" :: pokemon :: stone :: Nil =>
          MegaEvolve(PokemonParser.parse(pokemon), stone)
        case "-activateeffect" :: effect :: Nil =>
          ActivateEffect(effect)
        case "-hint" :: message :: Nil =>
          Hint(message)
        case "-center" :: Nil =>
          TripleCenter
        case "-message" :: msg :: Nil =>
          MiscMessage(msg)
        case "-endeffect" :: pokemon :: effect :: Nil =>
          EndEffect(PokemonParser.parse(pokemon), effect)
        case "-setboost" :: pokemon :: stat :: amount :: Nil =>
          SetBoost(PokemonParser.parse(pokemon), parseStat(stat), amount.toInt, None)
        case "-setboost" :: pokemon :: stat :: amount :: details :: Nil =>
          SetBoost(PokemonParser.parse(pokemon), parseStat(stat), amount.toInt, Some(details))
        case "-zmove" :: pokemon :: Nil =>
          Zpower(PokemonParser.parse(pokemon))
        case _ =>
          UnknownMessage(message)
      }
    }
  }
}

object GeneralParser {
  def parseGameType(game: String) : GameType = game match {
    case "singles" => Singles
    case "doubles" => Doubles
    case "triples" => Triples
  }

  def parsePlayer(player: String) : Player = player match {
    case "p1" => PlayerOne
    case "p2" => PlayerTwo
  }

  def parseRoomType(room: String) : RoomType = room match {
    case "battle" => BattleRoom
    case "chat" => ChatRoom
  }

  def parseUser(user: String): User = {
    User(user.tail, Option(user.head).filter(_ != ' '))
  }

  def parseStat(stat:String) : Stat = stat match {
    case "atk" => Atk
    case "def" => Def
    case "spa" => SpA
    case "spd" => SpD
    case "spe" => Spe
  }
}

object FormatsParser {
  def parse(formats: Seq[String]) : Map[String, Seq[Format]] = {
    Map()
  }
}

object PokemonParser {
  val id = "^(p[12])([a-z])?: (.*)".r
  def parse(pokemon: String): PokemonId = pokemon match {
    case id(player, position, name) =>
      PokemonId(GeneralParser.parsePlayer(player), Option(position), name)
  }

}

object DetailsParser {
  val speciesExtract = "^([^-]+)(-([.*]))?$".r
  val shinyToken = "^, ?shiny".r
  val levelToken = "^, ?L([0-9]+)$".r
  val maleToken = "^, ?M".r
  val femaleToken = "^, ?F".r
  def parse(details : String) : PokemonDetails = {
    val species :: tokens = details.split(", ?").toSeq
    val shiny = tokens.exists(_ match {
      case shinyToken() => true
      case _ => false
    })
    val gender = tokens.collectFirst {
      case maleToken() => Male
      case femaleToken() => Female
    }.getOrElse(Genderless)
    val level = tokens.collectFirst {
      case levelToken(level) => level.toInt
    }.getOrElse(100)

    species match {
      case speciesExtract(s, _, forme) =>
        PokemonDetails(species, Option(forme), shiny, gender, level)
    }
  }
}

object StatusParser {
  val percentStatus = "^([0-9]+)/100( .*)?".r
  val hpStatus = "^([0-9]+)/([0-9]+)( .*)?".r
  val binaryStatus = "^([0-9]+)/48( .*)?".r
  def parse(status: String) : PokemonStatus = status match {
    case "0 fnt" => FaintedStatus
    case percentStatus(current, effect) =>
      if (current.toInt > 0) PercentStatus(current.toInt, Option(effect).map(e => parseEffect(e.tail)))
      else FaintedStatus
    case binaryStatus(current, effect) =>
      if (current.toInt > 0) BinaryStatus(current.toInt, Option(effect).map(e => parseEffect(e.tail)))
      else FaintedStatus
    case hpStatus(current, max, effect) =>
      if (current.toInt > 0)
        HpStatus(current.toInt, max.toInt, Option(effect).map(e => parseEffect(e.tail)))
      else FaintedStatus
  }

  def parseEffect(effect: String) : StatusEffect = StatusEffect(effect)
}