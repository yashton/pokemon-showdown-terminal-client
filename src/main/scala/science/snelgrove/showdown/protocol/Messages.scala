package science.snelgrove.showdown.protocol
import java.time.Instant
import play.api.libs.json.JsValue
import science.snelgrove.showdown.BattleRequest

sealed trait Target {
  def msgs: Seq[ShowdownMessage]
}

case class Room(room: String, msgs: Seq[ShowdownMessage]) extends Target
case class Global(msgs: Seq[ShowdownMessage]) extends Target

sealed trait ShowdownMessage
case object Empty extends ShowdownMessage

sealed trait RoomMessage extends ShowdownMessage
case class RoomInit(roomType: RoomType) extends RoomMessage
case class RoomTitle(title: String) extends RoomMessage // Not Documented
case object RoomDeinit extends RoomMessage // Not Documented

sealed trait ChatMessage extends RoomMessage
case class Log(text: String) extends ChatMessage
case class Chat(user: User, text: String, timestamp: Option[Instant] = None) extends ChatMessage
case class HtmlMessage(body: String) extends ChatMessage
case class DynamicHtmlMessage(name: String, body: String) extends ChatMessage
case class DynamicHtmlMessageUpdate(name: String, body: String) extends ChatMessage
case class Timestamp(time: Instant) extends ChatMessage
case class BattleStart(room: String, playerOne: User, playerTwo: User) extends ChatMessage
case class RawMessage(body: String) extends ChatMessage // Not Documented

sealed trait UsersMessage extends RoomMessage
case class Join(user: User) extends UsersMessage
case class Leave(user: User) extends UsersMessage
case class Name(user: User, oldId: String) extends UsersMessage
case class RoomUsers(users: Seq[User]) extends UsersMessage

sealed trait GlobalMessage extends ShowdownMessage
case class UnknownMessage(msg: String) extends GlobalMessage
case class Popup(text: String) extends GlobalMessage
case class PrivateMessage(sender: User, receiver: User, text: String) extends GlobalMessage
case class UserCount(count: Int) extends GlobalMessage
case class NameTaken(username: String, reason: String) extends GlobalMessage
case class LoginChallenge(token: String) extends GlobalMessage
case class UpdateUser(username: String, named: Boolean, avatar: String) extends GlobalMessage
case class Format(name: String, random: Boolean, searchOnly: Boolean, challengeOnly: Boolean)
case class Formats(format: Map[String, Seq[Format]]) extends GlobalMessage
case class UpdateSearch(body: JsValue) extends GlobalMessage
case class UpdateChallenges(body: JsValue) extends GlobalMessage
case class QueryResponse(queryType: String, body: JsValue) extends GlobalMessage

case class UndocumentedStart(pokemon: PokemonId) extends ShowdownMessage

sealed trait BattleMessage extends ShowdownMessage
case class PlayerConnect(player: Player, username: String, avatar: String) extends BattleMessage
case class BattleGameType(gameType: GameType) extends BattleMessage
case class BattleGeneration(gen: Int) extends BattleMessage
case class BattleRated(league: String) extends BattleMessage
case class Rule(clause: String) extends BattleMessage
case class Tier(tier: String) extends BattleMessage
case object BattleStart extends BattleMessage
case object TeamPreview extends BattleMessage
case object Seed extends BattleMessage
case class PreviewPokemon(player: Player, details: PokemonDetails, item: Boolean) extends BattleMessage
case object ClearPreview extends BattleMessage
case class Request(body: Option[BattleRequest]) extends BattleMessage
case class BattleTimer(active: Boolean, message: String) extends BattleMessage
case class Turn(number: Int) extends BattleMessage
case class TeamSize(player: Player, size: Int) extends BattleMessage // Not Documented
case object Upkeep extends BattleMessage // Not Documented

sealed trait TerminalState extends BattleMessage
case class Win(username: String) extends TerminalState
case object Tie extends TerminalState

sealed trait MajorAction extends BattleMessage
sealed trait PokemonAction {
  def pokemon: PokemonId
}

case class Move(pokemon: PokemonId, move: String, target: PokemonId, hit: Boolean) extends MajorAction with PokemonAction
case class Switch(pokemon: PokemonId, details: PokemonDetails, status: PokemonStatus) extends MajorAction with PokemonAction
case class Drag(pokemon: PokemonId, details: PokemonDetails, status: PokemonStatus) extends MajorAction with PokemonAction
case class DetailsChange(pokemon: PokemonId, details: PokemonDetails) extends MajorAction with PokemonAction
case class FormeChange(pokemon: PokemonId, species: String, status: PokemonStatus) extends MajorAction with PokemonAction
case class Replace(pokemon: PokemonId, details: PokemonDetails, status: PokemonStatus) extends MajorAction with PokemonAction
case class Swap(pokemon: PokemonId, position: Int) extends MajorAction with PokemonAction
case class Cant(pokemon: PokemonId, reason: String, move: Option[String]) extends MajorAction with PokemonAction
case class Faint(pokemon: PokemonId) extends MajorAction with PokemonAction

sealed trait MinorAction extends BattleMessage
case class Weather(weather: String /* WeatherStatus */, upkeep: Boolean) extends MinorAction
case class FieldConditionStart(condition: String) extends MinorAction
case class FieldConditionStop(condition: String) extends MinorAction
case class SideConditionStart(side: Player, condition: String) extends MinorAction
case class SideConditionStop(side: Player, condition: String) extends MinorAction
case class ActivateEffect(effect: String) extends MinorAction
case class Hint(message: String) extends MinorAction
case object TripleCenter extends MinorAction
case class MiscMessage(msg: String) extends MinorAction

case class Failed(pokemon: PokemonId, action: String, detail: Option[String] = None) extends MinorAction with PokemonAction
case class Damage(pokemon: PokemonId, status: PokemonStatus) extends MinorAction with PokemonAction
case class Heal(pokemon: PokemonId, status: PokemonStatus) extends MinorAction with PokemonAction
case class Status(pokemon: PokemonId, effect: StatusEffect) extends MinorAction with PokemonAction
case class Cure(pokemon: PokemonId, effect: StatusEffect) extends MinorAction with PokemonAction
case class CureTeam(pokemon: PokemonId) extends MinorAction with PokemonAction
case class Boost(pokemon: PokemonId, stat: Stat, amount: Int, details: Option[String]) extends MinorAction with PokemonAction
case class Unboost(pokemon: PokemonId, stat: Stat, amount: Int, details: Option[String]) extends MinorAction with PokemonAction
case class Crit(pokemon: PokemonId) extends MinorAction with PokemonAction
case class SuperEffective(pokemon: PokemonId) extends MinorAction with PokemonAction
case class Resisted(pokemon: PokemonId) extends MinorAction with PokemonAction
case class Immune(pokemon: PokemonId) extends MinorAction with PokemonAction
case class Item(pokemon: PokemonId, item: String) extends MinorAction with PokemonAction
case class EndItem(pokemon: PokemonId, item: String) extends MinorAction with PokemonAction
case class Ability(pokemon: PokemonId, ability: String) extends MinorAction with PokemonAction
case class EndAbility(pokemon: PokemonId) extends MinorAction with PokemonAction
case class Transform(pokemon: PokemonId, species: String) extends MinorAction with PokemonAction
case class MegaEvolve(pokemon: PokemonId, species: String, stone: String) extends MinorAction with PokemonAction
case class SetBoost(pokemon: PokemonId, stat: Stat, amount: Int, details: Option[String]) extends MinorAction with PokemonAction // Not Documented
case class ClearNegativeBoost(pokemon: PokemonId) extends MinorAction with PokemonAction // Not Documented
case class Zpower(pokemon: PokemonId) extends MinorAction with PokemonAction // Not Documented
case class Activate(pokemon: PokemonId, status: String) extends MinorAction with PokemonAction // Not Documented
