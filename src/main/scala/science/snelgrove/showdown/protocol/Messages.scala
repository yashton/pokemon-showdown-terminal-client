package science.snelgrove.showdown.protocol
import java.time.Instant
import play.api.libs.json.JsValue

sealed trait Target
case class Room(room: String, msgs: Seq[ShowdownMessage]) extends Target
case class Global(msgs: Seq[ShowdownMessage]) extends Target

sealed trait ShowdownMessage
case object Empty extends ShowdownMessage

sealed trait RoomMessage extends ShowdownMessage
case class Log(text: String) extends RoomMessage
case class Chat(user: User, text: String, timestamp: Option[Instant]) extends RoomMessage
case class Join(user: User) extends RoomMessage
case class Leave(user: User) extends RoomMessage
case class Name(user: User, oldId: String) extends RoomMessage
case class RoomInit(roomType: RoomType) extends RoomMessage
case class RoomUsers(users: Seq[User]) extends RoomMessage
case class DirectMessage(msg: String) extends RoomMessage
case class HtmlMessage(body: String) extends RoomMessage
case class DynamicHtmlMessage(name: String, body: String) extends RoomMessage
case class DynamicHtmlMessageUpdate(name: String, body: String) extends RoomMessage
case class Timestamp(time: Instant) extends RoomMessage
case class BattleStart(room: String, playerOne: User, playerTwo: User) extends RoomMessage
case object RoomDeinit extends RoomMessage // Not Documented
case class RawMessage(body: String) extends RoomMessage // Not Documented

sealed trait GlobalMessage extends ShowdownMessage
case class UnknownMessage(msg: String) extends GlobalMessage
case class Popup(text: String) extends GlobalMessage
case class PrivateMessage(sender: User, receiver: User, text: String) extends GlobalMessage
case class UserCount(count: Int) extends GlobalMessage
case class NameTaken(username: String, reason: String) extends GlobalMessage
case class LoginToken(token: String) extends GlobalMessage
case class UpdateUser(username: String, named: Boolean, avatar: String) extends GlobalMessage
case class Format(name: String, random: Boolean, searchOnly: Boolean, challengeOnly: Boolean)
case class Formats(format: Map[String, Seq[Format]]) extends GlobalMessage
case class UpdateSearch(body: JsValue) extends GlobalMessage
case class UpdateChallenges(body: JsValue) extends GlobalMessage
case class QueryResponse(queryType: String, body: JsValue) extends GlobalMessage

sealed trait BattleMessage extends ShowdownMessage
case class PlayerConnect(player: Player, user: User, avatar: String) extends BattleMessage
case class BattleGameType(gameType: GameType) extends BattleMessage
case class BattleGeneration(gen: Int) extends BattleMessage
case object BattleRated extends BattleMessage
case class Rule(clause: String) extends BattleMessage
case class Tier(tier: String) extends BattleMessage
case object BattleStart extends BattleMessage
case object TeamPreview extends BattleMessage
case class PreviewPokemon(player: Player, details: PokemonDetails, item: String) extends BattleMessage
case object ClearPreview extends BattleMessage
case class Request(body: JsValue) extends BattleMessage
case class BattleTimer(active: Boolean, message: String) extends BattleMessage
case class Turn(number: Int) extends BattleMessage
case class Win(user: User) extends BattleMessage
case object Tie extends BattleMessage
case class TeamSize(player: Player, size: Int) extends BattleMessage // Not Documented
case object Upkeep extends BattleMessage // Not Documented

sealed trait MajorAction extends BattleMessage
case class Move(pokemon: PokemonId, move: String, target: Option[PokemonId], hit: Boolean) extends MajorAction
case class Switch(pokemon: PokemonId, details: PokemonDetails, status: PokemonStatus) extends MajorAction
case class Drag(pokemon: PokemonId, details: PokemonDetails, status: PokemonStatus) extends MajorAction
case class DetailsChange(pokemon: PokemonId, details: PokemonDetails, status: PokemonStatus) extends MajorAction
case class FormeChange(pokemon: PokemonId, species: String, status: PokemonStatus) extends MajorAction
case class Replace(pokemon: PokemonId, details: PokemonDetails, status: PokemonStatus) extends MajorAction
case class Swap(pokemon: PokemonId, position: Int) extends MajorAction
case class Cant(pokemon: PokemonId, reason: String, move: Option[String]) extends MajorAction
case class Faint(pokemon: PokemonId) extends MajorAction

sealed trait MinorAction extends BattleMessage
case class Failed(pokemon: PokemonId, action: String) extends MinorAction
case class Damage(pokemon: PokemonId, status: PokemonStatus) extends MinorAction
case class Heal(pokemon: PokemonId, status: PokemonStatus) extends MinorAction
case class Status(pokemon: PokemonId, effect: StatusEffect) extends MinorAction
case class Cure(pokemon: PokemonId, effect: StatusEffect) extends MinorAction
case class CureTeam(pokemon: PokemonId) extends MinorAction
case class Boost(pokemon: PokemonId, stat: Stat, amount: Int, details: Option[String]) extends MinorAction
case class Unboost(pokemon: PokemonId, stat: Stat, amount: Int, details: Option[String]) extends MinorAction
case class Weather(weather: String, upkeep: Boolean) extends MinorAction
case class FieldConditionStart(condition: String) extends MinorAction
case class FieldConditionStop(condition: String) extends MinorAction
case class SideConditionStart(side: Player, condition: String) extends MinorAction
case class SideConditionStop(side: Player, condition: String) extends MinorAction
case class Crit(pokemon: PokemonId) extends MinorAction
case class SuperEffective(pokemon: PokemonId) extends MinorAction
case class Resisted(pokemon: PokemonId) extends MinorAction
case class Immune(pokemon: PokemonId) extends MinorAction
case class Item(pokemon: PokemonId, item: String) extends MinorAction
case class EndItem(pokemon: PokemonId, item: String) extends MinorAction
case class Ability(pokemon: PokemonId, ability: String) extends MinorAction
case class EndAbility(pokemon: PokemonId) extends MinorAction
case class Transform(pokemon: PokemonId, species: String) extends MinorAction
case class MegaEvolve(pokemon: PokemonId, stone: String) extends MinorAction
case class ActivateEffect(effect: String) extends MinorAction
case class Hint(message: String) extends MinorAction
case object TripleCenter extends MinorAction
case class MiscMessage(msg: String) extends MinorAction
case class EndEffect(pokemon: PokemonId, effect: String) extends MinorAction // Not Documented
case class SetBoost(pokemon: PokemonId, stat: Stat, amount: Int, details: Option[String]) extends MinorAction // Not Documented
case class Zpower(pokemon:PokemonId) extends MinorAction // Not Documented