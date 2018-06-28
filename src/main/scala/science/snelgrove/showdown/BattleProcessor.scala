package science.snelgrove.showdown

import akka.event.Logging
import akka.actor.{ Actor, Stash }
import monocle.Lens
import monocle.function._
import monocle.macros.GenLens
import science.snelgrove.showdown.protocol._

class BattleProcessor extends Actor with Stash {
  val log = Logging(context.system, this)
  var state = GameState.defaults
  var preview = false

  def applyState(f: GameState => GameState): Unit = {
    state = f(state)
    context.parent ! state
  }

  def receive = (previewUpdates andThen applyState) orElse setup

  import GameState._
  import GameConfig._
  import Team._
  import PokemonState._
  import FieldState._

  def setup: PartialFunction[Any, Unit] = {
    case StatePoke =>
      sender ! state
    case TeamPreview =>
      preview = true
    case BattleStart =>
      unstashAll()
      context.become(running)
    case Request(body) => //todo
    case _ => stash()
  }

  def previewUpdates: PartialFunction[Any, GameState => GameState] = {
    case PlayerConnect(PlayerOne, u, _) =>
      (teamOne composeLens player).set(u)
    case PlayerConnect(PlayerTwo, u, _) =>
      (teamOne composeLens player).set(u)
    case BattleGameType(t) =>
      (config composeLens gameType).set(t)
    case BattleGeneration(gen) =>
      (config composeLens generation).set(gen)
    case BattleRated =>
      (config composeLens rated).set(true)
    case Rule(clause) =>
      (config composeLens clauses).modify(_ :+ clause)
    case Tier(t) =>
      (config composeLens tier).set(t)
    case PreviewPokemon(p, d, item) =>
      val pokemon = PokemonState(d.species, d.forme, None,
        d.gender, d.shiny, d.level, Some("item").filter(_ => item),
        PercentStatus(100, None), Map(), Map(), Map())
      val side = p match { case PlayerOne => teamOne; case PlayerTwo => teamTwo }
        (side composeLens team).modify(_ :+ pokemon)
    case ClearPreview =>
      (teamOne composeLens team).set(Seq())
      (teamTwo composeLens team).set(Seq())
    case TeamSize(PlayerOne, s) =>
      (teamOne composeLens size).set(s)
    case TeamSize(PlayerTwo, s) =>
      (teamTwo composeLens size).set(s)
  }

  def finished(result: TerminalState): PartialFunction[Any, Unit] = {
    case _ =>
  }

  def running: PartialFunction[Any, Unit] = game orElse ((gameGlobal orElse fieldState) andThen applyState)

  def game: PartialFunction[Any, Unit] = {
    case StatePoke => sender ! state
    case r: Win => context.become(finished(r))
    case Tie => context.become(finished(Tie))
  }

  def gameGlobal: PartialFunction[Any, GameState => GameState] = {
    case BattleTimer(active, message) =>
      timer.set(Timer(active, message))
    case Turn(number) =>
      turn.set(number)
    case Request(body) => ???
  }

  def fieldState: PartialFunction[Any, GameState => GameState] = {
    case Upkeep => f => f // dunno what this is
    case Weather(t, true) => s =>
      val x = turn.get(s)
      (field composeLens weather).set(WeatherState(x, t))(s)
    case FieldConditionStart(condition) =>
      (field composeLens fieldConditions).modify(_ + condition)
    case FieldConditionStop(condition) =>
      (field composeLens fieldConditions).modify(_ - condition)
    case SideConditionStart(PlayerOne, condition) =>
      (teamOne composeLens conditions).modify(_ + condition)
    case SideConditionStart(PlayerTwo, condition) =>
      (teamTwo composeLens conditions).modify(_ + condition)
    case SideConditionStop(PlayerOne, condition) =>
      (teamOne composeLens conditions).modify(_ - condition)
    case SideConditionStop(PlayerTwo, condition) =>
      (teamTwo composeLens conditions).modify(_ - condition)
    case ActivateEffect(effect) => f => f
    case TripleCenter => f => f
    case p: PokemonAction =>
      val lens = p.pokemon.player match {
        case PlayerOne => teamOne
        case PlayerTwo => teamTwo
      }
      lens.modify(pokemon(p))
  }

  def pokemon: Function[PokemonAction, Team => Team] = {
    case Move(pokemon, move, target, hit) => ???
      // Add move to pokemon if not seen before.
      // update use count.
    case Switch(pokemon, details, status) => ???
      // update active
    case Drag(pokemon, details, status) => ???
      // update active
    case DetailsChange(pokemon, details) => ???
    case FormeChange(pokemon, species, status) => ???
    case Replace(pokemon, details, status) => ???
    case Swap(pokemon, position) => ???
      // update active
    case Cant(pokemon, reason, move) => ???
    case Faint(pokemon) => ???

    case Failed(pokemon, action, details) => ???
    case Damage(pokemon, status) => ???
      // Update status
    case Heal(pokemon, status) => ???
    // Update status
    case Status(pokemon, effect) => ???
            // Update status
    case Cure(pokemon, effect) => ???
            // Update status
    case CureTeam(pokemon) => ???
            // Update status for team
    case Boost(pokemon, stat, amount, details) => ???
            // Update boosts
    case Unboost(pokemon, stat, amount, details) => ???
      // update boosts

    case Crit(pokemon) => ???
    case SuperEffective(pokemon) => ???
    case Resisted(pokemon) => ???
    case Immune(pokemon) => ???
    case Item(pokemon, item) => ???
      // update item
    case EndItem(pokemon, item) => ???
      // remove item
    case Ability(pokemon, ability) => ???
      // update ability
    case EndAbility(pokemon) => ???
      // update ability
    case Transform(pokemon, species) => ???
      // update species
    case MegaEvolve(pokemon, species, stone) => ???
      // update species
      // update item
    case SetBoost(pokemon, stat, amount, details) => ???
    //update boosts
    case Zpower(pokemon) =>
      zpowerUsed.set(true)
  }
}

case class GameState(
  val turn: Int,
  val timer: Timer,
  val config: GameConfig,
  val teamOne: Team,
  val teamTwo: Team,
  val field: FieldState,
  val history: Seq[BattleMessage]
)

object GameState {
  val timer = GenLens[GameState](_.timer)
  val turn = GenLens[GameState](_.turn)
  val teamOne = GenLens[GameState](_.teamOne)
  val teamTwo = GenLens[GameState](_.teamTwo)
  val field = GenLens[GameState](_.field)
  val config = GenLens[GameState](_.config)
  val history = GenLens[GameState](_.history)
  val defaults = GameState(
    turn = 0,
    timer = Timer(false, "Timer off"),
    config = GameConfig(
      gameType = Singles,
      generation = 7,
      rated = false,
      tier = "ou",
      clauses = Seq()
    ),
    teamOne = Team("player one loading", -1, 0, false, Seq(), Set()),
    teamTwo = Team("player two loading", -1, 0, false, Seq(), Set()),
    history = Seq(),
    field = FieldState(WeatherState(0, "Clear"), Set())
  )
}

case class GameConfig(
  val gameType: GameType,
  val generation: Int,
  val rated: Boolean,
  val tier: String,
  val clauses: Seq[String]
)

object GameConfig {
  val gameType = GenLens[GameConfig](_.gameType)
  val generation = GenLens[GameConfig](_.generation)
  val rated = GenLens[GameConfig](_.rated)
  val tier = GenLens[GameConfig](_.tier)
  val clauses = GenLens[GameConfig](_.clauses)
}

case class WeatherState(turn: Int, weather: String /* WeatherStatus */)

case class FieldState(
  weather: WeatherState,
  conditions: Set[String]
)

object FieldState {
  val weather = GenLens[FieldState](_.weather)
  val fieldConditions = GenLens[FieldState](_.conditions)
}

case class Team (
  val player: String,
  val active: Int,
  val size: Int,
  val zpowerUsed: Boolean,
  val team: Seq[PokemonState],
  val conditions: Set[String]
)

object Team {
  val player = GenLens[Team](_.player)
  val active = GenLens[Team](_.active)
  val size = GenLens[Team](_.size)
  val zpowerUsed = GenLens[Team](_.zpowerUsed)
  val team: Lens[Team, Seq[PokemonState]] = GenLens[Team](_.team)
  val conditions = GenLens[Team](_.conditions)
}

object PokemonState {
  val species = GenLens[PokemonState](_.species)
  val forme = GenLens[PokemonState](_.forme)
  val name = GenLens[PokemonState](_.name)
  val gender = GenLens[PokemonState](_.gender)
  val shiny = GenLens[PokemonState](_.shiny)
  val level = GenLens[PokemonState](_.level)
  val item = GenLens[PokemonState](_.item)
  val status = GenLens[PokemonState](_.status)
  val moves = GenLens[PokemonState](_.moves)
  val boosts = GenLens[PokemonState](_.boosts)
  val stats = GenLens[PokemonState](_.stats)
}

case class PokemonState(
  val species: String,
  val forme: Option[String],
  val name: Option[String],
  val gender: Gender,
  val shiny: Boolean,
  val level: Int,
  val item: Option[String],
  val status: PokemonStatus,
  val moves: Map[String, PokemonMove],
  val boosts: Map[Stat, Int],
  val stats:  Map[Stat, Int]
)

case class PokemonMove (
  val name: String,
  val used: Int
)

case class Species (
  val name: String,
  val firstType: PokemonType,
  val secondType: PokemonType
)

case class Timer(active: Boolean, msg: String)
