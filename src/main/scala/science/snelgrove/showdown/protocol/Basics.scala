package science.snelgrove.showdown.protocol

sealed trait RoomType
case object ChatType extends RoomType
case object GlobalType extends RoomType
case object BattleType extends RoomType

case class User(name: String, rank: Option[Char] = None)

sealed trait Player
case object PlayerOne extends Player
case object PlayerTwo extends Player

sealed trait GameType
case object Singles extends GameType
case object Doubles extends GameType
case object Triples extends GameType

case class PokemonId(player: Player, position: Option[String], name: String)

sealed trait Gender
case object Female extends Gender
case object Genderless extends Gender
case object Male extends Gender

case class PokemonDetails(species: String, forme: Option[String] = None,
  shiny: Boolean = false, gender: Gender = Genderless, level: Int = 100)

sealed trait StatusEffect
case object Unaffected extends StatusEffect
case object Poisoned extends StatusEffect
case object BadlyPoisoned extends StatusEffect
case object Paralyzed extends StatusEffect
case object Sleep extends StatusEffect

sealed trait PokemonStatus
case class HpStatus(hpCurrent: Int, hpMax: Int, status: Option[StatusEffect]) extends PokemonStatus
case class PercentStatus(hpCurrent: Int, status: Option[StatusEffect]) extends PokemonStatus
case class BinaryStatus(hpCurrent: Int, status: Option[StatusEffect]) extends PokemonStatus
case object FaintedStatus extends PokemonStatus

sealed trait Stat
case object HP extends Stat
case object Atk extends Stat
case object Def extends Stat
case object SpA extends Stat
case object SpD extends Stat
case object Spe extends Stat
case object Acc extends Stat
case object Eva extends Stat

sealed trait WeatherStatus
case object Clear extends WeatherStatus
case object HarshSunlight extends WeatherStatus
case object ExtremelyHarshSunlight extends WeatherStatus
case object Rain extends WeatherStatus
case object HeavyRain extends WeatherStatus
case object Hail extends WeatherStatus
case object Sandstorm extends WeatherStatus
case object MysteriousAirCurrent  extends WeatherStatus

sealed trait PokemonType
case object NormalType extends PokemonType
case object FireType extends PokemonType
case object FightingType extends PokemonType
case object WaterType extends PokemonType
case object FlyingType extends PokemonType
case object GrassType extends PokemonType
case object PoisonType extends PokemonType
case object ElectricType extends PokemonType
case object GroundType extends PokemonType
case object PsychicType extends PokemonType
case object RockType extends PokemonType
case object IceType extends PokemonType
case object BugType extends PokemonType
case object DragonType extends PokemonType
case object GhostType extends PokemonType
case object DarkType extends PokemonType
case object SteelType extends PokemonType
case object FairyType extends PokemonType
