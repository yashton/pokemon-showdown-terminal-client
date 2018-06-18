package science.snelgrove.showdown.protocol

sealed trait RoomType
case object ChatType extends RoomType
case object GlobalType extends RoomType
case object BattleType extends RoomType

case class User(name: String, rank: Option[Char])

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

case class PokemonDetails(species: String, forme: Option[String],
  shiny: Boolean, gender: Gender, level: Int)

case class StatusEffect(description: String)
// sealed trait StatusEffect
// case object Unaffected extends StatusEffect

// TODO enum

sealed trait PokemonStatus
case class HpStatus(hpCurrent: Int, hpMax: Int, status: Option[StatusEffect]) extends PokemonStatus
case class PercentStatus(hpCurrent: Int, status: Option[StatusEffect]) extends PokemonStatus
case class BinaryStatus(hpCurrent: Int, status: Option[StatusEffect]) extends PokemonStatus
case object FaintedStatus extends PokemonStatus

sealed trait Stat
case object Atk extends Stat
case object Def extends Stat
case object SpA extends Stat
case object SpD extends Stat
case object Spe extends Stat

sealed trait WeatherStatus
// TODO enum
case object NoWeather extends WeatherStatus
