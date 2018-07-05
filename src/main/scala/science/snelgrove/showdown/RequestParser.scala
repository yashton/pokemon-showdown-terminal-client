package science.snelgrove.showdown

import play.api.libs.json._
import science.snelgrove.showdown.protocol._

case class RequestMove (
  move: String,
  id: String,
  pp: Int,
  maxpp: Int,
  target: String,
  disabled: Boolean
)

case class RequestActive(
  moves: Seq[RequestMove]
)

case class RequestSide(
  name: String,
  id: Player,
  pokemon: Seq[RequestPokemon]
)

case class RequestPokemon(
  ident: PokemonId,
  details: PokemonDetails,
  condition: PokemonStatus,
  active: Boolean,
  stats: Map[Stat, Int],
  moves: Seq[String],
  baseAbility: String,
  item: String,
  pokeball: String,
  ability: String
)

case class BattleRequest(
  active: Option[Seq[RequestActive]],
  side: RequestSide,
  rqid: Int,
)

object RequestParser {
  private def stringReads[T](parse: String => T): Reads[T] = new Reads[T] {
    def reads(json: JsValue): JsResult[T] = json.validate[String].map(parse)
  }

  implicit val requestId: Reads[PokemonId] =
    stringReads(PokemonParser.parse)
  implicit val requestDetails: Reads[PokemonDetails] =
    stringReads(DetailsParser.parse)
  implicit val requestPlayer: Reads[Player] =
    stringReads(GeneralParser.parsePlayer)
  implicit val requestStatus: Reads[PokemonStatus] =
    stringReads(StatusParser.parse)
  implicit val requestStat: Reads[Stat] =
    stringReads(GeneralParser.parseStat)

  implicit val mapReads: Reads[Map[Stat, Int]] = new Reads[Map[Stat, Int]] {
    def reads(jv: JsValue): JsResult[Map[Stat, Int]] =
      for {
        m <- jv.validate[Map[String, Int]]
      } yield for {
        (k, v) <- m
      } yield GeneralParser.parseStat(k) -> v
  }
  implicit val requestPokemon = Json.reads[RequestPokemon]
  implicit val requestSide: Reads[RequestSide] = Json.reads[RequestSide]
  implicit val requestMove = Json.reads[RequestMove]
  implicit val requestActive = Json.reads[RequestActive]
  implicit val requestReads = Json.reads[BattleRequest]
}
