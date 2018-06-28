package science.snelgrove.showdown

import java.time.Instant
import org.scalatest.Matchers
import org.scalatest.FunSpec
import science.snelgrove.showdown.protocol._
import science.snelgrove.showdown.protocol.{ PokemonId => PID, PlayerOne => P1, PlayerTwo => P2 }

class MessageParserTest extends FunSpec with Matchers {
  def parses(cases: (String, ShowdownMessage)*): Unit = {
    for {
      (in, out) <- cases
    } MessageParser.parseMessage(in) should be (out)
  }

  describe("Parsing basic messages") {
    it("should parse the varieties of chat.") {
      parses(
        "|" -> Empty,
        "" -> Empty,
        "anything" -> Log("anything"),
        "|c| pikachu|hello world" -> Chat(User("pikachu"), "hello world"),
        "|c| pikachu|this|that" -> Chat(User("pikachu"), "this|that"),
        "|chat| pikachu|hello world" -> Chat(User("pikachu"), "hello world"),
        "|c|*pikachu|hello world" -> Chat(User("pikachu", Some('*')), "hello world"),
        "|c:|1530073547| pikachu|hello world" ->
          Chat(User("pikachu"), "hello world",
            Some(Instant.parse("2018-06-27T04:25:47Z"))),
        "|:|1530073547" -> Timestamp(Instant.parse("2018-06-27T04:25:47Z")),
        "|b|battle-gen7-randombattle-768096824| mudkip| torchic" ->
          BattleStart("battle-gen7-randombattle-768096824", User("mudkip"), User("torchic")),
        "|battle|battle-gen7-randombattle-768096834| clefairy| jigglypuff" ->
          BattleStart("battle-gen7-randombattle-768096834", User("clefairy"), User("jigglypuff")),
        "|B|battle-gen7-randombattle-768096866| chansey| magmar" ->
          BattleStart("battle-gen7-randombattle-768096866", User("chansey"), User("magmar")),
        "|popup|Hey, listen!" ->
          Popup("Hey, listen!"),
        "|pm|*Fennekin| Froakie|Let's battle!" ->
          PrivateMessage(User("Fennekin", Some('*')), User("Froakie"), "Let's battle!")
      )
    }
    it("should parse html messages") (pending)
    // TODO HtmlMessage, DynamicHtmlMessage, DynamicHtmlMessageUpdate, RawMessage
    it("should parse basic user comings and goings") {
      parses(
        "|j|*pikachu" -> Join(User("pikachu", Some('*'))),
        "|J| pikachu" -> Join(User("pikachu")),
        "|join| pikachu" -> Join(User("pikachu")),
        "|users| pikachu,*bulbasaur, squirtle, charmander" ->
          RoomUsers(Seq(
            User("pikachu"), User("bulbasaur", Some('*')),
            User("squirtle"), User("charmander"))),
        "|l|*pikachu" -> Leave(User("pikachu", Some('*'))),
        "|L| pikachu" -> Leave(User("pikachu")),
        "|leave| pikachu" -> Leave(User("pikachu")),
        "|name| pikachu|raichu" -> Name(User("pikachu"), "raichu"),
        "|N| squirtle|wartortle" -> Name(User("squirtle"), "wartortle"),
        "|n| wartortle|blastoise" -> Name(User("wartortle"), "blastoise")
      )
    }

    it("should parse room setup") {
      parses(
        "|init|battle" -> RoomInit(BattleType),
        "|init|chat" -> RoomInit(ChatType),
        "|deinit" -> RoomDeinit,
        "|title|Over Used" -> RoomTitle("Over Used")
      )
    }
    it("should parse some global messages") {
      parses(
        "|challstr|4|8d0b48ada" -> LoginChallenge("4|8d0b48ada")
        // todo UserCount NameTaken UpdateUser Formats
        // UpdateSearch UpdateChallenges QueryResponse Seed UndocumentedStart
      )
    }
  }
  describe("parsing battle messages") {
    it("should parse battle setup") {
      parses(
        "|player|p1|absol|32" -> PlayerConnect(P1, "absol", "32"),
        "|gametype|singles" -> BattleGameType(Singles),
        "|gen|7" -> BattleGeneration(7),
        "|tier|[Gen 7] OU" -> Tier("[Gen 7] OU"),
        "|rated|Tournament battle" -> BattleRated("Tournament battle"),
        "|seed|" -> Seed,
        "|rule|Sleep Clause Mod: Limit one foe put to sleep" ->
          Rule("Sleep Clause Mod: Limit one foe put to sleep"),
        "|start" -> BattleStart,
        "|clearpoke" -> ClearPreview,
        "|poke|p1|Zygarde|item" ->
          PreviewPokemon(P1, PokemonDetails("Zygarde"), item = true),
        "|poke|p2|Gyarados, F|item" ->
          PreviewPokemon(P2, PokemonDetails("Gyarados", gender = Female), item = true),
        "|poke|p2|Mewtwo" ->
          PreviewPokemon(P2, PokemonDetails("Mewtwo"), item = false),
        "|teampreview" -> TeamPreview,
        "|teamsize|p1|6" -> TeamSize(P1, 6)
      )
    }
    it("should parse turn state.") {
      parses(
        "|inactive|Spinda has 135 seconds left." ->
          BattleTimer(true, "Spinda has 135 seconds left."),
        "|inactiveoff|Battle timer disabled." ->
          BattleTimer(false, "Battle timer disabled."),
        "|turn|13" -> Turn(13),
        "|tie" -> Tie,
        "|win|Pangoro" -> Win("Pangoro")
      )
    }
    it("should parse a request") (pending)
    it("should parse a pokemon details.") {
      val cases = Map(
        "Ratata" -> PokemonDetails("Ratata"),
        "Mr. Mime, F, shiny" -> PokemonDetails("Mr. Mime", gender = Female, shiny = true),
        "Teddiursa, M" -> PokemonDetails("Teddiursa", gender = Male),
        "Sableye, shiny" -> PokemonDetails("Sableye", shiny = true),
        "Zigzagoon, F, shiny" -> PokemonDetails("Zigzagoon", shiny = true, gender = Female),
        "Ghastly, L36" -> PokemonDetails("Ghastly", level = 36),
        "Charizard, M, L23, shiny" -> PokemonDetails("Charizard", level = 23, gender = Male, shiny = true),
        "Rotom-Wash, shiny" -> PokemonDetails("Rotom", forme = Some("Wash"), shiny = true),
        "Deoxys-*, L55" -> PokemonDetails("Deoxys", forme = Some("*"), level = 55),
        "Kommo-o, L77" -> PokemonDetails("Kommo-o", level = 77),
        "Porygon-Z" -> PokemonDetails("Porygon-Z")
      )
      for {
        (in, out) <- cases
      } DetailsParser.parse(in) should be (out)
    }
    it("should parse a pokemon id.") {
      PokemonParser.parse("p1a: Blue") should be
      (PID(P1, Some("a"), "The Dude"))

      PokemonParser.parse("p1a: The Dude") should be
      (PID(P1, Some("a"), "The Dude"))

      PokemonParser.parse("p1: The Dude") should be
      (PID(P1, None, "The Dude"))
    }
    describe("Parsing major actions") {
      val id1Str = "p1a: The Dude"
      val id1 = PID(P1, Some("a"), "The Dude")
      val id2Str = "p2a: Rotom"
      val id2 = PID(P2, Some("a"), "Rotom")

      val details = PokemonDetails("Ratata", gender = Male)
      val detailsStr = "Ratata, M"

      it("should parse a move action.") {
        parses(
          s"|move|$id1Str|U-turn|$id2Str" -> Move(id1, "U-turn", id2, true),
          s"|move|$id1Str|Tackle|$id2Str" -> Move(id1, "Tackle", id2, true),
          s"|move|$id1Str|Solar Beam|$id2Str|[miss]" -> Move(id1, "Solar Beam", id2, false),
          "|move|p2a: Gyarados|Supersonic Skystrike|p1a: Zygarde|[zeffect]" -> Move(PID(P2, Some("a"), "Gyarados"), "Supersonic Skystrike", PID(P1, Some("a"), "Zygarde"), true),
          "|move|p1a: Virizion|Stone Edge|p2a: Tapu Koko|[miss]" -> Move(PID(P1, Some("a"), "Virizion"), "Stone Edge", PID(P2, Some("a"), "Tapu Koko"), false),
          "|move|p2a: Sceptile|Outrage|p1a: Steelix|[from]lockedmove" -> Move(PID(P2, Some("a"), "Sceptile"), "Outrage", PID(P1, Some("a"), "Steelix"), true)
        )
      }

      it("should parse a switch action.") {
        parses(
          s"|switch|$id1Str|$detailsStr|67/100" ->
            Switch(id1, details, PercentStatus(67, None)),
          s"|switch|$id1Str|$detailsStr|67/100 tox" ->
            Switch(id1, details, PercentStatus(67, Some(BadlyPoisoned))),

          s"|drag|$id1Str|$detailsStr|67/100" ->
            Drag(id1, details, PercentStatus(67, None))
        )
      }
      it("should parse details change") {
        parses(
          "|detailschange|p2a: Aerodactyl|Aerodactyl-Mega" ->
            DetailsChange(PID(P2, Some("a"), "Aerodactyl"),
              PokemonDetails("Aerodactyl", forme = Some("Mega"))),
          "|detailschange|p1a: Steelix|Steelix-Mega, L79, M" ->
            DetailsChange(PID(P1, Some("a"), "Steelix"),
              PokemonDetails("Steelix", forme = Some("Mega"), gender = Male, level = 79))
        )
      }
      it("should parse forme change") {
        parses(
          "|-formechange|p2a: Castform|Castform-Sunny|100/100" ->
            FormeChange(PID(P2, Some("a"), "Castform"),
              "Castform-Sunny",
              PercentStatus(100, None)),
        )
      }
      it("should parse a transform/replace") {
        parses(
          "|replace|p2a: Whismur|Zoroark|100/100" ->
            Replace(PID(P2, Some("a"), "Whismur"),
              PokemonDetails("Zoroark"),
              PercentStatus(100, None)),
          "|-transform|p2a: Ditto|Dragonite" ->
            Transform(PID(P2, Some("a"), "Ditto"), "Dragonite"),
          "|-mega|p1a: FutakuchiOnna|Mawile|Mawilite" ->
            MegaEvolve(PID(P1, Some("a"), "FutakuchiOnna"), "Mawile", "Mawilite"),
          "|-megaevolve|p1a: FutakuchiOnna|Mawile|Mawilite" ->
            MegaEvolve(PID(P1, Some("a"), "FutakuchiOnna"), "Mawile", "Mawilite")
        )
      }
      it("should parse a swap") {
        parses(
          "|swap|p2b: Ribombee|0" ->
            Swap(PID(P2, Some("b"), "Ribombee"), 0)
        )
      }
      it("should parse a faint") {
        parses(
          "|faint|p2a: Plusle" -> Faint(PID(P2, Some("a"), "Plusle"))
        )
      }
      it("should parse a failure message") {
        parses (
          "|cant|p1a: Wailmer|It is paralyzed! It can't move!" ->
            Cant(PID(P1, Some("a"), "Wailmer"),
              "It is paralyzed! It can't move!", None),
          "|cant|p1b: Wailord|No valid target|Hyper Beam" ->
            Cant(PID(P1, Some("b"), "Wailord"),
              "No valid target", Some("Hyper Beam"))
        )
      }
      it("should parse an upkeep message") {
        parses (
          "|upkeep" -> Upkeep
        )
      }
    }
    describe("Parsing minor actions.") {
      it ("should parse an action failure message") {
        parses (
          "|-fail|p1a: Mudkip|tox" ->
            Failed(PID(P1, Some("a"), "Mudkip"), "tox"),
          "|-fail|p1a: Bisharp|Giga Impact" ->
            Failed(PID(P1, Some("a"), "Bisharp"), "Giga Impact"),
          "|-fail|p1a: Skitty|unboost|atk" ->
            Failed(PID(P1, Some("a"), "Skitty"), "unboost", Some("atk")),
          "|-fail|p1a: Gengar|unboost" ->
            Failed(PID(P1, Some("a"), "Gengar"), "unboost")
        )
      }
      it ("should parse damage") {
        parses(
          "|-damage|p1a: Delibird|118/209 tox|[from] psn" ->
            Damage(PID(P1, Some("a"), "Delibird"),
              HpStatus(118, 209, Some(BadlyPoisoned))),
          "|-damage|p2a: Tapu Koko|0 fnt" ->
            Damage(PID(P2, Some("a"), "Tapu Koko"), FaintedStatus),
          "|-damage|p2a: Guzzlord|50/100" ->
            Damage(PID(P2, Some("a"), "Guzzlord"), PercentStatus(50, None)),
          "|-damage|p1a: Flareon|207/243|[from] Recoil" ->
            Damage(PID(P1, Some("a"), "Flareon"), HpStatus(207, 243, None)),
          "|-damage|p2a: Jolteon|19/100|[from] item: Life Orb" ->
            Damage(PID(P2, Some("a"), "Jolteon"), PercentStatus(19, None)),
        )
      }
      it("should parse some heal messages") {
        parses(
          "|-heal|p2a: Snorlax|78/100|[from] item: Leftovers" ->
            Heal(PID(P2, Some("a"), "Snorlax"), PercentStatus(78, None)),
          "|-heal|p1a: Shiftry|78/100" ->
            Heal(PID(P1, Some("a"), "Shiftry"), PercentStatus(78, None)),
        )
      }
      it("should parse status message") {
        parses (
          "|-status|p2a: Girafarig|tox" ->
            Status(PID(P2, Some("a"), "Girafarig"), BadlyPoisoned),
          "|-curestatus|p2a: Empoleon|tox" ->
            Cure(PID(P2, Some("a"), "Empoleon"), BadlyPoisoned),
          "|-cureteam|p1a: Blissey" ->
            CureTeam(PID(P1, Some("a"), "Blissey"))
        )
      }
      it("should parse stat changes") {
        parses (
          "|-boost|p2a: Ditto|atk|1" ->
            Boost(PID(P2, Some("a"), "Ditto"), Atk, 1, None),
          "|-unboost|p2a: Slaking|spd|1" ->
            Unboost(PID(P2, Some("a"), "Slaking"), SpD, 1, None),
          "|-unboost|p1a: Samurott|evasion|2" ->
            Unboost(PID(P1, Some("a"), "Samurott"), Eva, 2, None),
          "|-setboost|p2a: Kommo-o|atk|6|[from] move: Belly Drum" ->
            SetBoost(PID(P2, Some("a"), "Kommo-o"), Atk, 6, Some("[from] move: Belly Drum")),
          "|-setboost|p2a: Kommo-o|atk|6" ->
            SetBoost(PID(P2, Some("a"), "Kommo-o"), Atk, 6, None),
          "|-clearnegativeboost|p1a: Turtonator|[silent]" ->
            ClearNegativeBoost(PID(P1, Some("a"), "Turtonator"))
        )
      }
      it("should parse field effects") {
        parses (
          "|-weather|Sandstorm|[from] ability: Sand Stream|[of] p2a: Gigalith" ->
            Weather("Sandstorm", false),
          "|-weather|Sandstorm" ->
            Weather("Sandstorm", false),

          "|-weather|none" ->
            Weather("none", false),
          "|-weather|Hail|[upkeep]" ->
            Weather("Hail", true),
          "|-fieldstart|move: Misty Terrain|[from] ability: Misty Surge|[of] p1a: Tapu Fini" ->
            FieldConditionStart("move: Misty Terrain"),
          "|-fieldend|Misty Terrain" ->
            FieldConditionStop("Misty Terrain"),
          "|-sidestart|p2: playername|Spikes" ->
            SideConditionStart(P2, "Spikes"),
          "|-sidestart|p1: playername|move: Stealth Rock" ->
            SideConditionStart(P1, "move: Stealth Rock"),
          "|-sideend|p2: playername|Spikes|[from] move: Defog|[of] p2a: Lumineon" ->
            SideConditionStop(P2, "Spikes"),
        )
      }
      it("should do some move effects") {
        parses (
          "|-crit|p1a: Galvantula" -> Crit(PID(P1, Some("a"), "Galvantula")),
          "|-supereffective|p1a: Porygon-Z" ->
            SuperEffective(PID(P1, Some("a"), "Porygon-Z")),
          "|-resisted|p1a: Porygon2" ->
            Resisted(PID(P1, Some("a"), "Porygon2")),
          "|-immune|p1a: Mimikyu" ->
            Immune(PID(P1, Some("a"), "Mimikyu")),
          "|-zpower|p2a: Decidueye" ->
            Zpower(PID(P2, Some("a"), "Decidueye"))
        )
      }
      it("should parse item handling") {
        parses (
          "|-item|p2a: Omastar|Air Balloon" ->
            Item(PID(P2, Some("a"), "Omastar"), "Air Balloon"),
          "|-enditem|p2a: Kabutops|Sitrus Berry|[eat]" ->
            EndItem(PID(P2, Some("a"), "Kabutops"), "Sitrus Berry"),
          "|-enditem|p1a: Mew|White Herb" ->
            EndItem(PID(P1, Some("a"), "Mew"), "White Herb"),
          // TODO handle additional info
          "|-enditem|p1a: Celesteela|Leftovers|[from] move: Knock Off|[of] p2a: Scizor" ->
            EndItem(PID(P1, Some("a"), "Celesteela"), "Leftovers"),
        )
      }
      it("should parse abilities") {
        parses(
          "|-ability|p1a: Axew|Mold Breaker" ->
            Ability(PID(P1, Some("a"), "Axew"), "Mold Breaker"),
          "|-ability|p2a: Krookodile|Moxie|boost" ->
            Ability(PID(P2, Some("a"), "Krookodile"), "Moxie"),
          "|-ability|p2a: Staravia|Intimidate|boost" ->
            Ability(PID(P2, Some("a"), "Staravia"), "Intimidate"),
          "|-endability|p2a: Gigalith" ->
            EndAbility(PID(P2, Some("a"), "Gigalith"))
        )
      }
      it("should parse some miscellaneous effects") {
        parses (
          "|-activate|p1a: Amoonguss|trapped" ->
            Activate(PID(P1, Some("a"), "Amoonguss"), "trapped"),
          "|-activate|p1a: Golisopod|ability: Emergency Exit" ->
            Activate(PID(P1, Some("a"), "Golisopod"), "ability: Emergency Exit"),
          "|-hint|Something happened." ->
            Hint("Something happened."),
          "|-center" -> TripleCenter,
          "|-message|Blue forfeited" -> MiscMessage("Blue forfeited")
        )
      }
    }
  }
  describe("Parsing tournament messages") (pending)
}
