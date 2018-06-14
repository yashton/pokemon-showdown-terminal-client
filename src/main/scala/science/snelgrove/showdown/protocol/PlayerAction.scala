package science.snelgrove.showdown.protocol


sealed trait PlayerAction
case class SetTeamOrder(order: String) extends PlayerAction
case class UseMove(number: Int, target: PokemonId) extends PlayerAction
case class DoSwitch(number: Int) extends PlayerAction
case object DoShift extends PlayerAction
case object DoPass extends PlayerAction
case class DoublesAction(first: PlayerAction, second: PlayerAction)
case class TriplesAction(first: PlayerAction, second: PlayerAction)
// TODO tournament commands

/*
COMMANDS: /msg, /reply, /logout, /challenge, /search, /rating, /whois
OPTION COMMANDS: /nick, /avatar, /ignore, /away, /back, /timestamps, /highlight
INFORMATIONAL COMMANDS: /data, /dexsearch, /movesearch, /itemsearch, /groups, /faq, /rules, /intro, /formatshelp, /othermetas, /learn, /analysis, /calc (replace / with ! to broadcast. Broadcasting requires: + % @ * # & ~)
For an overview of room commands, use /roomhelp
For details of a specific command, use something like: /help data
 */
