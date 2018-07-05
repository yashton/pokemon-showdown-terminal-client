package science.snelgrove.showdown

import org.scalatest.Matchers
import org.scalatest.FunSpec
import science.snelgrove.showdown.protocol._
import scala.io.Source

class ExampleMessageParserTest extends FunSpec with Matchers {
  describe("Parsing a bunch of example messages") {
    it("should produce no UnknownMessage parse.") {
      val right = Source.fromResource("right").getLines
        .flatMap(MessageParser.parseRaw).flatten
        .flatMap(_.msgs)
        .collect { case x: UnknownMessage => x }
        .map{f => println(f); f}
      right.size should be (0)

      val left = Source.fromResource("left").getLines
        .flatMap(MessageParser.parseRaw).flatten
        .flatMap(_.msgs)
        .collect { case x: UnknownMessage => x }
        .map{ f => println(f); f}
      left.size should be (0)
    }
  }
}
