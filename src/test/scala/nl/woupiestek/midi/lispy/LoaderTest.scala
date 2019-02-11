package nl.woupiestek.midi.lispy

import nl.woupiestek.midi.parser.Parser
import nl.woupiestek.midi.{ ConsoleLogic, Player }
import org.scalatest.FunSuite
import scalaz._
import Scalaz._
import scala.io.Source

class LoaderTest extends FunSuite {

  val input: String = Source.fromFile("testLispy.txt").getLines().mkString("\n")

  test("sound free test") {
    assert(Loader.load("testLispy.txt").nonEmpty)
  }

  def parse[X](parser: Parser[Char, X], input: String): Option[X] =
    parser.parse[Option, List, Char, X](input.toList)

  test("look around") {
    assert(parse(LParser.file, input).nonEmpty)
  }

  val w: String =
    """put vader-jacob seq[
      |                  repeat 2 seq[note 60 24 note 62 24 note 64 24 note 60 24]
      |                  repeat 2 seq[note 64 24 note 65 24 note 67 48]
      |                  repeat 2 seq[note 67 12 note 69 12 note 67 12 note 65 12 note 64 24 note 60 24]
      |                  repeat 2 seq[note 60 24 note 55 24 note 60 48]]
      |seq[tempo 150 patch 4 rest 4
      |  get vader-jacob
      |  chord[
      |    get vader-jacob
      |    channel 1 seq[patch 10 rest 192 get vader-jacob]]]""".stripMargin

  test("console logic") {
    val x = ConsoleLogic.next(w, Map.empty, new Player {
      override def play(track: Track): Unit = println(track)

      override def close(): Unit = ()
    })
    assert(x contains "vader-jacob")
  }

  test("other steps") {
    assert(parse(LParser.number, "-123").contains(-123))
    assert(parse(LParser.identifier, "john").contains("john"))
    assert(parse(LParser.beginList, "[").nonEmpty)
    assert(parse(LParser.endList, "]").nonEmpty)
  }

}
