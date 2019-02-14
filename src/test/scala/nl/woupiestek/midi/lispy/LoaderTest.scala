package nl.woupiestek.midi.lispy

import nl.woupiestek.midi.parser._
import nl.woupiestek.midi.{ ConsoleLogic, Player }
import org.scalatest.FunSuite
import scalaz._
import Scalaz._
import scala.io.Source

class LoaderTest extends FunSuite {

  val input: String = Source.fromFile("testLispy.txt").getLines().mkString("\n")

  test("sound free test") {
    //assert(Loader.load("testLispy.txt").nonEmpty)
  }

  def parse[X](parser: Parser[Char, X], input: String): LazyMaybe[X] =
    parser.parse[LazyMaybe, List, Char, X](input.toList)

  test("look around") {
    //assert(parse(LParser.track, input).nonEmpty)
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

  ignore("console logic") {
    val x = ConsoleLogic.next(w, Map.empty, new Player {
      override def play(track: Track): Unit = println(track)

      override def close(): Unit = ()
    })
    assert(x contains "vader-jacob")
  }

  test("other steps") {
    assert(parse(LParser.beginList, "[").fold(true)(_ => false))
    assert(parse(LParser.endList, "]").fold(true)(_ => false))
    assert(parse(LParser.identifier, "john").fold[Option[String]](None)(Some(_)) == Some("john"))
    assert(parse(LParser.number, "123").fold[Option[Int]](None)(Some(_)) == Some(123))
    assert(parse(LParser.number, "-123").fold[Option[Int]](None)(Some(_)) == Some(-123))
  }

}
