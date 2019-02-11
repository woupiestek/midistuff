package nl.woupiestek.midi.lispy

import java.lang.Character._
import scalaz._
import Scalaz._
import nl.woupiestek.midi.parser.Parser
import nl.woupiestek.midi.parser.Parser._

object LParser {

  type P[A] = Parser[Char, A]

  sealed trait Result {
    def flatMap(f: Track => Result): Result

    def map(f: Track => Track): Result = flatMap(track => Play(f(track)))
  }

  case class Get(key: String, next: Track => Result) extends Result {
    override def flatMap(f: (Track) => Result): Result =
      copy(next = track => next(track).flatMap(f))
  }

  case class Play(track: Track) extends Result {
    override def flatMap(f: (Track) => Result): Result = f(track)
  }

  case class Put(key: String, track: Track, next: Result) extends Result {
    override def flatMap(f: (Track) => Result): Result =
      copy(next = next.flatMap(f))
  }

  private val reserved = Set('[', ']', ';', ' ', '\n', '\r', '\t', '\f')

  val beginList: P[Unit] = is('[')
  val endList: P[Unit] = is(']')
  val number: Parser[Char, Int] = (is('-') *> If(isDigit(_: Char))
    .scanMap(x => List(x))
    .map(digits => -digits.mkString.toInt)) <+> If(isDigit(_: Char))
    .scanMap(c => List(c))
    .map(_.mkString.toInt)

  val identifier: Parser[Char, String] =
    (If(isLetter(_: Char)).one |@| If((x: Char) => !reserved.contains(x)).one.list)(
      (h, t) => (h :: t).toList.mkString)

  def key(name: String): Parser[Char, Unit] =
    name.foldLeft(().point[P])((x, y) => x <+> is(y))

  private def is(c: Char): P[Unit] = If((_: Char) == c).one.map(_ => ())

  val comment: P[Unit] =
    If((_: Char) == ';').one *>
      If((c: Char) => c != '\n' && c != '\r').scanMap(_ => ())

  val space: P[Unit] = If(isWhitespace(_: Char)).scanMap(_ => ())

  val separator: P[Unit] = (space <+> comment).list.map(_ => ())
  def file: Parser[Char, Result] = track

  def scalar(f: (Track, Int) => Track): P[Result] =
    (number |@| track)((x, y) => y.flatMap(track => Play(f(track, x))))

  def fold(f: (Track, Track) => Track): P[Result] =
    (beginList *> track.nel <* endList)
      .map(
        _.foldLeft[Result](Play(Track.empty))(
          (x, y) => x.flatMap(x0 => y.map(y0 => f(x0, y0)))))

  def argumentParsers: Map[String, P[Result]] =
    Map(
      "note" ->
        (number |@| number)(
          (duration, key) =>
            Play(
              Track(
                duration,
                List((0, NoteOn(0, key)), (duration, NoteOff(0, key)))))),
      "rest" -> number.map(d => Play(Track(d, Nil))),
      "patch" -> number.map(
        p => Play(Track(0, List((0, ProgramChange(0, p)))))),
      "tempo" -> number.map(t => Play(Track(0, List((0, Tempo(t)))))),
      "seq" -> fold(_ append _),
      "chord" -> fold(_ stack _),
      "piu" -> scalar(_ piu _),
      "cresc" -> scalar(_ cresc _),
      "transpose" -> scalar(_ transpose _),
      "channel" -> scalar(_ toChannel _),
      "repeat" -> scalar(
        (y, x) => (1 to x).map(_ => y).foldLeft(Track.empty)(_ append _)),
      "put" -> (identifier |@| track |@| track)(
        (x, y, z) => y.flatMap(y2 => Put(x, y2, z))),
      "get" -> identifier.map(Get(_, Play)))
  def track: P[Result] = {
    implicit val x: Monoid[P[Result]] =
      ApplicativePlus[P].monoid[Result]
    argumentParsers.toList.foldMap {
      case (x, y) => key(x) *> y
    }
  }

}
