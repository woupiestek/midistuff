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

  private val reserved: Set[Char] = Set('[', ']', ';')

  val beginList: P[Unit] = is('[')
  val endList: P[Unit] = is(']')
  private val positive =
    If(isDigit(_: Char)).one.nel.map(_.toList.mkString.toInt)

  val number: Parser[Char, Int] = (is('-') *> positive.map(-_)) <+> positive

  val identifier: Parser[Char, String] =
    (If(isLetter(_: Char)).one <*> If(
      (x: Char) => !(isWhitespace(x) || reserved.contains(x))).scanRight[List[Char]](Nil)(_ :: _)
      .map(t => (h: Char) => (h :: t.reverse).mkString))

  def key(name: String): Parser[Char, Unit] =
    name.foldLeft(().point[P])((x, y) => x <+> is(y))

  private def is(c: Char): P[Unit] = If((_: Char) == c).one.map(_ => ())

  val comment: P[Unit] =
    If((_: Char) == ';').one *>
      If((c: Char) => c != '\n' && c != '\r').scanMap(_ => ())

  val space: P[Unit] = If(isWhitespace(_: Char)).scanMap(_ => ())

  val separator: P[Unit] = (space <+> comment).list.map(_ => ())

  def track: P[Result] =
    rule("note")((number |@| number)(playNote)) <+>
      rule("rest")(number.map(d => Play(Track(d, Nil)))) <+>
      rule("patch")(
        number.map(p => Play(Track(0, List((0, ProgramChange(0, p))))))) <+>
        rule("tempo")(number.map(t => Play(Track(0, List((0, Tempo(t))))))) <+>
        rule("seq")(fold(_ append _)) <+>
        rule("chord")(fold(_ stack _)) <+>
        rule("piu")(scalar(_ piu _)) <+>
        rule("cresc")(scalar(_ cresc _)) <+>
        rule("transpose")(scalar(_ transpose _)) <+>
        rule("channel")(scalar(_ toChannel _)) <+>
        rule("repeat")(
          scalar((y, x) => (1 to x).map(_ => y).foldLeft(Track.empty)(_ append _))) <+>
          rule("put")(
            (identifier |@| track |@| track)((x, y, z) => y.flatMap(Put(x, _, z)))) <+>
            rule("get")(identifier.map(Get(_, Play)))

  private def rule(x: String)(y: => P[Result]): P[Result] = key(x) *> suspend(y)

  private def scalar(f: (Track, Int) => Track): P[Result] =
    (number |@| track)((x, y) => y.map(f(_, x)))

  private def fold(f: (Track, Track) => Track): P[Result] =
    (beginList *> track.nel <* endList)
      .map(
        _.foldLeft[Result](Play(Track.empty))(
          (x, y) => x.flatMap(x0 => y.map(y0 => f(x0, y0)))))

  private def playNote(duration: Int, key: Int) =
    Play(
      Track(duration, List((0, NoteOn(0, key)), (duration, NoteOff(0, key)))))

}
