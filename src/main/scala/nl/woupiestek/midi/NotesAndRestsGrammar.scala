package nl.woupiestek.midi

import java.lang.Character._
import scalaz._
import Scalaz._
import nl.woupiestek.midi.parser.Parser
import nl.woupiestek.midi.parser.Parser._

object NotesAndRestsGrammar {

  type G[T] = Parser[Char, T]

  case class Track[M](length: Int, events: List[(Int, M)]) {
    def append(t: Track[M]): Track[M] =
      Track[M](length + t.length, events ++ t.events.map {
        case (x, y) => (x + length, y)
      })

    def stack(t: Track[M]): Track[M] =
      Track[M](math.max(length, t.length), events ++ t.events)
  }

  def grammar[M](implicit M: MidiMessages[M]): G[List[(Int, M)]] = {
    val whitespace: G[Unit] = If(isWhitespace(_: Char)).scanMap(_ => ())

    def char(c: Char): G[Char] = If((_: Char) == c).one <* whitespace

    val number: G[Int] =
      (If(('1' to '9').contains(_: Char)).one |@|
        If(('0' to '9').contains(_: Char)).one.list)(_ :: _)
        .map(_.toList.mkString.toInt)

    val note: G[Track[M]] =
      char('n') *> (number |@| number)(
        (length, pitch) =>
          Track(
            length,
            (0, M.noteOn(0, pitch, 64)) :: (length, M.noteOff(0, pitch)) :: Nil))

    val rest: G[Track[M]] = char('r') *> number.map(Track(_, Nil))

    val empty: Track[M] = Track(0, Nil)

    def poly: G[Track[M]] =
      (((char('[') *> sequence) |@| (char('|') *> sequence).list)(_ :: _) <* char(
        ']')).map(_.foldLeft(empty)(_ stack _))

    def sequence: G[Track[M]] =
      (note <+> rest <+> poly).list.map(_.foldLeft(empty)(_ append _))

    sequence.map(_.events.sortBy { case (t, _) => t })
  }
}
