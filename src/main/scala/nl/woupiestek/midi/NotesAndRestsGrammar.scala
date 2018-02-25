package nl.woupiestek.midi

import nl.woupiestek.midi.parser.Rule._

object NotesAndRestsGrammar {

  type G[T] = Grammar[Option[Char], T]

  case class Track[M](length: Int, events: List[(Int, M)]) {
    def append(t: Track[M]): Track[M] = Track[M](length + t.length, events ++ t.events.map { case (x, y) => (x + length, y) })

    def stack(t: Track[M]): Track[M] = Track[M](math.max(length, t.length), events ++ t.events)
  }

  def grammar[M](implicit M: MidiMessage[M]): G[List[(Int, M)]] = {
    def sequence: G[Track[M]] = {
      def capture: G[Char] = for {
        Some(x) <- read[Option[Char]]
      } yield x

      def whitespace: G[Unit] = capture.filter(s => " \n\r\t\f".contains(s)).zeroOrMore.map(_ => ())

      def char(c: Char): G[Unit] = for {
        x <- capture if x == c
        _ <- whitespace
      } yield ()

      def number: G[Int] = for {
        x <- capture if ('1' to '9').contains(x)
        y <- capture.filter(('0' to '9').contains).zeroOrMore
        _ <- whitespace
      } yield (x :: y).mkString.toInt

      def note: G[Track[M]] = for {
        _ <- char('n')
        pitch <- number
        length <- number
      } yield Track(length, (0, M.noteOn(0, pitch, 64)) :: (length, M.noteOff(0, pitch)) :: Nil)

      def rest: G[Track[M]] = for {
        _ <- char('r')
        length <- number
      } yield Track(length, Nil)

      val empty: Track[M] = Track(0, Nil)

      def poly: G[Track[M]] = for {
        _ <- char('[')
        x <- sequence
        y <- (for {
          _ <- char('|')
          z <- sequence
        } yield z).zeroOrMore
        _ <- char(']')
      } yield (x :: y).foldLeft(empty)(_ stack _)

      for (nors <- (note or rest or poly).zeroOrMore) yield nors.foldLeft(empty)(_ append _)
    }

    sequence.map(_.events.sortBy { case (t, _) => t })
  }
}