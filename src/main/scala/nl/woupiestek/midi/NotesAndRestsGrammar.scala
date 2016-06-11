package nl.woupiestek.midi

import nl.woupiestek.midi.parser.Grammar

object NotesAndRestsGrammar {

  type G[T] = Grammar[Option[Char], T]

  case class Track(length: Int, events: List[(Int, MidiMessage)]) {
    def append(t: Track): Track = Track(length + t.length, events ++ t.events.map { case (x, y) => (x + length, y) })

    def stack(t: Track): Track = Track(math.max(length, t.length), events ++ t.events)
  }

  def grammar: G[List[(Int, MidiMessage)]] = {
    def sequence: G[Track] = {
      def capture: G[Char] = for {
        Some(x) <- Grammar.read[Option[Char]]
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

      def note: G[Track] = for {
        - <- char('n')
        pitch <- number
        length <- number
      } yield Track(length, (0, NoteOn(0, pitch, 64)) ::(length, NoteOff(0, pitch)) :: Nil)

      def rest: G[Track] = for {
        _ <- char('r')
        length <- number
      } yield Track(length, Nil)

      val empty: Track = Track(0, Nil)

      def poly: G[Track] = for {
        _ <- char('[')
        x <- sequence
        y <- (for {
          _ <- char('|')
          z <- sequence
        } yield z).zeroOrMore
        _ <- char(']')
      } yield (x :: y).foldLeft(empty)(_ stack _)

      for (nors <- (note | rest | poly).zeroOrMore) yield nors.foldLeft(empty)(_ append _)
    }

    sequence.map(_.events.sortBy { case (t, _) => t })
  }
}