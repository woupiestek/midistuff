package nl.woupiestek.midi

import nl.woupiestek.midi.parser.Grammar

object NotesAndRestsGrammar {

  type G[T] = Grammar[Option[Char], T]

  private case class Temp(length: Int, events: List[(Int, MidiEvent)]) {
    def append(t: Temp): Temp = Temp(length + t.length, events ++ t.events.map { case (x, y) => (x + length, y) })

    def stack(t: Temp): Temp = Temp(math.max(length, t.length), events ++ t.events)
  }

  def grammar: G[List[(Int, MidiEvent)]] = sequence.map(_.events.sortBy { case (t, _) => t })

  private def number: G[Int] = for {
    x <- capture if ('1' to '9').contains(x)
    y <- capture.filter(('0' to '9').contains).zeroOrMore
    _ <- whitespace
  } yield (x :: y).mkString.toInt

  private def note: G[Temp] = for {
    - <- char('n')
    pitch <- number
    length <- number
  } yield Temp(length, (0, NoteOn(0, pitch, 64)) ::(length, NoteOff(0, pitch)) :: Nil)

  private def rest: G[Temp] = for {
    _ <- char('r')
    length <- number
  } yield Temp(length, Nil)

  private def poly: G[Temp] = for {
    _ <- char('[')
    x <- charSeparatedList(sequence, '|')
    _ <- char(']')
  } yield x.foldLeft(emptyTemp)(_ stack _)

  private def sequence: G[Temp] =
    for (nors <- (note | rest | poly).zeroOrMore) yield nors.foldLeft(emptyTemp)(_ append _)

  private val emptyTemp: Temp = Temp(0, Nil)

  private def charSeparatedList[T](g: G[T], separator: Char): G[List[T]] = for {
    x <- g
    y <- (for {
      _ <- char(separator)
      z <- charSeparatedList(g, separator)
    } yield z) | Grammar.point(Nil)
  } yield x :: y

  private def char(c: Char): G[Unit] = for {
    x <- capture if x == c
    _ <- whitespace
  } yield ()

  private def capture: G[Char] = for {
    Some(x) <- Grammar.read[Option[Char]]
  } yield x

  private def whitespace: G[Unit] = capture.filter(s => " \n\r\t\f".contains(s)).zeroOrMore.map(_ => ())

}