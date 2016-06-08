package nl.woupiestek.midi.extended

import nl.woupiestek.midi.parser.Grammar

object EGrammar {

  type G[T] = Grammar[Option[Char], T]

  def sequence: G[Sequence] = put | element.zeroOrMore.map(Elements)

  def element: G[Sequence.Element] = {

    def note: G[Sequence.Note] = for {
      _ <- spaced(char('n'))
      pitch <- number
      duration <- number
    } yield Sequence.Note(pitch, duration)

    def rest: G[Sequence.Rest] = for {
      _ <- spaced(char('r'))
      duration <- number
    } yield Sequence.Rest(duration)

    def poly: G[Sequence.Poly] = for {
      _ <- spaced(char('['))
      x <- sequence
      y <- (for {
        _ <- spaced(char('|'))
        z <- sequence
      } yield z).zeroOrMore
      _ <- spaced(char(']'))
    } yield Sequence.Poly(x :: y)

    def get: G[Sequence.Get] = for {
      _ <- char('$')
      x <- identifier
    } yield Sequence.Get(x)

    note | rest | poly | get
  }

  def put: G[Sequence] = {
    for {
      _ <- spaced(char('{'))
      x <- sequence
      _ <- spaced(char('}'))
      _ <- spaced(char('='))
      y <- identifier
      _ <- spaced(char('.'))
      z <- sequence
    } yield Put(x, y, z)
  }

  def identifier: G[String] = spaced(Grammar.read[Option[Char]].collect[Char] {
    case Some(char) if ('A' to 'Z').union('a' to 'z').contains(char) => char
  }.oneOrMore).map(_.mkString)

  def number: G[Int] = spaced(Grammar.read[Option[Char]].collect[Char] {
    case Some(n) if ('0' to '9').contains(n) => n
  }.oneOrMore).map(_.mkString.toInt)

  def spaced[T](g: G[T]): G[T] = for {
    x <- g
    _ <- space
  } yield x

  def char(c: Char): G[Unit] = Grammar.read[Option[Char]].collect { case Some(x) if x == c => () }

  def space = Grammar.read[Option[Char]].collect {
    case Some(char) if " \n\r\t\f".contains(char) => ()
  }.zeroOrMore.map(_ => ())

}
