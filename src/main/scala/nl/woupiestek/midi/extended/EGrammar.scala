package nl.woupiestek.midi.extended

import nl.woupiestek.midi.extended.ESequence.Element
import nl.woupiestek.midi.parser.Rule._

object EGrammar {

  type G[T] = Grammar[Option[Char], T]

  def sequence: G[ESequence] = {

    def element: G[Element] = {
      def number: G[Int] = spaced(read[Option[Char]].collect[Char] {
        case Some(n) if ('0' to '9').contains(n) => n
      }.oneOrMore).map(_.mkString.toInt)

      def note: G[Element] = for {
        _ <- spaced(char('n'))
        pitch <- number
        duration <- number
      } yield ESequence.Note(pitch, duration)

      def rest: G[Element] = for {
        _ <- spaced(char('r'))
        duration <- number
      } yield ESequence.Rest(duration)

      def poly: G[Element] = for {
        _ <- spaced(char('['))
        x <- sequence
        y <- (for {
          _ <- spaced(char('|'))
          z <- sequence
        } yield z).zeroOrMore
        _ <- spaced(char(']'))
      } yield ESequence.Poly(x :: y)

      def get: G[Element] = for {
        _ <- char('$')
        x <- identifier
      } yield ESequence.Get(x)

      note or rest or poly or get
    }

    def put: G[ESequence] = {
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

    def identifier: G[String] = spaced(read[Option[Char]].collect[Char] {
      case Some(char) if ('A' to 'Z').union('a' to 'z').contains(char) => char
    }.oneOrMore).map(_.mkString)

    def spaced[T](g: G[T]): G[T] = {
      def space = read[Option[Char]].collect {
        case Some(char) if " \n\r\t\f".contains(char) => ()
      }.zeroOrMore.map(_ => ())
      for {
        x <- g
        _ <- space
      } yield x
    }

    def char(c: Char): G[Unit] = read[Option[Char]].collect { case Some(x) if x == c => () }

    put or element.zeroOrMore.map(Elements)
  }
}
