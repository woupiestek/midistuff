package nl.woupiestek.midi.extended

import java.lang.Character._
import scalaz._
import Scalaz._
import nl.woupiestek.midi.parser.Parser
import nl.woupiestek.midi.parser.Parser._

import nl.woupiestek.midi.extended.ESequence.Element

object EGrammar {

  type G[T] = Parser[Char, T]

  val space = If(isWhitespace(_: Char)).scanMap(_ => ())

  val identifier: G[String] =
    If(isLetter(_: Char)).one.nel.map(_.toList.mkString) <* space

  val put: G[ESequence] =
    ((token('{') *> sequence <* token('}') <* token('=')) |@| (identifier <* token(
      '.')) |@| sequence)(Put)

  def char(c: Char): G[Unit] = If((_: Char) == c).one.map(_ => ())
  def token(c: Char): G[Unit] = char(c) <* space
  def sequence: G[ESequence] = {

    def element: G[Element] = {
      def number: G[Int] =
        If(isDigit(_: Char)).one.nel
          .map(_.toList.mkString.toInt) <* space

      def note: G[Element] =
        token('n') *> (number |@| number)(ESequence.Note)

      def rest: G[Element] = token('r') *> number.map(ESequence.Rest)

      def poly: G[Element] =
        ((token('[') *> sequence) |@|
          ((token('|') *> sequence).list <* token(']')))(
            (x, y) => ESequence.Poly((x :: y).toList))

      def get: G[Element] =
        char('$') *> identifier.map(ESequence.Get)

      note <+> rest <+> poly <+> get
    }

    put <+> element.list.map(elts => Elements(elts.toList))
  }
}
