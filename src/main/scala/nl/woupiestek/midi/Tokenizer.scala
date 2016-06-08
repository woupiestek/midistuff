package nl.woupiestek.midi

import nl.woupiestek.midi.parser.Grammar

object Tokenizer {

  type G[T] = Grammar[Option[Char], T]

  sealed trait Token

  case class Number(value: Int) extends Token

  case class Symbol(char: Char) extends Token

  def tokens: G[List[Token]] = whitespace.flatMap(_ => repeat(number | symbol))

  private def number: G[Token] = for {
    x <- capture if ('1' to '9').contains(x)
    y <- repeat(capture.filter(('0' to '9').contains))
    _ <- whitespace
  } yield Number((x :: y).mkString.toInt)

  private def symbol: G[Token] = for {
    x <- capture if !('0' to '9').contains(x)
    _ <- whitespace
  } yield Symbol(x)

  private def whitespace: G[Unit] = repeat(capture.filter(s => " \n\r\t\f".contains(s))).map(_ => ())

  private def repeat[T](g: G[T]): G[List[T]] = (for {
    x <- g
    y <- repeat(g)
  } yield x :: y) | Grammar.point(Nil)

  private def capture: G[Char] = for {
    Some(x) <- Grammar.read[Option[Char]]
  } yield x
}
