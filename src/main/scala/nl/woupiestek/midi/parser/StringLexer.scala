package nl.woupiestek.midi.parser

object StringLexer {

  type G[T] = Grammar[Option[Char], T]

  def stringLiteral: G[List[(Char, Boolean)]] = for {
    Some('"') <- Grammar.read[Option[Char]]
    y <- repeat(capture.filter(x => x != '"' && x != '\\').map((_, false)) | (for {
      Some('\\') <- Grammar.read[Option[Char]]
      b <- capture
    } yield (b, true)))
    Some('"') <- Grammar.read[Option[Char]]
  } yield y

  def nonNegativeInteger: G[Int] = for {
    x <- capture if ('1' to '9').contains(x)
    y <- repeat(capture.filter(('0' to '9').contains))
    _ <- whitespace
  } yield (x :: y).mkString.toInt

  private def repeat[T](g: G[T]): G[List[T]] = (for {
    x <- g
    y <- repeat(g)
  } yield x :: y) | Grammar.point(Nil)

  private def capture: G[Char] = for {
    Some(x) <- Grammar.read[Option[Char]]
  } yield x

  private def whitespace: G[Unit] = repeat(capture.filter(s => " \n\r\t\f".contains(s))).map(_ => ())
}
