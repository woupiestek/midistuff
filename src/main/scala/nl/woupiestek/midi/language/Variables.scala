package nl.woupiestek.midi.language

import nl.woupiestek.midi.language.SParser.token
import scalaz._
import Scalaz._
import nl.woupiestek.midi.parser.Parser
import nl.woupiestek.midi.parser.Parser._

object Variables {

  type D[V, T] = Map[V, T] => Option[T]

  def variables[V, T](v: Parser[Char, V]): Parser[Char, D[V, T]] = v.map(get)

  private def get[V, T](x: V)(y: Map[V, T]): Option[T] = y.get(x)

  def let[V, T](
    v: Parser[Char, V],
    m: Parser[Char, D[V, T]],
    n: Parser[Char, D[V, T]]): Parser[Char, D[V, T]] =
    ((token('[') *> v <* token('=')) |@| m <* token(']') |@| n)(put)

  private def put[T, V, I](
    x: V,
    p: D[V, T],
    q: D[V, T]): Map[V, T] => Option[T] =
    (y: Map[V, T]) => p(y).flatMap(z => q(y + (x -> z)))
}
