package nl.woupiestek.midi.language

import nl.woupiestek.midi.language.SParser.token
import nl.woupiestek.midi.parser.Rule.Grammar

object Variables {

  type D[V, T] = Map[V, T] => Option[T]

  def variables[V, T](v: Grammar[Char, V]): Grammar[Char, D[V, T]] = v.map(get)

  private def get[V, T](x: V)(y: Map[V, T]): Option[T] = y.get(x)

  def let[V, T](v: Grammar[Char, V], m: Grammar[Char, D[V, T]], n: Grammar[Char, D[V, T]]): Grammar[Char, D[V, T]] =
    for {
      _ <- token('[')
      x <- v
      _ <- token('=')
      p <- m
      _ <- token(']')
      q <- n
    } yield put(x, p, q)

  private def put[T, V, I](x: V, p: D[V, T], q: D[V, T])(y: Map[V, T]): Option[T] = p(y).flatMap(z => q(y + (x -> z)))
}
