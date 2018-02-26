package nl.woupiestek.midi.parser

import scalaz.Monoid

object StringParser {

  type G[X] = Rule.Grammar[Char, X]

  private class Instance[X] extends Monoid[List[Char] => Option[X]] {
    override def zero: List[Char] => Option[X] = _ => None

    override def append(f1: List[Char] => Option[X], f2: => List[Char] => Option[X]): List[Char] => Option[X] = x => f1(x) orElse f2(x)
  }

  def parse[X](input: String, grammar: G[X]): Option[X] = input.foldLeft(grammar)(_ next _).done.headOption
}