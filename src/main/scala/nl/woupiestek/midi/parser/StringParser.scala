package nl.woupiestek.midi.parser

import scala.annotation.tailrec
import scalaz.{-\/, \/, \/-}

object StringParser {

  type G[X] = Grammar[Option[Char], X]

  def parse[X](input: String, grammar: G[X]): Option[X] = {
    @tailrec def first(options: List[(X \/ (Option[Char] => G[X]), Int)]): Option[X] = options match {
      case Nil => None
      case head :: tail => head match {
        case (-\/(x), _) => Some(x)
        case (\/-(f), o) => first(f(option(o)).options.map((_, o + 1)) ++ tail)
      }
    }
    def option(offset: Int): Option[Char] =
      Some(offset).filter(i => i < input.length && 0 <= i).map(input.charAt)
    first(grammar.options.map((_, 0)))
  }

}