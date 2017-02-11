package nl.woupiestek.midi.tagless

import nl.woupiestek.midi.parser.Grammar
import nl.woupiestek.midi.parser.Grammar.{ collect, fail, read }

trait Tokenizer[T] {
  def number: Grammar[T, Int]

  def identifier: Grammar[T, String]

  def beginList: Grammar[T, Unit]

  def endList: Grammar[T, Unit]

  def beginFile: Grammar[T, Unit]

  def endFile: Grammar[T, Unit]
}

object StringTokenizer extends Tokenizer[Option[Char]] {

  override def number: Grammar[Option[Char], Int] = separate {
    read[Option[Char]].flatMap {
      case Some('-') =>
        collect[Option[Char], Char] { case Some(x) if Character.isDigit(x) => x }
          .oneOrMore
          .map(digits => -digits.mkString.toInt)
      case Some(first) if ('0' to '9').contains(first) =>
        collect[Option[Char], Char] {
          case Some(c) if ('0' to '9').contains(c) => c
        }.zeroOrMore.map {
          digits => (first :: digits).mkString.toInt
        }
      case _ => fail
    }
  }

  override def identifier: Grammar[Option[Char], String] = separate {
    read[Option[Char]].flatMap {
      case Some(x) if Character.isAlphabetic(x) => for {
        digits <- collect[Option[Char], Char] {
          case Some(c) if !reserved.contains(c) => c
        }.zeroOrMore
      } yield (x :: digits).mkString
    }
  }

  override def beginList: Grammar[Option[Char], Unit] = separate(collect { case Some('[') => () })

  override def endList: Grammar[Option[Char], Unit] = separate(collect { case Some(']') => () })

  override def beginFile: Grammar[Option[Char], Unit] = identifier.collect { case "midistuff-file-version-0.1" => () }

  override def endFile: Grammar[Option[Char], Unit] = separate(collect { case None => () })

  private type G[T] = Grammar[Option[Char], T]

  private def reserved = Set('[', ']', ';', ' ', '\n', '\r', '\t', '\f')

  def comment: G[Unit] = for {
    _ <- read[Option[Char]].filter(_.forall(';'.equals))
    _ <- read[Option[Char]].filter(_.forall(Set('\n', '\r').contains)).zeroOrMore
  } yield ()

  def space: G[Unit] = read[Option[Char]].filter(_.forall(Character.isWhitespace)).map(_ => ())

  def separator: G[Unit] = (space | comment).zeroOrMore.map(_ => ())

  def separate[T](x: G[T]): G[T] = for {
    y <- x
    _ <- separator
  } yield y
}
