package nl.woupiestek.midi.tagless

import java.lang.Character._
import scalaz._
import Scalaz._
import nl.woupiestek.midi.parser.Parser
import nl.woupiestek.midi.parser.Parser._

trait Tokenizer[T] {
  def number: Parser[T, Int]

  def identifier: Parser[T, String]

  def keyword(name: String): Parser[T, Unit]

  def beginList: Parser[T, Unit]

  def endList: Parser[T, Unit]

  def beginFile: Parser[T, Unit]

  def endFile: Parser[T, Unit]
}

object StringTokenizer extends Tokenizer[Char] {

  private type G[T] = Parser[Char, T]
  val space: G[Unit] = If(isWhitespace(_: Char)).scanMap(_ => ())

  val comment: G[Unit] =
    is(';') *> If((c: Char) => c != '\n' && c != '\r').scanMap(_ => ())

  val separator: G[Unit] = (space <+> comment).list.map(_ => ())

  override val beginList: G[Unit] = token('[')

  override val endList: G[Unit] = token(']')

  override val beginFile: G[Unit] = keyword("midistuff-file-version-0.1")

  override val endFile: G[Unit] = ().point[G]
  private val reserved = Set('[', ']', ';', ' ', '\n', '\r', '\t', '\f')

  override val number: G[Int] = {

    val digits = If(isDigit(_: Char))
      .scanMap(c => List(c))
      .map(_.mkString.toInt) <* space

    is('-') *> digits.map(-_) <+> digits

  }

  override val identifier: G[String] =
    (If(isLetter(_: Char)).one |@|
      If((c: Char) => !reserved.contains(c)).one.list)(_ :: _)
      .map(_.toList.mkString) <* space

  override def keyword(word: String): G[Unit] = {
    implicit val monoid: Monoid[G[Unit]] = ApplicativePlus[G].monoid[Unit]
    word.toList.foldMap(is) <* space
  }

  private def token(c: Char): G[Unit] = is(c) <* space

  private def is(c: Char) = If((_: Char) == c).one.map(_ => ())

}
