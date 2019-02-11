package nl.woupiestek.midi.language

import java.lang.Character._
import scalaz._
import Scalaz._
import nl.woupiestek.midi.parser.Parser
import nl.woupiestek.midi.parser.Parser._

object SParser {

  val lispComment: Parser[Char, Unit] =
    token(';') *> If((c: Char) => c != '\n' && c != '\r').scanMap(_ => ())

  val ignore: Parser[Char, Unit] =
    If(isWhitespace(_: Char)).scanMap(_ => ()) <+> lispComment

  private val stringChar: Parser[Char, Char] =
    If((x: Char) => x != '"' && x != '\\').one <+>
      (If((x: Char) => x == '\\').one *> If((_: Char) => true).one)

  val string: Parser[Char, String] =
    (token('"') *> stringChar.list <* token('"')).map(_.toList.mkString)

  val number: Parser[Char, Int] = If(('0' to '9').contains(_: Char)).one.nel
    .map(_.foldLeft(0)((x: Int, y: Char) => 10 * x + y - '0'))

  private val operators: Set[Char] =
    ('!' to '~').filterNot(isLetter).filterNot(Set(';', '(', ')', '"')).toSet

  val atom: Parser[Char, String] = If(operators.contains(_: Char)).one.nel
    .map(_.toList.mkString) <* ignore

  def token(c: Char): Parser[Char, Char] = If((_: Char) == c).one

  def list[S](elt: Parser[Char, S]): Parser[Char, IList[S]] =
    token('(') *> elt.list <* token(')')

  def lisp[S](atom: Parser[Char, S])(sList: IList[S] => S): Parser[Char, S] =
    list(lisp(atom)(sList)).map(sList) <+> atom

  def unit[S](
    sAtom: String => S,
    sString: String => S,
    sNumber: Int => S,
    sList: IList[S] => S): Parser[Char, S] =
    lisp(atom.map(sAtom) <+> number.map(sNumber) <+> string.map(sString))(sList)
}
