package nl.woupiestek.midi.language

import java.lang.Character._

import nl.woupiestek.midi.parser.Rule._

object SParser {

  val lispComment: Grammar[Char, Unit] =
    token(';') ~> fixPoint[Char, Unit](x => read[Char].map(c => if (c == '\n' || c == '\r') write(()) else x))

  val ignore: Grammar[Char, Unit] =
    (read[Char].filter(isWhitespace).discard or lispComment).zeroOrMore.discard

  val string: Grammar[Char, String] = token('"') ~>
    fixPoint[Char, List[Char]](rest => read[Char].flatMap {
      case '"' => write(Nil)
      case '\\' => read[Char].flatMap(h => rest.map(h :: _))
      case h => rest.map(h :: _)
    }).map(_.mkString)

  val number: Grammar[Char, Int] = read[Char].filter(('0' to '9').contains).oneOrMore.map(_.foldLeft(0)(10 * _ + _ - '0'))

  private val operators = ('!' to '~').filterNot(isLetter).filterNot(Set(';', '(', ')', '"')).toSet

  val atom: Grammar[Char, String] = read[Char].filter(operators).oneOrMore.map(_.toString()) <~ ignore

  def token(c: Char): Grammar[Char, Unit] = read[Char].flatMap(d => if (c == d) ignore else fail)

  def list[S](elt: Grammar[Char, S]): Grammar[Char, List[S]] = token('(') ~> elt.zeroOrMore <~ token(')')

  def lisp[S](atom: Grammar[Char, S])(sList: List[S] => S): Grammar[Char, S] =
    list(lisp(atom)(sList)).map(sList) or atom

  def unit[S](sAtom: String => S, sString: String => S, sNumber: Int => S, sList: List[S] => S): Grammar[Char, S] =
    lisp(atom.map(sAtom) or number.map(sNumber) or string.map(sString))(sList)
}

