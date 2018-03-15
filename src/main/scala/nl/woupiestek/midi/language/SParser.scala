package nl.woupiestek.midi.language

import java.lang.Character._

import nl.woupiestek.midi.parser.Rule._

object SParser {

  private val lispComment: Grammar[Char, Unit] =
    token(';') ~> fixPoint[Char, Unit](x => read[Char].map(c => if (c == '\n' || c == '\r') write(()) else x))

  val separator: Grammar[Char, Unit] =
    (read[Char].filter(isWhitespace).map(_ => ()) or lispComment).zeroOrMore.map(_ => ())

  private val jIdentifier: Grammar[Char, String] =
    (read[Char].filter(isJavaIdentifierStart) ~
      read[Char].filter(isJavaIdentifierPart).zeroOrMore)((a, b) => (a :: b).mkString) <~
      separator

  private val string: Grammar[Char, String] =
    read[Char].flatMap(x => if (x == '"') fixPoint[Char, List[Char]](rest => read[Char].flatMap {
      case '"' => write(Nil)
      case '\\' => read[Char].flatMap(h => rest.map(h :: _))
      case h => rest.map(h :: _)
    }).map(_.mkString)
    else fail)

  private val number: Grammar[Char, Int] =
    read[Char].filter(('0' to '9').contains).oneOrMore.map(_.foldLeft(0)(10 * _ + _ - '0'))

  def token(c: Char): Grammar[Char, Unit] = read[Char].flatMap(d => if (c == d) separator else fail)

  private def list[S](elt: Grammar[Char, S]): Grammar[Char, List[S]] = token('(') ~> elt.zeroOrMore <~ token(')')

  private def lisp[S](atom: Grammar[Char, S])(implicit sList: List[S] => S): Grammar[Char, S] =
    list(lisp(atom)).map(sList) or atom

  def unit[S](implicit sAtom: String => S, sString: String => S, sNumber: Int => S, sList: List[S] => S): Grammar[Char, S] =
    lisp(jIdentifier.map(sAtom) or number.map(sNumber) or string.map(sString))
}

