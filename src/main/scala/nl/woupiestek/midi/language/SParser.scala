package nl.woupiestek.midi.language

import java.lang.Character._

import nl.woupiestek.midi.parser.Rule.{ Grammar, fail, read, write }

object SParser {

  private val whiteSpace: Grammar[Char, Unit] = read[Char].filter(isWhitespace).zeroOrMore.map(_ => ())

  private val jIdentifier: Grammar[Char, String] = for {
    a <- read[Char].filter(isJavaIdentifierStart)
    b <- read[Char].filter(isJavaIdentifierPart).zeroOrMore
    _ <- whiteSpace
  } yield (a :: b).mkString

  private val string: Grammar[Char, String] = {
    def rest: Grammar[Char, List[Char]] = read[Char].flatMap {
      case '"' => write(Nil)
      case '\\' => read[Char].flatMap(h => rest.map(h :: _))
      case h => rest.map(h :: _)
    }

    read[Char].flatMap(x => if (x == '"') rest.map(_.mkString) else fail)
  }

  private val number: Grammar[Char, Int] =
    read[Char].filter(('0' to '9').contains).oneOrMore.map(_.foldLeft(0)(10 * _ + _ - '0'))

  private def token(c: Char): Grammar[Char, Unit] = read[Char].flatMap(d => if (c == d) whiteSpace else fail)

  private def lisp[S](atom: Grammar[Char, S])(implicit sList: List[S] => S): Grammar[Char, S] = (for {
    _ <- token('(')
    s <- lisp(atom).zeroOrMore
    _ <- token(')')
  } yield sList(s)) or atom

  def unit[S](implicit sAtom: String => S, sString: String => S, sNumber: Int => S, sList: List[S] => S): Grammar[Char, S] =
    lisp(jIdentifier.map(sAtom) or number.map(sNumber) or string.map(sString))
}
