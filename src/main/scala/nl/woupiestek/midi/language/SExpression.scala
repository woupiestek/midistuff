package nl.woupiestek.midi.language

import java.lang.Character._

import nl.woupiestek.midi.parser.Rule._

trait SExpression[S] {
  def atom(h: String): S

  def string(h: String): S

  def number(i: Int): S

  def list(s: List[S]): S
}

class SParser[S](implicit S: SExpression[S]) {

  private val whiteSpace: Grammar[Char, Unit] = read[Char].filter(isWhitespace).zeroOrMore.map(_ => ())

  private val atom: Grammar[Char, S] = for {
    a <- read[Char].filter(isJavaIdentifierPart).oneOrMore
    _ <- whiteSpace
  } yield S.atom(a.mkString)

  private val string: Grammar[Char, S] = {
    def rest: Grammar[Char, List[Char]] = read[Char].flatMap {
      case '"' => write(Nil)
      case '\\' => read[Char].flatMap(h => rest.map(h :: _))
      case h => rest.map(h :: _)
    }

    read[Char].flatMap(x => if (x == '"') rest.map(r => S.string(r.mkString)) else fail)
  }

  private val number: Grammar[Char, S] =
    read[Char].filter(('0' to '9').contains).oneOrMore.map(t => S.number(t.foldLeft(0)(10 * _ + _ - '0')))

  private def token(c: Char): Grammar[Char, Unit] = read[Char].flatMap(d => if (c == d) whiteSpace else fail)

  private def list: Grammar[Char, S] = for {
    _ <- token('(')
    s <- unit.zeroOrMore
    _ <- token(')')
  } yield S.list(s)

  val unit: Grammar[Char, S] = atom or number or string or list
}