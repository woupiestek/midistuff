package nl.woupiestek.midi.lispy

import java.lang.Character._

import nl.woupiestek.midi.parser.Rule
import nl.woupiestek.midi.parser.Rule._

sealed trait Token

case class Number(value: Int) extends Token

case class Identifier(name: String) extends Token

case object BeginList extends Token

case object EndList extends Token

case object BeginFile extends Token

case object EndFile extends Token

object Parser {

  type TG = Grammar[Token, Result]

  sealed trait Result {
    def flatMap(f: Track => Result): Result

    def map(f: Track => Track): Result = flatMap(track => Play(f(track)))
  }

  case class Get(key: String, next: Track => Result) extends Result {
    override def flatMap(f: (Track) => Result): Result = copy(next = track => next(track).flatMap(f))
  }

  case class Play(track: Track) extends Result {
    override def flatMap(f: (Track) => Result): Result = f(track)
  }

  case class Put(key: String, track: Track, next: Result) extends Result {
    override def flatMap(f: (Track) => Result): Result = copy(next = next.flatMap(f))
  }

  def file: Rule.Grammar[Char, Result] =
    Tokenizer.token andThen (for {
      BeginFile <- read[Token] //why does this do anything?
      x <- track
      EndFile <- read[Token]
    } yield x)

  def track: TG = {
    val number: Grammar[Token, Int] = read[Token].collect { case Number(x) => x }

    def scalar(f: (Track, Int) => Track): TG = for {
      x <- number
      y <- track
    } yield y.flatMap(track => Play(f(track, x)))

    def fold(f: (Track, Track) => Track): TG = for {
      BeginList <- read[Token]
      ts <- track.oneOrMore
      EndList <- read
    } yield ts.foldLeft[Result](Play(Track.empty)) {
      case (x, y) => for {
        u <- x
        v <- y
      } yield f(u, v)
    }

    val identifier: Grammar[Token, String] = read[Token].collect { case Identifier(x) => x }

    val argumentParsers: Map[String, TG] = Map(
      "note" -> (for {
        key <- number
        duration <- number
      } yield Play(Track(duration, List((0, NoteOn(0, key)), (duration, NoteOff(0, key)))))),
      "rest" -> number.map(d => Play(Track(d, Nil))),
      "patch" -> number.map(p => Play(Track(0, List((0, ProgramChange(0, p)))))),
      "tempo" -> number.map(t => Play(Track(0, List((0, Tempo(t)))))),
      "seq" -> fold(_ append _),
      "chord" -> fold(_ stack _),
      "piu" -> scalar(_ piu _),
      "cresc" -> scalar(_ cresc _),
      "transpose" -> scalar(_ transpose _),
      "channel" -> scalar(_ toChannel _),
      "repeat" -> scalar((y, x) => (1 to x).map(_ => y).foldLeft(Track.empty)(_ append _)),
      "put" -> (for {
        x <- identifier
        y <- track
        z <- track
      } yield y.flatMap(y2 => Put(x, y2, z))),
      "get" -> identifier.map(Get(_, Play)))

    for {
      x <- identifier
      y <- argumentParsers.getOrElse(x, fail)
    } yield y
  }
}

object Tokenizer {
  type TT = Grammar[Char, Token]

  def token: TT = (for {
    c <- read[Char]
    t <- rest(c)
    _ <- separator
  } yield t) or write(EndFile)

  private val reserved = Set('[', ']', ';', ' ', '\n', '\r', '\t', '\f')
  private val digit = read[Char].filter(('0' to '9').contains)

  def rest: Char => TT = {
    case '[' => write(BeginList)
    case ']' => write(EndList)
    case '-' => digit.oneOrMore.map(digits => Number(-digits.mkString.toInt))
    case first if isDigit(first) => digit.zeroOrMore.map(digits => Number((first :: digits).mkString.toInt))
    case first if isLetter(first) =>
      read[Char].filterNot(reserved.contains).zeroOrMore.map(digits => Identifier((first :: digits).mkString))
    case _ => fail
  }

  type TU = Grammar[Char, Unit]

  def comment: TU = for {
    ';' <- read[Char]
    _ <- read[Char].filterNot(Set('\n', '\r').contains).zeroOrMore
  } yield ()

  def space: TU = read[Char].filter(isWhitespace).map(_ => ())

  def separator: TU = (space or comment).zeroOrMore.map(_ => ())
}
