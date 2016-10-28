package nl.woupiestek.midi.lispy

import nl.woupiestek.midi.parser.Grammar
import nl.woupiestek.midi.parser.Grammar._


object LGrammar {
  val header_0_1 = "midistuff-file-version-0.1"

  def file: Grammar[Option[Char], Track] = for {
    header <- Tokens.identifier if header_0_1.equalsIgnoreCase(header)
    x <- new Context(Map.empty).track
  } yield x
}

class Context(entries: Map[String, Track]) {
  type TrackG = Grammar[Option[Char], Track]

  private def scalar(f: (Track, Int) => Track): TrackG = for {
    x <- Tokens.number
    y <- track
  } yield f(y, x)

  private def trackListFold(f: (Track, Track) => Track): TrackG = for {
    _ <- Tokens.leftBracket
    ts <- track.oneOrMore
    _ <- Tokens.rightBracket
  } yield ts.foldLeft(Track.empty)(f)

  private val argumentParsers: Map[String, TrackG] = Map(
    "note" -> (for {
      key <- Tokens.natural
      duration <- Tokens.natural
    } yield Track(duration, List((0, NoteOn(0, key, 60)), (duration, NoteOff(0, key))))),
    "rest" -> Tokens.number.map(d => Track(d, Nil)),
    "patch" -> Tokens.number.map(p => Track(0, List((0, ProgramChange(0, p))))),
    "tempo" -> Tokens.number.map(t => Track(0, List((0, Tempo(t))))),
    "seq" -> trackListFold(_ append _),
    "chord" -> trackListFold(_ stack _),
    "piu" -> scalar(_ piu _),
    "cresc" -> scalar(_ cresc _),
    "transpose" -> scalar(_ transpose _),
    "channel" -> scalar(_ toChannel _),
    "repeat" -> scalar((y, x) => (1 to x).map(_ => y).foldLeft(Track.empty)(_ append _)),
    "put" -> (for {
      x <- Tokens.identifier
      y <- track
      z <- new Context(entries + (x -> y)).track
    } yield z),
    "get" -> Tokens.identifier.collect(entries))

  def track: TrackG = for {
    x <- Tokens.method if argumentParsers contains x
    y <- argumentParsers(x)
  } yield y
}

sealed trait Token

object Token {

  case class Number(value: Int) extends Token

  case class Method(name: String) extends Token

  case class Identifier(name: String) extends Token

  case object BeginList extends Token

  case object EndList extends Token

  case object BeginFile extends Token

  case object EndFile extends Token


  def file: Grammar[Option[Char], Track] =
    Tokenizer.token andThen (for {
      BeginFile <- read[Token]
      x <- new Context(Map.empty).track
      EndFile <- read[Token]
    } yield x)

  class Context(entries: Map[String, Track]) {

    type TG = Grammar[Token, Track]

    private def scalar(f: (Track, Int) => Track): TG = for {
      x <- collect[Token, Int] { case Number(w) => w }
      y <- track
    } yield f(y, x)

    private def fold(f: (Track, Track) => Track): TG = for {
      BeginList <- read[Token]
      ts <- track.oneOrMore
      EndList <- read
    } yield ts.foldLeft(Track.empty)(f)

    private val argumentParsers: Map[String, TG] = Map(
      "note" -> (for {
        key <- collect[Token,Int] { case Number(x) => x }
        duration <- collect[Token,Int] { case Number(x) => x }
      } yield Track(duration, List((0, NoteOn(0, key, 60)), (duration, NoteOff(0, key))))),
      "rest" -> collect { case Number(d) => Track(d, Nil) },
      "patch" -> collect { case Number(p) => Track(0, List((0, ProgramChange(0, p)))) },
      "tempo" -> collect { case Number(t) => Track(0, List((0, Tempo(t)))) },
      "seq" -> fold(_ append _),
      "chord" -> fold(_ stack _),
      "piu" -> scalar(_ piu _),
      "cresc" -> scalar(_ cresc _),
      "transpose" -> scalar(_ transpose _),
      "channel" -> scalar(_ toChannel _),
      "repeat" -> scalar((y, x) => (1 to x).map(_ => y).foldLeft(Track.empty)(_ append _)),
      "put" -> (for {
        x <- collect[Token,String] { case Identifier(x) => x }
        y <- track
        z <- new Context(entries + (x -> y)).track
      } yield z),
      "get" -> collect[Token,String] { case Identifier(x) => x }.collect(entries))

    def track: TG = for {
      x <- collect[Token,String] { case Method(name) => name }
      y <- argumentParsers.getOrElse(x, fail)
    } yield y
  }

  object Tokenizer {
    type TT = Grammar[Option[Char], Token]

    def token: TT = number | method | identifier | beginList | endList | beginFile | endFile

    def number: TT = natural | (for {
      Some('-') <- read[Option[Char]]
      Number(n) <- natural
    } yield Number(-n))

    private def natural: TT = for {
      digits <- collect[Option[Char], Char] { case Some(c) if ('0' to '9').contains(c) => c }.oneOrMore
      _ <- separator
    } yield Number(digits.mkString.toInt)

    def method: TT = for {
      digits <- collect[Option[Char], Char] { case Some(c) if ('a' to 'z').contains(c) => c }.oneOrMore
      _ <- separator
    } yield Method(digits.mkString)

    private def reserved = Set('[', ']', ';', '-', ' ', '\n', '\r', '\t', '\f')

    def identifier: TT = for {
      digits <- collect[Option[Char], Char] { case Some(c) if !reserved.contains(c) => c }.oneOrMore
      _ <- separator
    } yield Identifier(digits.mkString)

    def beginList: TT = for {
      t <- collect[Option[Char], Token] { case Some('[') => BeginList }
      _ <- separator
    } yield t

    def endList: TT = for {
      t <- collect[Option[Char], Token] { case Some(']') => EndList }
      _ <- separator
    } yield t

    def beginFile: TT = for {
      _ <- separator
      "midistuff-file-version-0.1" <- collect[Option[Char], Char] { case Some(c) => c }.oneOrMore.map(_.mkString)
      _ <- separator
    } yield BeginFile

    def endFile: TT = collect { case None => EndFile }

    type TU = Grammar[Option[Char], Unit]

    def newLine: TU = collect { case Some(c) if c == '\n' || c == '\r' => () }

    def comment: TU = for {
      Some(';') <- read[Option[Char]]
      _ <- read[Option[Char]].zeroOrMore
      _ <- newLine
    } yield ()

    def space: TU = collect { case Some(c) if Character.isWhitespace(c) => () }

    def separator: TU = (space | comment).zeroOrMore.map(_ => ())

  }

}

object Tokens {

  type G[T] = Grammar[Option[Char], T]

  private def capture: G[Char] = collect { case Some(c) => c }

  //remove whitespace and comments
  def separator: G[Unit] = (for {
    x <- capture
    _ <- if (Set(' ', '\n', '\r', '\t', '\f').contains(x)) separator else if (';' == x) comment else fail
  } yield ()) | point(())

  private def comment: G[Unit] = for {
    x <- capture
    _ <- if ('\n' == x || '\r' == x) separator else comment
  } yield ()

  def natural: G[Int] = select(('0' to '9').contains).map(_.toInt)

  private def select(f: Char => Boolean): G[String] = for {
    x <- capture.filter(f).oneOrMore
    _ <- separator
  } yield x.mkString

  def number: G[Int] = natural | (for {
    Some('-') <- read[Option[Char]]
    n <- natural
  } yield -n)

  def method: G[String] = select(('a' to 'z').contains)

  private val reserved = Set('[', ']', ';', ' ', '\n', '\r', '\t', '\f')

  def identifier: G[String] = select(!reserved.contains(_))

  private def single(c: Char): G[Unit] = for {
    d <- capture if c == d
    _ <- separator
  } yield ()

  def leftBracket: G[Unit] = single('[')

  def rightBracket: G[Unit] = single(']')
}

