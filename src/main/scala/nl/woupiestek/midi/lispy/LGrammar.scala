package nl.woupiestek.midi.lispy

import nl.woupiestek.midi.parser.Grammar
import nl.woupiestek.midi.parser.Grammar._

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

  //case class Result(track: Track, context: Map[String, Track] = Map.empty)

  def file: Grammar[Option[Char], Result] =
    Tokenizer.token andThen (for {
      BeginFile <- read[Token]
      x <- track
      EndFile <- read[Token]
    } yield x)


  def track: TG = {

    def scalar(f: (Track, Int) => Track): TG = for {
      x <- collect[Token, Int] { case Number(w) => w }
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

    val argumentParsers: Map[String, TG] = Map(
      "note" -> (for {
        key <- collect[Token, Int] { case Number(x) => x }
        duration <- collect[Token, Int] { case Number(x) => x }
      } yield Play(Track(duration, List((0, NoteOn(0, key, 60)), (duration, NoteOff(0, key)))))),
      "rest" -> collect { case Number(d) => Play(Track(d, Nil)) },
      "patch" -> collect { case Number(p) => Play(Track(0, List((0, ProgramChange(0, p))))) },
      "tempo" -> collect { case Number(t) => Play(Track(0, List((0, Tempo(t))))) },
      "seq" -> fold(_ append _),
      "chord" -> fold(_ stack _),
      "piu" -> scalar(_ piu _),
      "cresc" -> scalar(_ cresc _),
      "transpose" -> scalar(_ transpose _),
      "channel" -> scalar(_ toChannel _),
      "repeat" -> scalar((y, x) => (1 to x).map(_ => y).foldLeft(Track.empty)(_ append _)),
      "put" -> (for {
        x <- collect[Token, String] { case Identifier(x) => x }
        y <- track
        z <- track
      } yield y.flatMap(y2 => Put(x,y2,z))),
      "get" -> collect { case Identifier(x) => Get(x,Play) })

    for {
      x <- collect[Token, String] { case Identifier(name) => name }
      y <- argumentParsers.getOrElse(x, fail)
    } yield y
  }
}

object Tokenizer {
  type TT = Grammar[Option[Char], Token]

  def token: TT = for {
    option <- read[Option[Char]]
    t <- option match {
      case None => point(EndFile)
      case Some(c) => for {
        t <- rest(c)
        _ <- separator
      } yield t
    }
  } yield t

  private def reserved = Set('[', ']', ';', ' ', '\n', '\r', '\t', '\f')

  def rest(first: Char): TT = {
    if ('[' == first) point(BeginList)
    else if (']' == first) point(EndList)
    else if ('-' == first) {
      for {
        digits <- collect[Option[Char], Char] { case Some(c) if ('0' to '9').contains(c) => c }.oneOrMore
      } yield Number(-digits.mkString.toInt)
    } else if (('0' to '9').contains(first)) {
      for {
        digits <- collect[Option[Char], Char] { case Some(c) if ('0' to '9').contains(c) => c }.zeroOrMore
      } yield Number((first :: digits).mkString.toInt)
    } else if (Character.isAlphabetic(first)) {
      for {
        digits <- collect[Option[Char], Char] { case Some(c) if !reserved.contains(c) => c }.zeroOrMore
      } yield Identifier((first :: digits).mkString)
    } else fail
  }

  type TU = Grammar[Option[Char], Unit]

  def comment: TU = for {
    Some(';') <- read[Option[Char]]
    _ <- read[Option[Char]].filter(option => !option.contains('\n') && !option.contains('\r')).zeroOrMore
  } yield ()

  def space: TU = collect[Option[Char], Unit] { case Some(c) if Character.isWhitespace(c) => () }

  def separator: TU = (space | comment).zeroOrMore.map(_ => ())
}
