package nl.woupiestek.midi.lispy

import nl.woupiestek.midi.parser.Grammar

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
    "pui" -> scalar(_ piu _),
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

object Tokens {

  type G[T] = Grammar[Option[Char], T]

  private def capture = Grammar.read[Option[Char]].collect { case Some(c) => c }

  //remove whitespace and comments
  def separator: G[Unit] = (for {
    x <- capture
    _ <- if (Set(' ', '\n', '\r', '\t', '\f').contains(x)) separator else if (';' == x) comment else Grammar.fail
  } yield ()) | Grammar.point(())

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
    Some('-') <- Grammar.read[Option[Char]]
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

