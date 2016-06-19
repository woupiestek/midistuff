package nl.woupiestek.midi.lispy

import nl.woupiestek.midi.parser.Grammar

object LGrammar {

  type G[T] = Grammar[Option[Char], T]

  def file: G[Track] = for {
    _ <- separator
    x <- new Context(Map.empty).track
  } yield x

  class Context(entries: Map[String, Track]) {
    def scalar(f: (Track, Int) => Track): G[Track] = for {
      x <- number
      y <- track
    } yield f(y, x)

    def trackListFold(f: (Track, Track) => Track): G[Track] = for {
      '[' <- capture
      _ <- separator
      ts <- track.oneOrMore
      ']' <- capture
      _ <- separator
    } yield ts.foldLeft(Track.empty)(f)

    val argumentParsers: Map[String, G[Track]] = Map(
      "note" -> (for {
        key <- natural
        duration <- natural
      } yield Track(duration, List((0, NoteOn(0, key, 60)), (duration, NoteOff(0, key))))),
      "rest" -> number.map(d => Track(d, Nil)),
      "patch" -> number.map(p => Track(0, List((0, ProgramChange(0, p))))),
      "seq" -> trackListFold(_ append _),
      "chord" -> trackListFold(_ stack _),
      "pui" -> scalar(_ piu _),
      "cresc" -> scalar(_ cresc _),
      "transpose" -> scalar(_ transpose _),
      "channel" -> scalar(_ toChannel _),
      "repeat" -> scalar((y, x) => (1 to x).map(_ => y).foldLeft(Track.empty)(_ append _)),
      "put" -> (for {
        x <- identifier
        y <- track
        z <- new Context(entries + (x -> y)).track
      } yield z),
      "get" -> identifier.collect(entries))

    def track: G[Track] = for {
      x <- method if argumentParsers contains x
      y <- argumentParsers(x)
    } yield y
  }

  def capture = Grammar.read[Option[Char]].collect { case Some(c) => c }

  //remove whitespace and comments
  def separator: G[Unit] = (for {
    x <- capture
    _ <- if (Set(' ', '\n', '\r', '\t', '\f').contains(x)) separator else if (';' == x) comment else Grammar.fail
  } yield ()) | Grammar.point(())

  def comment: G[Unit] = for {
    x <- capture
    _ <- if (Set('\n', '\r').contains(x)) separator else comment
  } yield ()

  def natural: G[Int] = select(('0' to '9').contains).map(_.toInt)

  def select(f: Char => Boolean): G[String] = for {
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
}
