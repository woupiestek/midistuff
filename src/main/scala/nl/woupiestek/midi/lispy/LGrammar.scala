package nl.woupiestek.midi.lispy

import nl.woupiestek.midi.parser.Grammar

object LGrammar {

  type G[T] = Grammar[Option[Char], T]

  class Context(entries: Map[String, Track]) {
    def track: G[Track] = {
      def args(name: String): G[Track] = name match {
        case "note" => for {
          key <- natural
          duration <- natural
        } yield Track(duration, List((0, NoteOn(0, key, 60)), (duration, NoteOff(0, key))))
        case "rest" => number.map(d => Track(d, Nil))
        case "seq" => track.zeroOrOne.map(_.foldLeft(Track.empty)(_ append _))
        case "chord" => track.zeroOrOne.map(_.foldLeft(Track.empty)(_ stack _))
        case "pui" => for {
          x <- number
          y <- track
        } yield y piu x
        case "cresc" => for {
          x <- number
          y <- track
        } yield y cresc x
        case "transpose" => for {
          x <- number
          y <- track
        } yield y transpose x
        case "channel" => for {
          x <- natural
          y <- track
        } yield y toChannel x
        case "put" => for {
          x <- identifier
          y <- track
          z <- new Context(entries + (x -> y)).track
        } yield z
        case "get" => identifier.collect(entries)
        case _ => Grammar.fail
      }

      for {
        '(' <- capture
        _ <- separator
        name <- method
        t <- args(name)
        ')' <- capture
        _ <- separator
      } yield t
    }
  }

  def capture = Grammar.read[Option[Char]].collect { case Some(c) => c }

  //remove whitespace and comments
  def separator: G[Unit] = for {
    x <- capture
    _ <- if (Set(' ', '\n', '\r', '\t', '\f').contains(x)) separator else if (';' == x) comment else Grammar.point(())
  } yield ()

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

  private val reserved = Set('(', ')', ';', ' ', '\n', '\r', '\t', '\f')

  def identifier: G[String] = select(!reserved.contains(_))
}
