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

      def separator: G[Unit] = for {
        _ <- space
        _ <- comment.zeroOrMore
      } yield ()

      def space: G[Unit] = Grammar.read[Option[Char]].collect {
        case Some(c) if Set(' ', '\n', '\r', '\t', '\f').contains(c) => c
      }.zeroOrMore.map(_ => ())

      def comment: G[String] = {
        def lineSep(x: Char): Boolean = x == '\n' || x == '\r'
        for {
          Some(';') <- Grammar.read[Option[Char]]
          x <- Grammar.read[Option[Char]].collect {
            case Some(x) if !lineSep(x) => x
          }.zeroOrMore
          Some(y) <- Grammar.read[Option[Char]] if lineSep(y)
          _ <- space
        } yield x.mkString
      }

      def natural: G[Int] = select(('0' to '9').contains).map(_.toInt)

      def select(f: Char => Boolean): G[String] = for {
        x <- Grammar.read[Option[Char]].collect { case Some(c) if f(c) => c }.oneOrMore
        _ <- separator
      } yield x.mkString

      def number: G[Int] = natural | (for {
        Some('-') <- Grammar.read[Option[Char]]
        n <- natural
      } yield -n)

      def method: G[String] = select(('a' to 'z').contains)

      def identifier: G[String] = select(('<' to '~').contains)

      for {
        Some('(') <- Grammar.read[Option[Char]]
        _ <- separator
        name <- method
        t <- args(name)
        Some(')') <- Grammar.read[Option[Char]]
        _ <- separator
      } yield t
    }
  }

}
