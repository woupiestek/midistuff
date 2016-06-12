package nl.woupiestek.midi.lispy

import nl.woupiestek.midi.parser.Grammar

object LGrammar {

  type G[T] = Grammar[Option[Char], T]

  def track: G[Track] = {

    def space: G[Unit] = Grammar.read[Option[Char]].collect {
      case Some(c) if Set(' ', '\n', '\r', '\t', '\f').contains(c) => c
    }.zeroOrMore.map(_ => ())

    def paren[T](g: G[T]): G[T] = for {
      Some('(') <- Grammar.read[Option[Char]]
      _ <- space
      x <- g
      Some(')') <- Grammar.read[Option[Char]]
      _ <- space
    } yield x

    def number: G[Int] = for {
      sign <- Grammar.read[Option[Char]].collect { case Some('-') => () }.zeroOrOne
      x <- Grammar.read[Option[Char]].collect { case Some(c) if ('0' to '9').contains(c) => c }.oneOrMore
      _ <- space
    } yield (if (sign.isEmpty) 1 else -1) * x.mkString.toInt

    def small: G[String] = for {
      x <- Grammar.read[Option[Char]].collect { case Some(c) if ('a' to 'z').contains(c) => c }.oneOrMore
      _ <- space
    } yield x.mkString

    def note: G[Track] = for {
      "note" <- small
      key <- number
      duration <- number
    } yield Track(duration, List((0, NoteOn(0, key, 60)), (duration, NoteOff(0, key))))
    def rest: G[Track] = for {
      "rest" <- small
      key <- number
      duration <- number
    } yield Track(duration, Nil)

    def seq: G[Track] = for {
      "seq" <- small
      tracks <- track.zeroOrMore
    } yield tracks.foldLeft(Track.empty)(_ append _)
    def chord: G[Track] = for {
      "chord" <- small
      tracks <- track.zeroOrMore
    } yield tracks.foldLeft(Track.empty)(_ stack _)

    def piu: G[Track] = for {
      "piu" <- small
      x <- number
      y <- track
    } yield y piu x
    def cresc: G[Track] = for {
      "cresc" <- small
      x <- number
      y <- track
    } yield y cresc x
    def transpose: G[Track] = for {
      "transpose" <- small
      x <- number
      y <- track
    } yield y transpose x
    def channel: G[Track] = for {
      "channel" <- small
      x <- number
      y <- track
    } yield y toChannel x

    paren(note | rest | seq | chord | piu | cresc | transpose | channel)
  }


}
