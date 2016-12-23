package nl.woupiestek.midi.tagless

import nl.woupiestek.midi.lispy.{BeginList, EndList}
import nl.woupiestek.midi.parser.Grammar
import nl.woupiestek.midi.parser.Grammar.{fail, read}

class Parser[LToken, Track](implicit d: Tokenizer[LToken], e: MidiTrack[Track]) {

  type TG = Grammar[LToken, Track]

  def file: TG = for {
    _ <- d.beginFile
    x <- track
    _ <- read[LToken]
  } yield x

  def scalar(f: (Int, Track) => Track): TG = for {
    x <- d.number
    y <- track
  } yield f(x, y)

  def fold(f: Seq[Track] => Track): TG = for {
    BeginList <- read[LToken]
    ts <- track.oneOrMore
    EndList <- read[LToken]
  } yield f(ts)

  val argumentParsers: Map[String, TG] = Map(
    "note" -> (for {
      key <- d.number
      duration <- d.number
    } yield e.note(key, duration)),
    "rest" -> d.number.map(e.rest),
    "patch" -> d.number.map(e.setProgram),
    "tempo" -> d.number.map(e.setTempo),
    "seq" -> fold(e.append),
    "chord" -> fold(e.stack),
    "piu" -> scalar(e.piu),
    "cresc" -> scalar(e.cresc),
    "transpose" -> scalar(e.transpose),
    "channel" -> scalar(e.setChannel),
    "repeat" -> scalar((x, y) => e.append(Seq.fill(x)(y))),
    "put" -> (for {
      x <- d.identifier
      y <- track
      z <- track
    } yield e.put(x, y, z)),
    "get" -> d.identifier.map(e.get))

  def track: TG = for {
    x <- d.identifier
    y <- argumentParsers.getOrElse(x, fail)
  } yield y

}

