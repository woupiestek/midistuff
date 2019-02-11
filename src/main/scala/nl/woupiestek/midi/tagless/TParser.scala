package nl.woupiestek.midi.tagless

import scalaz._
import Scalaz._
import nl.woupiestek.midi.parser.Parser
import nl.woupiestek.midi.parser.Parser._

class TParser[Token, Track](
  implicit
  d: Tokenizer[Token],
  e: Score[Track],
  f: Memorized[Track]) {

  type TG = Parser[Token, Track]

  def file: TG = d.beginFile *> track <* d.endFile

  def scalar(f: (Int, Track) => Track): TG =
    (d.number |@| track)(f)

  def fold(f: Seq[Track] => Track): TG =
    d.beginList *> track.list.map(x => f(x.toList)) <* d.endList

  val argumentParsers: Map[String, TG] = Map(
    "note" -> (d.number |@| d.number)(e.note),
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
    "put" -> (d.identifier |@| track |@| track)(f.put),
    "get" -> d.identifier.map(f.get))

  def track: TG = {
    implicit val monoid: Monoid[TG] =
      ApplicativePlus[({ type P[X] = Parser[Token, X] })#P].monoid
    argumentParsers.toList.foldMap { case (k, v) => d.keyword(k) *> v }
  }
}
