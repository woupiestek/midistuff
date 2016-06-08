package nl.woupiestek.midi.extended

import scala.concurrent.duration.Duration

trait Sequence

case class Elements(elements: List[Sequence.Element]) extends Sequence

//{S}=x.S
case class Put(value: Sequence, identifier: String, context: Sequence) extends Sequence

object Sequence {

  sealed trait Element

  case class Note(pitch: Int, duration: Int) extends Element

  case class Rest(duration: Int) extends Element

  case class Poly(list: List[Sequence]) extends Element

  case class Get(identifier: String) extends Element

}








