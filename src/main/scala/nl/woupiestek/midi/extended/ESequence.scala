package nl.woupiestek.midi.extended

trait ESequence

case class Elements(elements: List[ESequence.Element]) extends ESequence

//{S}=x.S
case class Put(value: ESequence, key: String, context: ESequence) extends ESequence

object ESequence {

  sealed trait Element

  case class Note(pitch: Int, duration: Int) extends Element

  case class Rest(duration: Int) extends Element

  case class Poly(list: List[ESequence]) extends Element

  case class Get(identifier: String) extends Element

}