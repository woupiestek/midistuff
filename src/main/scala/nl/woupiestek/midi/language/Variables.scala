package nl.woupiestek.midi.language

trait Variables[I, T] {

  def variable(id: I): T

  def let(id: T, value: T, context: T): T

}

//U -> V?
trait Functions[I, T] {

  def abstraction(id: I, term: T): T

  def application(operator: T, operand: T): T

}

trait Command[C] {
  def noteOn: C

  def noteOff: C

  def keyPressure: C

  def channelPressure: C

  def controlChange: C

  def programChange: C

  def pitchWheelChange: C
}

trait Track[C, T] {

  def single(command: C, channel: Int, arg0: Int, arg1: Int, tick: Int): T

  def empty: T

  def merge(x: T, y: T): T

}