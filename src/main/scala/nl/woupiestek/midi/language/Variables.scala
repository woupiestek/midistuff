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

trait Event[M] {

  def noteOn(key: Int, velocity: Int): M

  def noteOff(key: Int, velocity: Int): M

  def keyPressure(key: Int, pressure: Int): M

  def channelPressure(pressure: Int): M

  def setControl(controller: Int, value: Int): M

  def setProgram(program: Int): M

  def setPitchWheel(pos0: Int, pos1: Int): M
}

trait Track[M, T] {

  def single(tick: Int, channel: Int, message: M): T

  def empty: T

  def merge(x: T, y: T): T

}