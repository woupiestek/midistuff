package nl.woupiestek.midi

trait MidiMessage[M] {

  def noteOn(channel: Int, pitch: Int, velocity: Int): M

  def noteOff(channel: Int, pitch: Int): M

  def setProgram(channel: Int, program: Int): M

}
