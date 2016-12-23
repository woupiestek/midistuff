package nl.woupiestek.midi.tagless

trait MidiTrack[T] {
  def empty: T

  def setTempo(beatsPerMinute: Int): T

  def note(key: Int, duration: Int): T

  def rest(duration: Int): T

  def setProgram(program: Int): T

  def append(list: Seq[T]): T

  def stack(list: Seq[T]): T

  def piu(gain: Int, track: T): T

  def cresc(gain: Int, track: T): T

  def transpose(keys: Int, track:T):T

  def setChannel(channel: Int, track: T): T

  def get(key: String): T

  def put(key: String, value: T, next: T): T
}
