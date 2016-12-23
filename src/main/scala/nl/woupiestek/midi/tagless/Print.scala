package nl.woupiestek.midi.tagless

class Print extends MidiTrack[String] {
  override def empty: String = ""

  override def append(list: Seq[String]): String = s"seq[${list.mkString(" ")}]"

  override def stack(list: Seq[String]): String = s"chord[${list.mkString(" ")}]"

  override def piu(gain: Int, track: String): String = s"piu $gain $track"

  override def cresc(gain: Int, track: String): String = s"cresc $gain $track"

  override def setChannel(channel: Int, track: String): String = s"channel $channel track"

  override def setTempo(beatsPerMinute: Int): String = s"tempo $beatsPerMinute"

  override def note(key: Int, duration: Int): String = s"note $key $duration"

  override def rest(duration: Int): String = s"rest $duration"

  override def setProgram(program: Int): String = s"patch $program"

  override def put(key: String, value: String, next: String): String = s"put $key $value\n$next"

  override def get(key: String): String = s"get $key"

  override def transpose(keys: Int, track: String): String = s"transpose $keys $track"
}
