package nl.woupiestek.midi

object Score {

  sealed abstract class Temp(val duration: Double)

  case class Rest(override val duration: Double) extends Temp(duration)

  case class Note(override val duration: Double, pitch: Int) extends Temp(duration)

  def tracks(tempo: Double, channel: Int, velocity: Int)(temps: List[Temp]): List[Track] = {
    temps.foldLeft[(Double, List[Track])]((0, Nil)) {
      case ((x, y), Rest(z)) => (x + z * 60000.0 / tempo, y)
      case ((w, x), Note(y, z)) =>
        val w1 = w + y * 60000.0 / tempo
        (w1, Track.track(w.toLong, NoteOn(channel, z, velocity)) ::
          Track.track(w1.toLong, NoteOff(channel, z)) :: x)
    } match {
      case (_, ts) => ts
    }
  }
}




