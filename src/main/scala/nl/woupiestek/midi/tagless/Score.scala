package nl.woupiestek.midi.tagless

trait Score[T] {
  def empty: T

  def setTempo(beatsPerMinute: Int): T

  def note(key: Int, duration: Int): T

  def rest(duration: Int): T

  def setProgram(program: Int): T

  def append(list: Seq[T]): T

  def stack(list: Seq[T]): T

  def piu(gain: Int, track: T): T

  def cresc(gain: Int, track: T): T

  def transpose(keys: Int, track: T): T

  def setChannel(channel: Int, track: T): T
}


trait Memorized[T] {
  def get(key: String): T

  def put(key: String, value: T, next: T): T
}

class Memorizer[T] extends Memorized[Map[String, T] => Option[T]] {
  override def get(key: String): (Map[String, T]) => Option[T] = _.get(key)

  override def put(key: String, value: (Map[String, T]) => Option[T], next: (Map[String, T]) => Option[T]): (Map[String, T]) => Option[T] =
    m => next(value(m) match {
      case Some(x) => m + (key -> x)
      case None => m
    })
}
