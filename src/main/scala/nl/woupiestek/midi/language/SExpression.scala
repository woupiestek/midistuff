package nl.woupiestek.midi.language

trait SExpression[S] {
  def atom(h: String): S

  def string(h: String): S

  def number(i: Int): S

  def list(s: List[S]): S
}