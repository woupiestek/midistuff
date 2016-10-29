package nl.woupiestek.midi.parser

object StringParser {

  type G[X] = Grammar[Option[Char], X]

  def parse[X](input: String, grammar: G[X]): Option[X] = new First(input).fold(grammar)(0)

  class First[X](input: String) extends Folder[Option[Char], X, Int => Option[X]] {
    override def onFail: (Int) => Option[X] = _ => None

    override def onPoint(out: X, or: => (Int) => Option[X]): (Int) => Option[X] = _ => Some(out)

    override def onRead(read: (Option[Char]) => (Int) => Option[X], or: => (Int) => Option[X]): (Int) => Option[X] =
      i => read(option(i))(i + 1) orElse or(i)

    private def option(i: Int): Option[Char] = {
      Some(i).collect { case j if j >= 0 && j < input.length => input.charAt(j) }
    }
  }

}