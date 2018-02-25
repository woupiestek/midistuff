package nl.woupiestek.midi.parser

import scalaz.Monoid

import scala.annotation.tailrec

sealed trait Rule[I, O]

object Rule {

  case class Read[I, O](f: I => List[Rule[I, O]]) extends Rule[I, O]

  case class Write[I, O](o: O) extends Rule[I, O]

  def write[I, O](o: O): Grammar[I, O] = Grammar(List(Write(o)))

  def read[I]: Grammar[I, I] = Grammar(List(Read[I, I](i => List(Write(i)))))

  //def lift[I,O](f: I => List[Rule[I, O]]): Grammar[I,O] = Grammar(List(Read(f)))

  def fail[I, O]: Grammar[I, O] = Grammar(Nil)

  case class Grammar[I, O](rules: List[Rule[I, O]]) {
    def fold[Z](onWrite: O => Z, onRead: (I => Z) => Z)(implicit Z: Monoid[Z]): Z = {
      def start(rs: List[Rule[I, O]]) = helper(rs, Z.zero)

      @tailrec def helper(rs: List[Rule[I, O]], result: Z): Z = rs match {
        case Nil => result
        case Write(o) :: t => helper(t, Z.append(result, onWrite(o)))
        case Read(g) :: t => helper(t, Z.append(result, onRead((i: I) => start(g(i)))))
      }

      start(rules)
    }

    def foldList[Z](onWrite: O => List[Z], onRead: (I => List[Z]) => List[Z]): List[Z] = {
      def f(rs: List[Rule[I, O]]): List[Z] = rs.flatMap {
        case Read(g) => onRead(g andThen f)
        case Write(o) => onWrite(o)
      }

      f(rules)
    }

    def flatMap[O2](f: O => Grammar[I, O2]): Grammar[I, O2] =
      Grammar(foldList(f(_).rules, (g: I => List[Rule[I, O2]]) => List(Read(g))))

    def map[O2](f: O => O2): Grammar[I, O2] = flatMap(f andThen write)

    def collect[O2](f: PartialFunction[O, O2]): Grammar[I, O2] = flatMap((o: O) => if (f.isDefinedAt(o)) write(f(o)) else fail)

    def filter(f: O => Boolean): Grammar[I, O] = flatMap((o: O) => if (f(o)) write(o) else fail)

    def withFilter(f: O => Boolean): Grammar[I, O] = filter(f)

    def or(other: Grammar[I, O]): Grammar[I, O] = Grammar(rules ++ other.rules)

    def zeroOrMore: Grammar[I, List[O]] = oneOrMore or write(Nil)

    def oneOrMore: Grammar[I, List[O]] = flatMap(h => zeroOrMore.map(h :: _))

    def zeroOrOne: Grammar[I, Option[O]] = map[Option[O]](Some(_)) or write(None)

    def andThen[O2](next: Grammar[O, O2]): Grammar[I, O2] =
      Grammar[I, O2](next.foldList[Rule[I, O2]](
        (o: O2) => List(Write(o)),
        (f: O => List[Rule[I, O2]]) => foldList(f, (g: I => List[Rule[I, O2]]) => List(Read(g)))
      ))
  }

}