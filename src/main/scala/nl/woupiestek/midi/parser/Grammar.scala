package nl.woupiestek.midi.parser

import scalaz.{-\/, \/, \/-}

final class Grammar[-In, +Out] private(ops: => List[Out \/ (In => Grammar[In, Out])]) {

  lazy val options = ops

  def flatMap[In2 <: In, Out2](f: Out => Grammar[In2, Out2]): Grammar[In2, Out2] =
    new Grammar(options.flatMap {
      case -\/(out) => f(out).options
      case \/-(g) => List(\/-((in: In) => g(in).flatMap(f)))
    })

  def map[Out2](f: Out => Out2): Grammar[In, Out2] =
    new Grammar(options.map(either => either.bimap(f, g => (in: In) => g(in).map(f))))

  def filter(f: Out => Boolean): Grammar[In, Out] =
    new Grammar[In, Out](options.collect {
      case -\/(out) if f(out) => -\/(out)
      case \/-(read) => \/-((in: In) => read(in).filter(f))
    })

  def withFilter(f: Out => Boolean) = filter(f)

  def |[In2 <: In, Out2 >: Out](simplifiedGrammar: Grammar[In2, Out2]): Grammar[In2, Out2] =
    new Grammar[In2, Out2](options ++ simplifiedGrammar.options)

  def collect[Out2](f: PartialFunction[Out, Out2]): Grammar[In, Out2] =
    new Grammar[In, Out2](options.collect {
      case -\/(out) if f.isDefinedAt(out) => -\/(f(out))
      case \/-(read) => \/-((in: In) => read(in).collect(f))
    })

  def zeroOrMore: Grammar[In, List[Out]] = oneOrMore | Grammar.point(Nil)

  def oneOrMore: Grammar[In, List[Out]] = for {
    x <- this
    y <- zeroOrMore
  } yield x :: y

  def zeroOrOne: Grammar[In, Option[Out]] = map(Some(_)) | Grammar.point(None)

  def andThen[Out2](grammar: Grammar[Out, Out2]): Grammar[In, Out2] = new Grammar[In, Out2](
    grammar.options.flatMap {
      case -\/(out2) => List(-\/(out2))
      case \/-(read2) => flatMap(out => andThen(read2(out))).options
    })

  def fold[P](f: List[Out \/ (In => P)] => P): P = {
    f(options.map {
      either => either.map {
        read => (in: In) => read(in).fold(f)
      }
    })
  }
}

object Grammar {

  def point[Out](out: Out): Grammar[Any, Out] = new Grammar(List(-\/(out)))

  def read[In]: Grammar[In, In] = new Grammar(List(\/-(point[In])))

  def collect[In, Out](f: PartialFunction[In, Out]) = read[In].collect(f)

  val fail: Grammar[Any, Nothing] = new Grammar(Nil)
}

