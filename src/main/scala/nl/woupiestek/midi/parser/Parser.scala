package nl.woupiestek.midi.parser

import scalaz._
import scalaz.Scalaz._
import Parser._
import scala.collection.mutable

sealed abstract class Parser[-I, +O] {
  def value[F[_]: ApplicativePlus, P >: O]: F[P]
  def derive(i: I): Parser[I, O]

  def parse[F[_]: ApplicativePlus, G[_]: Foldable, J <: I, P >: O](
      input: G[J]
  ): F[P] = {
    def dm = memoized((p: Parser[I, O]) => memoized(p.derive(_)))
    def vm = memoized((_: Parser[I, O]).value[F, P])
    vm(input.foldLeft(this)(dm(_)(_)))
  }

}

object Parser {

  def suspend[I, O](parser: => Parser[I, O]): Parser[I, O] = new Parser[I, O] {
    def value[F[_]: ApplicativePlus, P >: O]: F[P] = parser.value[F, P]
    def derive(i: I): Parser[I, O] = parser.derive(i)
  }

  private case object Empty extends Parser[Any, Nothing] {
    def value[F[_]: ApplicativePlus, B >: Nothing] = ApplicativePlus[F].empty[B]
    def derive(i: Any) = this
  }

  private case class Point[+A](f: Need[A]) extends Parser[Any, A] {
    def value[F[_]: ApplicativePlus, B >: A] =
      ApplicativePlus[F].point[B](f.value)
    def derive(i: Any) = Empty
  }

  private case class End[-A, +B](fa: Parser[A, B]) extends Parser[A, B] {
    def value[F[_]: ApplicativePlus, C >: B] = fa.value[F, C]
    def derive(j: A) = Empty
  }

  implicit def isApplicativePlus[I]
      : ApplicativePlus[({ type G[O] = Parser[I, O] })#G] = {
    type G[O] = Parser[I, O]
    new ApplicativePlus[G] {
      def empty[A]: G[A] = Empty

      def point[A](a: => A): G[A] = Point(Need(a))

      def ap[A, B](fa: => G[A])(f: => G[A => B]): G[B] =
        new G[B] {
          def value[F[_]: ApplicativePlus, C >: B] = {
            ApplicativePlus[F].ap(fa.value[F, A])(f.value[F, A => C])
          }

          def derive(i: I) =
            plus(ap(fa.derive(i))(f), ap(End(fa))(f.derive(i)))
        }

      def plus[A](a: G[A], b: => G[A]): G[A] =
        if (a == Empty) b
        else
          new G[A] {
            def value[F[_]: ApplicativePlus, B >: A] =
              ApplicativePlus[F].plus(a.value[F, B], b.value[F, B])
            def derive(i: I) = plus(a.derive(i), b.derive(i))
          }
    }
  }

  implicit class applicativePlusOps[I, O](rule: Parser[I, O]) {
    def nel: Parser[I, NonEmptyList[O]] =
      isApplicativePlus.ap(rule)(
        rule.list.map(t => (h: O) => NonEmptyList.nel(h, t))
      )

    def list: Parser[I, IList[O]] =
      isApplicativePlus.plus(
        rule.nel.map(_.list),
        isApplicativePlus.point(IList.empty[O])
      )

    def maybe: Parser[I, Maybe[O]] =
      isApplicativePlus.plus(
        rule.map(_.point[Maybe]),
        isApplicativePlus.point(Maybe.empty[O])
      )
  }

  case class If[I](f: I => Boolean) {
    def one: Parser[I, I] =
      new Parser[I, I] {
        def value[F[_]: ApplicativePlus, J >: I] = ApplicativePlus[F].empty
        def derive(i: I) = if (f(i)) isApplicativePlus[I].point(i) else Empty
      }

    def scanMap[B](g: I => B)(implicit B: scalaz.Monoid[B]): Parser[I, B] =
      scanRight(B.zero)((i, b) => B.append(g(i), b))

    def scanRight[B](z: => B)(g: (I, => B) => B): Parser[I, B] =
      (one |@| suspend(scanRight(z)(g)))(g(_, _)) <+> isApplicativePlus[I]
        .point(z)
  }

  private def memoized[A, B](f: A => B): A => B = {
    val memo = mutable.Map.empty[A, B]

    (a: A) =>
      memo.getOrElse(a, {
        val b = f(a)
        memo += (a -> b)
        b
      })
  }
}
