package nl.woupiestek.midi.injection

import scala.util.{Success, Try}

sealed trait Component[T] {
  def flatMap[U](f: T => Component[U]): Component[U]

  def map[U](f: T => U): Component[U] = flatMap(t => new Pure(f(t)))

  def initialize(container: Container): Try[T]
}

class Pure[T](t: T) extends Component[T] {
  override def flatMap[U](f: (T) => Component[U]): Component[U] = f(t)

  override def initialize(container: Container): Try[T] = Success(t)
}

class With[T, U](key: Class[T], build: T => Component[U]) extends Component[U] {
  override def flatMap[V](f: (U) => Component[V]): Component[V] = new With(key, (t: T) => build(t).flatMap(f))

  override def initialize(container: Container): Try[U] = for {
    d <- container.dependency(key)
    b <- build(d).initialize(container)
  } yield b
}

object Container {
  def inject[T](key: Class[T]): With[T, T] = new With(key, new Pure(_))
}

trait Container {
  def dependency[T](key: Class[T]): Try[T]
}
