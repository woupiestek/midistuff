package nl.woupiestek.midi.injection

import scala.reflect.ClassTag
import scala.util.{Success, Try}

sealed trait Component[T] {
  def flatMap[U](f: T => Component[U]): Component[U]

  def map[U](f: T => U): Component[U] = flatMap(t => new Pure(f(t)))

  def initialize(implicit container: Container): Try[T]
}

class Pure[T](t: T) extends Component[T] {
  override def flatMap[U](f: (T) => Component[U]): Component[U] = f(t)

  override def initialize(implicit container: Container): Try[T] = Success(t)
}

class With[T, U](key: Class[T], build: T => Component[U]) extends Component[U] {
  override def flatMap[V](f: (U) => Component[V]): Component[V] = new With(key, (t: T) => build(t).flatMap(f))

  override def initialize(implicit container: Container): Try[U] = for {
    d <- container.dependency(key)
    b <- build(d).initialize(container)
  } yield b
}

object Container {
  def instance[T](key: Class[T]): Component[T] = new With[T, T](key, new Pure(_))

  def inject[T](implicit ct: ClassTag[T]): Component[T] = instance[T](ct.runtimeClass.asInstanceOf[Class[T]])
}

trait Container {
  def dependency[T](key: Class[T]): Try[T]
}
