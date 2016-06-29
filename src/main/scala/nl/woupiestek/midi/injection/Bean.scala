package nl.woupiestek.midi.injection

import scala.reflect.ClassTag

sealed trait Bean[T] {
  def flatMap[U](f: T => Bean[U]): Bean[U]

  def map[U] = flatMap(Pure(_))

  def inject(config: Traversable[Dependency]): Option[T]
}

case class Pure[T](t: T) extends Bean[T] {
  override def flatMap[U](f: (T) => Bean[U]): Bean[U] = f(t)

  override def inject(config: Traversable[Dependency]): Option[T] = Some(t)
}

case class With[T, U](key: Class[T], bean: T => Bean[U]) extends Bean[U] {
  override def flatMap[V](f: (U) => Bean[V]): Bean[V] = copy(bean = (t: T) => bean(t).flatMap(f))

  override def inject(config: Traversable[Dependency]): Option[U] = for {
    t <- config.find(dep => dep.key == key)
    u <- bean(t.value.asInstanceOf[T]).inject(config)
  } yield u
}

sealed trait Dependency {
  type T

  val key: Class[_]

  def value: T
}

case class Single[C: ClassTag](build: () => C) extends Dependency {
  override type T = C
  override lazy val value: T = build()
  override val key: Class[_] = implicitly[ClassTag[C]].runtimeClass
}

case class Many[C: ClassTag](build: () => C) extends Dependency {
  override type T = C
  override val key: Class[_] = implicitly[ClassTag[C]].runtimeClass

  override def value: T = build()
}
