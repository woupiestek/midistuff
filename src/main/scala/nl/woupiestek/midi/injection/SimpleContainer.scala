package nl.woupiestek.midi.injection

import scala.reflect.ClassTag
import scala.util.Try

class SimpleContainer private(dependencies: Map[Class[_], _]) extends Container {
  override def dependency[T](key: Class[T]): Try[T] = {
    Try(dependencies(key).asInstanceOf[T])
  }

  def add[T](t: T)(implicit ct: ClassTag[T]): SimpleContainer =
    new SimpleContainer(dependencies + (ct.runtimeClass -> t))
}

object SimpleContainer {
  val empty = new SimpleContainer(Map.empty)

  def add[T](t: T)(implicit ct: ClassTag[T]): SimpleContainer =
    new SimpleContainer(Map(ct.runtimeClass -> t))
}

