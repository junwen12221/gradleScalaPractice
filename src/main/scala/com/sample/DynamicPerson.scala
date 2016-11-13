package com.sample

import scala.collection.immutable.HashMap
import scala.language.dynamics

/**
  * Created by root on 16-11-2.
  */
class DynamicPerson extends Dynamic {
  type CallFun = Int => String
  private var fields = HashMap.empty[String, Any].withDefault {
    key => throw new NoSuchFieldError(key)
  }
  private var functions = HashMap.empty[String, CallFun].withDefault {
    key => throw new NoSuchFieldError(key)
  }

  def selectDynamic(key: String) = fields(key)

  def updateDynamic(key: String)(args: Any): Unit = {
    args match {
      case x if key.startsWith("do") => functions += (key -> x.asInstanceOf[CallFun])
      case _ => fields += (key -> args)
    }
  }

  def applyDynamic(key: String)(args: Int) = {
    println(functions(key)(args))
  }

}

object SimpleDemo extends App {
  /*  val person = new DynamicPerson()
    person.Name = "Mike";
    person.doPrintInfo = (age: Int) => s"${person.Name} 今年 $age 岁"
    person.doPrintInfo(80);*/
  def sort(xs: Array[Int]): Array[Int] = {
    if (xs.length <= 1) xs
    else {
      val pivot = xs(xs.length / 2)
      Array.concat(
        sort(xs filter (pivot >)),
        xs filter (pivot ==),
        sort(xs filter (pivot >))
      )
    }
  }

  val r = for (i <- Range(1, 10)) yield i
  r.foreach(println)

  abstract class SemiGroup[A] {
    def add(x: A, y: A): A
  }

  abstract class Monoid[A] extends SemiGroup[A] {
    def unit: A
  }

  implicit object stringMonoid extends Monoid[String] {
    override def add(x: String, y: String): String = x.concat(y)

    override def unit: String = ""
  }

  implicit object intMonoid extends Monoid[Int] {
    override def add(x: Int, y: Int): Int = x + y

    override def unit: Int = 0
  }

  def sum[A](xs: List[A])(implicit m: Monoid[A]): A =
    if (xs.isEmpty) m.unit
    else m.add(xs.head, sum(xs.tail)(m))


  println(sum(List("a", "bc", "def")))
  println(sum(List(1, 2, 3)))


}