package UsingScala

import scala.io.Source

/**
  * Created by root on 16-11-19.
  */
object ReadingURL {
  def main(args: Array[String]): Unit = {
    val source=Source.fromURL("http://www.scala-lang.org/docu/files/api/index.html")
    println(source.take(3))
    val content=source.mkString
    val VersionRegEx="""[\D\S]+scaladoc\s+\(version\s+(.+)\)[\D\S]+""".r
    content match {
      case VersionRegEx(vesion)=>println("Scala doc for version:"+vesion)
    }
  }
}
