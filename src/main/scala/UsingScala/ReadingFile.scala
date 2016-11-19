package UsingScala

import scala.io.Source

/**
  * Created by root on 16-11-19.
  */
object ReadingFile {
  def main(args: Array[String]): Unit = {
    println("*** The content of the file you read is:")
    Source.fromFile("ReadingFile.scala").foreach(print)
  }
}
