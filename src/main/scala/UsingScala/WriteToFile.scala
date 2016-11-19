package UsingScala

import java.io.{File, PrintWriter}

/**
  * Created by root on 16-11-19.
  */
object WriteToFile {
  def main(args: Array[String]): Unit = {
    val writer=new PrintWriter(new File("sysbols.txt"))
    writer.write("AAPL")
    writer.close();
  }
}
