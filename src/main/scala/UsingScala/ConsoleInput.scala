package UsingScala

import scala.io.StdIn

/**
  * Created by root on 16-11-19.
  */
object ConsoleInput {
  def main(args: Array[String]): Unit = {
    print("Please enter a ticker symbol:")
    val symbol = StdIn.readLine();
    println("OK,got it,you own " + symbol)
  }

}
