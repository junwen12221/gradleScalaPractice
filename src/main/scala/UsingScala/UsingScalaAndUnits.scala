package UsingScala

import scala.xml.XML

/**
  * Created by root on 16-11-19.
  */
object UsingScalaAndUnits {
  def main(args: Array[String]): Unit = {
    val stocksAndUnits = XML.load("stocks.xml")
    println(stocksAndUnits.getClass)
    println("Loaded file has " + (stocksAndUnits \\ "symbol").size + "symbol elements")
  }
}
