package UsingScala.StockPrices

import java.time.LocalDate

import scala.io.Source
import scala.xml.XML

/**
  * Created by root on 16-11-19.
  */
object StockPriceFinder {
def getLatestClosingPrice(symbol: String)={
  val url="http://ichart.finance.yahoo.com/table.csv?s=${symbol}&a=00&b=01&c="+LocalDate.now().getDayOfYear
  val data=Source.fromURL(url).mkString
  val mostRecentData=data.split("\n")(1)
  val closingPrice=mostRecentData.split(",")(4).toDouble
  closingPrice
}
  def getTickersAndUnits()={
    val stocksAndUnitsXML= XML.load("stpck.xml")
    (Map[String,Int]/:(stocksAndUnitsXML \ "symbol")){
      (map,symbolNode)=>
        val ticker=(symbolNode \ "@ticker").toString()
        val units=(symbolNode \ "units").text.toInt;
        
    }
  }
}
