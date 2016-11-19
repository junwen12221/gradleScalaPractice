package UsingScala.StockPrices

/**
  * Created by root on 16-11-19.
  */
class FindTotalWorthSequential {
  def main(args: Array[String]): Unit = {
    val symbolsAndUnits=StockPriceFinder.getTickersAndUnits()
    println("Today is "+new java.util.Date())
    println("Ticker Units Closing Price($) Total Value($)")
    val startTime=System.nanoTime()
    val netWorth=(0.0 /: symbolsAndUnits){
      (worth,symbolsAndUnits)=>
        val
    }
  }

}
