package com.ansvia.manticore

import java.net.{MalformedURLException, URL}
import java.io.{BufferedReader, InputStreamReader, IOException}
import java.util.Date

/**
 * Author: robin
 * Date: 11/28/13
 * Time: 3:51 PM
 *
 */
class TrueFXMonitor(pairName:String) extends Thread {

    private val uri = "http://webrates.truefx.com/rates/connect.html?c=" + pairName + "&f=csv"
    private var done = false
    private var read = true

    private val prevTick = TickData(pairName, 0, 0, 0, 0, 0, 0)
    private val currentTick: TickData = TickData(pairName, 0, 0, 0, 0, 0, 0)

    lazy val trueFxEndpoint = {
//        try {
            new URL(uri)
//        }catch{
//            case e:MalformedURLException =>
//                e.printStackTrace()
//        }
    }

    def isRead = read

    def getCurrentTick = {
        read = true
        currentTick
    }

    def update() = {
        var in:BufferedReader = null

        try {

            in = new BufferedReader(new InputStreamReader(trueFxEndpoint.openStream()))

            val raw = in.readLine().split(",")

//            println(raw.toSeq)

            currentTick.timestamp = raw(1).trim.toLong
            currentTick.high = raw(6).trim.toDouble
            currentTick.low = raw(7).trim.toDouble
            currentTick.open = raw(8).trim.toDouble
            currentTick.buy = (raw(2) + raw(3)).toDouble
            currentTick.sell = (raw(4) + raw(5)).toDouble

            read = prevTick.high == currentTick.high && prevTick.low == currentTick.low &&
                prevTick.open == currentTick.open && prevTick.buy == currentTick.buy &&
                prevTick.sell == currentTick.sell

            prevTick.timestamp = currentTick.timestamp
            prevTick.high = currentTick.high
            prevTick.low = currentTick.low
            prevTick.open = currentTick.open
            prevTick.buy = currentTick.buy
            prevTick.sell = currentTick.sell

        }catch{
            case e:IOException =>
                e.printStackTrace()
        }finally{
            if (in != null)
                in.close()
        }
    }

    override def run(){

        println("TrueFXMonitor started.")

        while(!done){
            update()
            Thread.sleep(500)
        }

        println("TrueFXMonitor down.")
    }

    def shutdown(){
        done = true
    }

}

case class TickData(pairName:String, var timestamp:Long,
                    var high:Double, var low:Double, var open:Double,
                    var sell:Double, var buy:Double){
    override def toString: String = ("[%s]  %s   -   high: %f, " +
        "low: %f, open: %f, sell: %f, buy: %f").format(pairName, new Date(timestamp), high, low, open, sell, buy)
}

object TrueFXMonitorApp {
    def main(args: Array[String]) {
        val mon = new TrueFXMonitor("EUR/USD")
        mon.start()
        while(true){
            if (!mon.isRead)
                println(mon.getCurrentTick)
            Thread.sleep(1000)
        }
    }
}
