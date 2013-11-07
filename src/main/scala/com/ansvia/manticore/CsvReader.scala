package com.ansvia.manticore

import java.io._
import java.util.NoSuchElementException

/**
 * Author: robin
 * Date: 11/7/13
 * Time: 11:39 PM
 *
 */


case class Record(index:Int, time:String, open:Double, high:Double, low:Double, close:Double, volumes:Double){
    override def hashCode() = {
        "%s%s%s%s%s%s%s".format(index,time,open,high,low,close,volumes).hashCode
    }

}



class CsvReader(is:InputStream) {

    private var idx = 0
    lazy val bfr = new BufferedReader(new InputStreamReader(is))

    def this(file:File){
        this(new FileInputStream(file))
    }

    def this(file:String){
        this(new File(file))
    }


    def nextRecord():Record = {
        var line = bfr.readLine()

        if (line == null || line == "")
            throw new NoSuchElementException()

        if (line.toLowerCase.startsWith("time"))
            line = bfr.readLine()

        if (line == null || line == "")
            throw new NoSuchElementException()

        val s = line.split(",")
        val rv = Record(idx, s(0), s(1).toDouble, s(2).toDouble, s(3).toDouble, s(4).toDouble, s(5).toDouble)
        idx = idx + 1
        rv
    }


}
