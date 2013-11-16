package com.ansvia.manticore

import java.io._
import java.util.NoSuchElementException
import java.security.InvalidParameterException
import scala.collection.mutable.ArrayBuffer

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

    def direction = {
        if (close > open) 1 else 0
    }

}



class CsvReader(is:InputStream, untilDate:String) {

    private var idx = 0
    lazy val bfr = new BufferedReader(new InputStreamReader(is))

    def this(file:File, untilDate:String){
        this(new FileInputStream(file), untilDate)
    }

    def this(file:String, untilDate:String="-"){
        this(new File(file), untilDate)
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

        if (s(0) + " " + s(1) == untilDate)
            throw new NoSuchElementException()

        val rv = {
            if (s.length == 6)
                Record(idx, s(0), s(1).toDouble, s(2).toDouble, s(3).toDouble, s(4).toDouble, s(5).toDouble)
            else if (s.length == 7)
                Record(idx, s(0) + " " + s(1), s(2).toDouble, s(3).toDouble,
                    s(4).toDouble, s(5).toDouble, s(6).toDouble)
            else
                throw new InvalidParameterException("Invalid csv format for column " + s.length)
        }
        idx = idx + 1
        rv
    }

    def toArray = {
        var buff = new ArrayBuffer[Record]

        try {
            while (true) {
                buff :+= nextRecord()
            }
        }
        catch {
            case e:NoSuchElementException =>

        }
        buff.toArray
    }

    def close(){
        bfr.close()
    }


}
